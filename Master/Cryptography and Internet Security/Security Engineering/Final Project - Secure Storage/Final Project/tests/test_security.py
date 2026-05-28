from datetime import datetime, timedelta, timezone

import pytest
from fastapi import HTTPException, Request, status
from fastapi.testclient import TestClient

from server import main as main_module, models
from server.auth import get_current_user
from server.database import engine
from server.main import app


def _upload_file(client: TestClient, username: str, payload: bytes = b"encrypted-bytes") -> str:
    files = {"file": ("encrypted_blob.bin", payload, "application/octet-stream")}
    response = client.post("/files/", files=files, headers={"X-Test-User": username})
    assert response.status_code == 200
    return response.json()["file_id"]


@pytest.fixture()
def client(tmp_path):
    storage_dir = tmp_path / "storage"
    storage_dir.mkdir(parents=True, exist_ok=True)

    # Isolate file writes for tests.
    main_module.STORAGE_DIR = str(storage_dir)

    # Avoid test flakiness from rate limiting during test loops.
    main_module.MAX_REQUESTS = 10_000
    main_module._rate_store.clear()

    # Start each test from a clean schema.
    models.Base.metadata.drop_all(bind=engine)
    models.Base.metadata.create_all(bind=engine)

    def fake_current_user(request: Request) -> str:
        user = request.headers.get("X-Test-User")
        if not user:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Missing test identity",
            )
        return user

    app.dependency_overrides[get_current_user] = fake_current_user

    with TestClient(app) as c:
        yield c

    app.dependency_overrides.clear()


def test_tenant_isolation_blocks_cross_tenant_access(client: TestClient):
    file_id = _upload_file(client, "alice", b"alice-secret")

    list_bob = client.get("/files/", headers={"X-Test-User": "bob"})
    assert list_bob.status_code == 200
    body = list_bob.json()
    assert body["tenant_id"] == "bob"
    assert all(item["file_id"] != file_id for item in body["owned_files"])
    assert all(item["file_id"] != file_id for item in body["shared_with_me"])

    blocked_download = client.get(f"/files/{file_id}/download", headers={"X-Test-User": "bob"})
    # Hardened behavior: avoid leaking file existence across tenants.
    assert blocked_download.status_code == 404


def test_permission_bypass_read_only_shared_user_cannot_update(client: TestClient):
    file_id = _upload_file(client, "alice")

    share_response = client.post(
        f"/files/{file_id}/share",
        json={"target_user_id": "bob"},
        headers={"X-Test-User": "alice"},
    )
    assert share_response.status_code == 200

    set_read_only = client.put(
        f"/files/{file_id}/permissions",
        json={"permissions": "read"},
        headers={"X-Test-User": "alice"},
    )
    assert set_read_only.status_code == 200

    update_attempt = client.put(
        f"/files/{file_id}",
        files={"file": ("encrypted_blob.bin", b"new-content", "application/octet-stream")},
        headers={"X-Test-User": "bob"},
    )
    assert update_attempt.status_code == 403


def test_api_misuse_attempts_are_rejected(client: TestClient):
    missing_identity = client.get("/files/")
    assert missing_identity.status_code == 401

    file_id = _upload_file(client, "alice")

    invalid_permission = client.put(
        f"/files/{file_id}/permissions",
        json={"permissions": "admin"},
        headers={"X-Test-User": "alice"},
    )
    assert invalid_permission.status_code == 400

    self_share = client.post(
        f"/files/{file_id}/share",
        json={"target_user_id": "alice"},
        headers={"X-Test-User": "alice"},
    )
    assert self_share.status_code == 400


def test_expired_anonymous_link_returns_410(client: TestClient):
    file_id = _upload_file(client, "alice")

    create_link = client.post(
        f"/files/{file_id}/anonymous-link",
        headers={"X-Test-User": "alice"},
    )
    assert create_link.status_code == 200
    link_id = create_link.json()["link_id"]

    with main_module.SessionLocal() as db:
        link = db.query(models.AnonymousLink).filter(models.AnonymousLink.link_id == link_id).first()
        link.expires_at = datetime.now(timezone.utc) - timedelta(seconds=1)
        db.commit()

    expired_access = client.get(f"/anon/{link_id}")
    assert expired_access.status_code == 410


def test_file_ttl_blocks_download_after_expiration(client: TestClient):
    file_id = _upload_file(client, "alice", b"ttl-protected")

    set_ttl = client.put(
        f"/files/{file_id}/ttl",
        json={"expires_in_days": 1},
        headers={"X-Test-User": "alice"},
    )
    assert set_ttl.status_code == 200

    with main_module.SessionLocal() as db:
        file_record = db.query(models.FileMetadata).filter(models.FileMetadata.file_id == file_id).first()
        file_record.expires_at = datetime.now(timezone.utc) - timedelta(seconds=1)
        db.commit()

    expired_download = client.get(f"/files/{file_id}/download", headers={"X-Test-User": "alice"})
    assert expired_download.status_code == 410


def test_file_ttl_rejects_invalid_days(client: TestClient):
    file_id = _upload_file(client, "alice", b"ttl-invalid")

    invalid_ttl = client.put(
        f"/files/{file_id}/ttl",
        json={"expires_in_days": 0},
        headers={"X-Test-User": "alice"},
    )
    assert invalid_ttl.status_code == 400


def test_upload_rejects_payload_above_max_size(client: TestClient):
    previous_max = main_module.MAX_UPLOAD_BYTES
    main_module.MAX_UPLOAD_BYTES = 8
    try:
        response = client.post(
            "/files/",
            files={"file": ("encrypted_blob.bin", b"0123456789", "application/octet-stream")},
            headers={"X-Test-User": "alice"},
        )
        assert response.status_code == 413
    finally:
        main_module.MAX_UPLOAD_BYTES = previous_max


def test_missing_disk_file_triggers_orphan_metadata_cleanup(client: TestClient):
    file_id = _upload_file(client, "alice", b"orphan-test")

    # Simulate external/manual storage deletion to create an orphan metadata entry.
    file_path = main_module.os.path.join(main_module.STORAGE_DIR, file_id)
    main_module.os.remove(file_path)

    missing_download = client.get(f"/files/{file_id}/download", headers={"X-Test-User": "alice"})
    assert missing_download.status_code == 410

    with main_module.SessionLocal() as db:
        still_exists = db.query(models.FileMetadata).filter(models.FileMetadata.file_id == file_id).first()
        assert still_exists is None


def test_rate_limit_uses_forwarded_ip_header(client: TestClient):
    previous_max = main_module.MAX_REQUESTS
    main_module.MAX_REQUESTS = 1
    main_module._rate_store.clear()
    try:
        first = client.get("/", headers={"X-Forwarded-For": "198.51.100.20"})
        assert first.status_code == 200

        second_same_ip = client.get("/", headers={"X-Forwarded-For": "198.51.100.20"})
        assert second_same_ip.status_code == 429

        third_other_ip = client.get("/", headers={"X-Forwarded-For": "198.51.100.21"})
        assert third_other_ip.status_code == 200
    finally:
        main_module.MAX_REQUESTS = previous_max
        main_module._rate_store.clear()


def test_oversized_upload_does_not_persist_metadata(client: TestClient):
    previous_max = main_module.MAX_UPLOAD_BYTES
    main_module.MAX_UPLOAD_BYTES = 8
    try:
        response = client.post(
            "/files/",
            files={"file": ("encrypted_blob.bin", b"0123456789", "application/octet-stream")},
            headers={"X-Test-User": "alice"},
        )
        assert response.status_code == 413

        with main_module.SessionLocal() as db:
            all_files = db.query(models.FileMetadata).all()
            assert len(all_files) == 0
    finally:
        main_module.MAX_UPLOAD_BYTES = previous_max


def test_shared_user_cannot_delete_owner_file(client: TestClient):
    file_id = _upload_file(client, "alice", b"owner-delete-protection")

    share_response = client.post(
        f"/files/{file_id}/share",
        json={"target_user_id": "bob"},
        headers={"X-Test-User": "alice"},
    )
    assert share_response.status_code == 200

    delete_as_shared = client.delete(f"/files/{file_id}", headers={"X-Test-User": "bob"})
    assert delete_as_shared.status_code == 404


def test_shared_user_with_write_can_update_file(client: TestClient):
    file_id = _upload_file(client, "alice", b"v1")

    share_response = client.post(
        f"/files/{file_id}/share",
        json={"target_user_id": "bob"},
        headers={"X-Test-User": "alice"},
    )
    assert share_response.status_code == 200

    set_write = client.put(
        f"/files/{file_id}/permissions",
        json={"permissions": "read/write"},
        headers={"X-Test-User": "alice"},
    )
    assert set_write.status_code == 200

    update_as_shared = client.put(
        f"/files/{file_id}",
        files={"file": ("encrypted_blob.bin", b"v2", "application/octet-stream")},
        headers={"X-Test-User": "bob"},
    )
    assert update_as_shared.status_code == 200


def test_ttl_update_is_owner_only(client: TestClient):
    file_id = _upload_file(client, "alice", b"ttl-owner-only")

    share_response = client.post(
        f"/files/{file_id}/share",
        json={"target_user_id": "bob"},
        headers={"X-Test-User": "alice"},
    )
    assert share_response.status_code == 200

    ttl_as_shared = client.put(
        f"/files/{file_id}/ttl",
        json={"expires_in_days": 3},
        headers={"X-Test-User": "bob"},
    )
    assert ttl_as_shared.status_code == 404
