import uuid
import secrets
from datetime import datetime, timezone, timedelta
from sqlalchemy import Column, String, Integer, DateTime, ForeignKey, Table
from sqlalchemy.orm import relationship
from .database import Base 

file_shares_table = Table(
    "file_shares",
    Base.metadata,
    Column("file_id", String, ForeignKey("file_metadata.file_id", ondelete="CASCADE"), primary_key=True),
    Column("shared_user_id", String, ForeignKey("shared_users.user_id", ondelete="CASCADE"), primary_key=True)
)

class FileMetadata(Base):
    __tablename__ = "file_metadata"

    file_id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()), index=True)
    owner = Column(String, index=True, nullable=False)
    permissions = Column(String, default="read/write")

    versions = Column(Integer, default=1)
    created_at = Column(DateTime, default=lambda: datetime.now(timezone.utc))
    expires_at = Column(DateTime, nullable=True)
    updated_at = Column(
        DateTime, 
        default=lambda: datetime.now(timezone.utc), 
        onupdate=lambda: datetime.now(timezone.utc)
    )

    shared_with = relationship(
        "FileShareRecord", 
        secondary=file_shares_table, 
        backref="shared_files"
    )
    
    anonymous_links = relationship(
        "AnonymousLink", 
        backref="file_metadata", 
        cascade="all, delete-orphan"
    )

class FileShareRecord(Base):
    __tablename__ = "shared_users"

    user_id = Column(String, primary_key=True)


class AnonymousLink(Base):
    __tablename__ = "anonymous_links"
    
    link_id = Column(String, primary_key=True, default=lambda: secrets.token_urlsafe(32), index=True)
    
    file_id = Column(String, ForeignKey("file_metadata.file_id", ondelete="CASCADE"), nullable=False)
    
    permissions = Column(String, default="read")

    created_at = Column(DateTime, default=lambda: datetime.now(timezone.utc))
    expires_at = Column(DateTime, default=lambda: datetime.now(timezone.utc) + timedelta(days=7))