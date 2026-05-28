Docker deployment instructions

1. Set required environment variable for JWT audience validation:

	export GOOGLE_CLIENT_ID="<google_client_id>"

2. Build the image:

	docker build -f deploy/Dockerfile -t secure-file-storage-api:latest .

3. Run the API container:

	docker rm -f secure-file-storage-api >/dev/null 2>&1 || true
	docker run -d --name secure-file-storage-api \
	  -p 8000:8000 \
	  -e GOOGLE_CLIENT_ID="$GOOGLE_CLIENT_ID" \
	  -e DATABASE_URL=sqlite:////app/data/filestorage.db \
	  -e STORAGE_DIR=/app/data/storage \
	  -v "$PWD/data:/app/data" \
	  secure-file-storage-api:latest

4. API is available at:

	http://127.0.0.1:8000

5. Stop containers:

	docker stop secure-file-storage-api