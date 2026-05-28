FROM python:3.12-slim

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

ARG UID=1000
ARG GID=1000

WORKDIR /app

RUN addgroup --system --gid ${GID} appgroup && adduser --system --uid ${UID} --ingroup appgroup appuser

COPY server/requirements.txt /app/server/requirements.txt
RUN python -m pip install --upgrade pip && \
    pip install --no-cache-dir -r /app/server/requirements.txt

COPY server /app/server

RUN mkdir -p /app/data/storage && chown -R appuser:appgroup /app

USER appuser

EXPOSE 8000

ENV DATABASE_URL=sqlite:////app/data/filestorage.db \
    STORAGE_DIR=/app/data/storage

CMD ["uvicorn", "server.main:app", "--host", "0.0.0.0", "--port", "8000"]
