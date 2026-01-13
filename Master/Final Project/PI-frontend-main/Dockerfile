ARG CFG
FROM node:lts-slim AS builder

WORKDIR /app
COPY . .

RUN --mount=type=secret,id=env_file cat /run/secrets/env_file > ./.env

RUN set -eux; \
  npm ci; \
  npm run build; \
  npm prune --omit=dev

FROM debian:bookworm-slim AS runner

ENV NODE_ENV production

WORKDIR /app

RUN set -eux; \
  apt-get update -y; \
  apt-get install -y --no-install-recommends gosu caddy; \
  apt-get -y autoremove; \
  rm -rf /var/cache/apt /var/lib/apt/* /var/cache/debconf/* /var/lib/apt/lists/*; \
  groupmod -g 911 users; \
  useradd -u 911 -U -d /app -s /bin/false runner; \
  usermod -G users runner; \
  chown -R runner:users /app

COPY --from=builder --chown=runner:users /app/build /app
COPY --from=builder --chown=runner:users /app/.env /app
COPY scripts/container-entrypoint.sh /container-entrypoint.sh
COPY scripts/Caddyfile /Caddyfile

ENTRYPOINT ["/container-entrypoint.sh"]
CMD ["caddy", "run", "--config", "/Caddyfile"]

