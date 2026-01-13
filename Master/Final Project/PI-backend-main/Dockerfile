FROM golang:latest AS build

ARG COMMIT="000000"
ARG VERSION="0.0.0"

WORKDIR /app
COPY . .

RUN set -eux; \
  go build -ldflags "-s -w -X main.commit=$COMMIT -X main.version=$VERSION" ./cmd/app; \
  go install -tags postgres github.com/golang-migrate/migrate/v4/cmd/migrate@latest

FROM debian:bookworm-slim

WORKDIR /app

RUN set -eux; \
    apt-get update -y; \
    apt-get install --no-install-recommends -y \
    gosu \
    ca-certificates; \
    apt-get -y autoremove; \
    rm -rf /var/cache/apt /var/lib/apt/* /var/cache/debconf/* /var/lib/apt/lists/*; \
    groupmod -g 911 users; \
    useradd -u 911 -U -d /app -s /bin/false runner; \
    usermod -G users runner; \
    mkdir /app/postgres

COPY --from=build /app/app /app/npadmin
COPY --from=build /app/postgres/migrations /app/postgres/migrations
COPY --from=build /app/mailer/header/logo.png /app/mailer/header/logo.png
COPY --from=build /go/bin/migrate /app/migrate
COPY scripts/container-entrypoint.sh /app/container-entrypoint.sh
COPY scripts/container-cmd.sh /app/container-cmd.sh

ENTRYPOINT ["/app/container-entrypoint.sh"]
CMD ["/app/container-cmd.sh"]
