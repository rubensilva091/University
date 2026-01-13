#!/bin/bash

set -eux

DSN=$(sed -nr 's/dsn="(.*)"/\1/p' /vault/npadmin.toml | tr -d [:space:])

./migrate -path=./postgres/migrations -database=$DSN up

./npadmin -config /vault/npadmin.toml
