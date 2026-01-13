# NonProfit-Administration Backend API

This repo is NonProfit-Administration project backend written in go.

## Content

As it is, it can:

-   create a http/https local server
-   manage and access a postgres database with tables of users/accounts information, authentication and authorization, users profiles and notifications.
-   validate input values (from forms usually) and validate email uniqueness in the database accounts table.
-   jwt, password hashing and token generation.

### Endpoints

-   /api/v1/healthcheck Get: API healthcheck
-   /api/v1/account POST : account creation. 
-   /api/v1/account/confirm GET : account confirmation/verification (email send).
-   /api/v1/account/resend-confirmation POST : resend confirmation email with new token.
-   /api/v1/account/reset-password Post: reset password email sent.
-   /api/v1/account/reset-password Get: validation of token for password reset.
-   /api/v1/account/reset-password Put: new password added.
-   /api/v1/account/update-email Post: initiate account email change.
-   /api/v1/account/update-email Put: change email of account.
-   /api/v1/account/resend-email Post: resend email of email change.
-   /api/v1/login Post: password login, creates JWT.
-   /api/v1/refresh Get: creates new refresh token.

## Create Database 
- Install [Postgres](https://www.postgresql.org) locally.
- First, you need to create the PostgreSQL database where the backend will store its data.
- To do so, run the following command:
  ```
  psql postgres -c 'CREATE DATABASE "npadmin_development";'
  ```

## Migrations

Migrations are managed by [go-migrate](https://github.com/golang-migrate/migrate#cli-usage)

**Run Migrations**

`migrate -path=./postgres/migrations -database="postgres://user@localhost/npadmin_development?sslmode=disable" up`

-   `$DSN` should contain your database data source string

### Create an email sandbox

- To send emails, such as account confirmation requests, an email service must be configured.
- We recommend creating an account on [Mailtrap](https://mailtrap.io).

- After logging in, navigate to your inbox to find the credentials, including the `username` and `password`.
- Paste your Mailtrap credentials, username and password, into the respective fields inside the mailer section.

## Run
before running:
- Double-check that the dsn field under the database section references the name of the database you created earlier.

If all was set up correctly you should be able to run the server

```
go run cmd/app/main.go
```

And by visiting http://localhost:4000/api/v1/healthcheck you should receive a success json.

## Configuration variables

A configuration file example (etc/project.config):

```
    env="development"

    [logger]
        level="DEBUG"
        path="logs/npadmin.log"

    [http]
        address="127.0.0.1"
        fqdn="localhost"
        listen-port=4000
        port=4000
        read-timeout=15
        write-timeout=15
        shutdown-timeout=10
        jwt-private="./tests/private-auth.pem"
        jwt-public="./tests/public-auth.pem"
        jwt-refresh-private="./tests/private-refresh.pem"
        jwt-refresh-public="./tests/public-refresh.pem"
        tls=false

    [database]
        dsn="postgres://user:user@localhost:5432/<db-name>?sslmode=disable"
        max-open-connections=25
        max-idle-connections=25
        max-idle-time="15m"

    [mailer]
        host="smtp.example.com"
        port=587
        username="1234"
        password="1234"
        sender="Example <no-reply@example.com>"

    [payment]
        multibanco-key="XXX-123456"
        multibanco-url="https://ifthenpay.com/api/multibanco/reference/sandbox"
        entidade=12345
        subentidade=123
        mbway-key="XXX-123456"
        mbway-url="https://mbway.ifthenpay.com/ifthenpaymbw.asmx"
        anti-phishing-key="123456asdfg"

    [webapp]
        admin-dashboard-url="http://localhost:3000/"
        associate-dashboard-url="http://localhost:3000/"
        scan-associate-url="http://localhost:3000/scan?id="
        account-success-page-url="http://localhost:3000/blank"
        account-error-page-url="http://localhost:3000/blank?status=400"
        login-error-page-url="http://localhost:3000/blank?status=loginfailed"

    [pusher]
        app-id="1234567"
        key="1234asdfg"
        secret="asdf1234"
        cluster="eu"

    [admin]
        email="administrator@email.com"
        nif="111111111"
```
