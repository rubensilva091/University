# Self-Hosted Runner

Build the image:

```bash
docker build -t gh-self-hosted-runner .
```

Run the container with a registration token from GitHub:

```bash
docker run --rm -it \
  -e REPO_URL="https://github.com/ORG/REPO" \
  -e RUNNER_TOKEN="TOKEN_AQUI" \
  -e RUNNER_NAME="week6-runner" \
  gh-self-hosted-runner
```

Required environment variables:

- `REPO_URL`: repository URL, for example `https://github.com/org/repo`
- `RUNNER_TOKEN`: self-hosted runner registration token

Optional environment variables:

- `RUNNER_NAME`: runner name shown in GitHub
- `RUNNER_WORKDIR`: working directory used by the runner
- `RUNNER_LABELS`: runner labels, default `self-hosted,linux,X64`