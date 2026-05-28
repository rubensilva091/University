FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    jq \
    git \
    sudo \
    libicu70 \
    libkrb5-3 \
    zlib1g \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -m runner && echo "runner ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

WORKDIR /home/runner

ARG RUNNER_VERSION=2.328.0
RUN curl -fsSL -o actions-runner.tar.gz \
    "https://github.com/actions/runner/releases/download/v${RUNNER_VERSION}/actions-runner-linux-x64-${RUNNER_VERSION}.tar.gz" \
    && mkdir actions-runner \
    && tar xzf actions-runner.tar.gz -C actions-runner \
    && rm actions-runner.tar.gz \
    && chown -R runner:runner /home/runner/actions-runner

COPY start-runner.sh /home/runner/start-runner.sh
RUN chmod +x /home/runner/start-runner.sh && chown runner:runner /home/runner/start-runner.sh

USER runner
WORKDIR /home/runner/actions-runner

ENTRYPOINT ["/home/runner/start-runner.sh"]