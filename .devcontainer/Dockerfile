# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.187.0/containers/javascript-node/.devcontainer/base.Dockerfile

# [Choice] Node.js version: 16, 14, 12
ARG VARIANT="16-buster"
FROM mcr.microsoft.com/vscode/devcontainers/javascript-node:0-${VARIANT}

# Install basic packages useful for development 
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends rsync git m4 build-essential patch unzip wget pkg-config libgmp-dev libev-dev libhidapi-dev libffi-dev opam jq zlib1g-dev inotify-tools tmuxinator

# [Optional] Uncomment if you want to install an additional version of node using nvm
# ARG EXTRA_NODE_VERSION=10
# RUN su node -c "source /usr/local/share/nvm/nvm.sh && nvm install ${EXTRA_NODE_VERSION}"

# Install esy
RUN su node -c "sudo npm install -g --unsafe-perm @esy-nightly/esy"

# Install LIGO
COPY tools/* /tmp/tools/
RUN chmod +x /tmp/tools/ligo && sudo cp /tmp/tools/ligo /usr/local/bin

# Download mainnet docker script (commented out because it requires docker-in-docker)
# RUN wget -O mainnet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh && chmod +x mainnet.sh

