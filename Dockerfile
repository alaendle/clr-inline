FROM ubuntu:bionic as build

# install stack
RUN apt-get update && \
    apt-get install -y wget && \
    wget -qO- https://get.haskellstack.org/ | sh

# Prerequisites
RUN wget -q https://packages.microsoft.com/config/ubuntu/18.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb && \
    dpkg -i packages-microsoft-prod.deb && \
    apt-get update && \
    apt-get install -y --no-install-recommends apt-transport-https && \
    apt-get update && \
    apt-get install -y --no-install-recommends dotnet-sdk-3.1 && \
    rm -rf /var/lib/apt/lists/*

# CACHE!
RUN stack update  --resolver lts-14.22
RUN stack setup  --resolver lts-14.22
COPY stack.yaml .
COPY libs/clr-typed/*.cabal libs/clr-typed/
COPY libs/clr-host/*.cabal libs/clr-host/
COPY libs/clr-bindings/*.cabal libs/clr-bindings/
COPY libs/clr-import-gen/*.cabal libs/clr-import-gen/
COPY libs/clr-inline/*.cabal libs/clr-inline/
COPY libs/clr-marshal/*.cabal libs/clr-marshal/
COPY examples/clr-inline-demo/*.cabal examples/clr-inline-demo/
COPY utils/clr-win-linker/*.cabal utils/clr-win-linker/
RUN stack build --copy-bins --only-dependencies

# Build (& maybe later test -- then also cache --test dependencies)
COPY . .
RUN stack clean
RUN stack build
RUN stack exec winforms