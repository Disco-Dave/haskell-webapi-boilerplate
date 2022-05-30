FROM debian:bullseye-slim AS builder-base

ENV LANG=C.UTF-8
ENV DEBIAN_FRONTEND=noninteractive
ENV PATH=/root/.ghcup/bin:$PATH 

RUN apt-get update \
  && apt-get install -y binutils build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 libnuma-dev zlib1g-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

ARG GHC_VERSION=9.0.2
ARG GHCUP_VERSION=0.1.17.8
ARG STACK_VERSION=2.7.5

RUN curl --proto '=https' --tlsv1.2 -sSf https://downloads.haskell.org/~ghcup/${GHCUP_VERSION}/x86_64-linux-ghcup-${GHCUP_VERSION} --output /usr/local/bin/ghcup \
  && chmod +x /usr/local/bin/ghcup

RUN ghcup install ghc --set ${GHC_VERSION}
RUN ghcup install stack --set ${STACK_VERSION}


FROM builder-base AS builder

RUN apt-get update \
  && apt-get install -y libpq-dev liblzma-dev

WORKDIR /build

COPY ./stack.yaml ./stack.yaml.lock ./package.yaml /build/
RUN stack build --system-ghc --no-install-ghc --only-dependencies --ghc-options -O2

COPY . /build/
RUN stack install --system-ghc --no-install-ghc --local-bin-path bin --ghc-options -O2
RUN strip -s /build/bin/boilerplate


FROM debian:bullseye-slim AS final

ENV LANG=C.UTF-8
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
  && apt-get install -y libpq-dev liblzma-dev libnuma-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/bin/boilerplate /usr/local/bin/boilerplate

USER 1000:1000

ENTRYPOINT [ "/usr/local/bin/boilerplate" ]
