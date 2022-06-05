FROM debian:bullseye-slim AS ghc-and-cabal

ENV LANG=C.UTF-8
ENV DEBIAN_FRONTEND=noninteractive
ENV PATH=/root/.ghcup/bin:$PATH 

RUN apt-get update \
  && apt-get install -y binutils build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 libnuma-dev zlib1g-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*


ARG GHCUP_VERSION=0.1.17.8
RUN curl --proto '=https' --tlsv1.2 -sSf https://downloads.haskell.org/~ghcup/${GHCUP_VERSION}/x86_64-linux-ghcup-${GHCUP_VERSION} --output /usr/local/bin/ghcup \
  && chmod +x /usr/local/bin/ghcup

ARG GHC_VERSION=8.10.7
RUN ghcup install ghc --set ${GHC_VERSION}

ARG CABAL_VERSION=3.6.2.0
RUN ghcup install cabal --set ${CABAL_VERSION}


FROM ghc-and-cabal AS build

RUN apt-get update \
  && apt-get install -y libpq-dev liblzma-dev

WORKDIR /build

COPY cabal.project cabal.project.freeze boilerplate.cabal /build/
RUN cabal update
RUN cabal build -O2 --only-dependencies lib:boilerplate

COPY . /build/
RUN cabal install -O2 --installdir bin --install-method copy exe:boilerplate
RUN strip -s /build/bin/boilerplate


FROM debian:bullseye-slim AS final

ENV LANG=C.UTF-9
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
  && apt-get install -y libpq-dev liblzma-dev libnuma-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY --from=build /build/bin/boilerplate /usr/local/bin/boilerplate

USER 999:1000

ENTRYPOINT [ "/usr/local/bin/boilerplate" ]
