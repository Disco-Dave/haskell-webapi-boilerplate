FROM haskell:8.10.7-buster AS builder

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
  && apt-get install -y binutils libpq-dev liblzma-dev

WORKDIR /build

COPY ./stack.yaml ./stack.yaml.lock ./package.yaml /build/
RUN stack build --system-ghc --no-install-ghc --only-dependencies --ghc-options -O2

COPY . /build/
RUN stack install --system-ghc --no-install-ghc --local-bin-path bin --ghc-options -O2 \
  && strip -s /build/bin/boilerplate


FROM debian:buster-slim AS final

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
  && apt-get install -y libpq-dev liblzma-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/bin/boilerplate /usr/local/bin/boilerplate

USER 1000:1000

ENTRYPOINT [ "/usr/local/bin/boilerplate" ]
