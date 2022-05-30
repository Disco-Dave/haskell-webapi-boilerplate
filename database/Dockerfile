FROM rust:1.61-slim-bullseye AS builder

WORKDIR /build

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8

RUN apt-get update \
  && apt-get install -y binutils pkg-config libssl-dev

ARG sqlx_version="0.5.13"
RUN cargo install sqlx-cli --version "${sqlx_version}" --no-default-features --features native-tls,postgres --root . \
  && strip -s /build/bin/sqlx


FROM debian:bullseye-slim AS final

WORKDIR /usr/local/etc/sqlx

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8

RUN apt-get update \
  && apt-get install -y libpq-dev libssl-dev postgresql-client-13 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/bin/sqlx /usr/local/bin/sqlx

USER 1000:1000
