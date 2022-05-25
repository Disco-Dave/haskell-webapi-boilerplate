FROM rust:1.61-slim-bullseye AS builder

WORKDIR /build

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
  && apt-get install -y binutils pkg-config libssl-dev

ARG SQLX_VERSION="0.5.13"
RUN cargo install sqlx-cli --version "${SQLX_VERSION}" --no-default-features --features native-tls,postgres --root . \
  && strip -s /build/bin/sqlx


FROM debian:bullseye-slim AS final

WORKDIR /usr/local/etc/sqlx

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
  && apt-get install -y libpq-dev libssl-dev postgresql-client-13 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY entrypoint.sh entrypoint.sh
COPY --from=builder /build/bin/sqlx /usr/local/bin/sqlx

USER 1000:1000

ENTRYPOINT [ "/usr/local/etc/sqlx/entrypoint.sh" , "/usr/local/bin/sqlx" ]
