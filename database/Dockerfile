FROM rust:1.59-slim-buster AS builder

WORKDIR /build

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
  && apt-get install -y binutils pkg-config libssl-dev

RUN cargo install sqlx-cli --version 0.5.11 --no-default-features --features native-tls,postgres --root . \
  && strip -s /build/bin/sqlx


FROM debian:buster-slim AS final

WORKDIR /usr/local/etc/sqlx

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
  && apt-get install -y libpq-dev libssl-dev postgresql-client-11 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY entrypoint.sh entrypoint.sh
COPY --from=builder /build/bin/sqlx /usr/local/bin/sqlx

USER 1000:1000

ENTRYPOINT [ "/usr/local/etc/sqlx/entrypoint.sh" , "/usr/local/bin/sqlx" ]
