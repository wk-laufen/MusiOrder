FROM rust:1.81.0-bookworm AS builder
WORKDIR /app
RUN apt update && apt install -y libpcsclite-dev
COPY . .
RUN cargo install --path .

FROM debian:bookworm-slim AS final
RUN apt update && apt install -y libpcsclite1 && rm -rf /var/lib/apt/lists/*
COPY --from=builder /usr/local/cargo/bin/nfc-reader /usr/local/bin/nfc-reader
ENV ROCKET_ADDRESS=0.0.0.0
EXPOSE 8000
CMD ["nfc-reader"]
