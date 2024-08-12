FROM denoland/deno:latest

WORKDIR /app

COPY . .

ENV PORT=3003
ENV CLIENTS_COUNT=32

EXPOSE 3003

CMD ["deno", "run", "--allow-net", "--allow-env", "server.mjs"]
