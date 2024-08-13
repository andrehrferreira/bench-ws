FROM busybox AS base

LABEL author="ziglings.org"
LABEL description="Minimal docker container for using the Zig toolchain for automated testing."

ENV ZIG_VERSION=0.12.0
ENV ZIG_PATH=/zig/${ZIG_VERSION}/files

RUN wget -q https://github.com/marler8997/zigup/releases/download/v2024_03_13/zigup.ubuntu-latest-x86_64.zip && \
    unzip zigup.ubuntu-latest-x86_64.zip -d /usr/bin \
    && chmod +x /usr/bin/zigup \
    && zigup $ZIG_VERSION --install-dir /zig \
    && chmod -R a+w ${ZIG_PATH} \
    && rm zigup.ubuntu-latest-x86_64.zip /usr/bin/zigup

FROM busybox AS build

COPY --from=base /zig/0.12.0/files/lib /lib
COPY --from=base /usr/bin/zig /bin/zig

# Defina o diretório de trabalho dentro do container
WORKDIR /app

# Copie os arquivos de código-fonte para o diretório de trabalho
COPY . .

# Compile a biblioteca e o executável Zig
RUN zig build -Drelease

# Use uma imagem mais leve para a fase de produção
FROM debian:buster-slim

# Copie o executável compilado da fase de build
COPY --from=build /app/zig-out/bin/server-zig /usr/local/bin/server-zig

# Exponha a porta em que o servidor WebSocket irá rodar
EXPOSE 3008

# Comando para iniciar o servidor
CMD ["server-zig"]
