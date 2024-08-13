# Use uma imagem base com Dart SDK
FROM dart:stable AS build

# Defina o diretório de trabalho dentro do container
WORKDIR /app

# Copie os arquivos de código-fonte para o diretório de trabalho
COPY . .

# Compile o código Dart para um executável nativo
RUN dart pub get
RUN dart compile exe server.dart -o bin/server

# Use uma imagem mais leve para a fase de produção
FROM debian:buster-slim

# Instale as dependências necessárias
RUN apt-get update && apt-get install -y \
    ca-certificates \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Defina o diretório de trabalho
WORKDIR /app

# Copie o executável compilado da fase de build
COPY --from=build /app/bin/server /app/server

# Exponha a porta em que o servidor WebSocket irá rodar
EXPOSE 3016

# Comando para iniciar o servidor
CMD ["./server"]
