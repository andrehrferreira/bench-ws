# Use uma imagem oficial do PHP com Swoole pré-instalado
FROM php:8.1-cli-alpine

# Instale dependências do sistema necessárias, incluindo ferramentas de construção e pkg-config
RUN apk add --no-cache libstdc++ libssl3 \
    && apk add --no-cache --virtual .build-deps autoconf g++ make pkgconfig brotli-dev

# Instale o Swoole
RUN pecl install swoole && docker-php-ext-enable swoole

# Remova as dependências de construção para manter a imagem leve
RUN apk del .build-deps

# Defina o diretório de trabalho dentro do container
WORKDIR /app

# Copie o código-fonte da aplicação para o diretório de trabalho
COPY . .

# Exponha a porta em que o servidor WebSocket irá rodar
EXPOSE 3013

# Comando para iniciar o servidor WebSocket com PHP
CMD ["php", "/app/server.php"]
