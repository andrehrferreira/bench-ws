FROM php:8.1-cli-alpine

RUN apk add --no-cache libstdc++ libssl3 \
    && apk add --no-cache --virtual .build-deps autoconf g++ make pkgconfig brotli-dev

RUN pecl install swoole && docker-php-ext-enable swoole

RUN apk del .build-deps

WORKDIR /app

COPY . .

EXPOSE 3013

CMD ["php", "/app/server.php"]
