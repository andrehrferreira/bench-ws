FROM php:8.1-cli-alpine

RUN apk add --no-cache libstdc++ libssl1.1

RUN pecl install swoole && docker-php-ext-enable swoole

WORKDIR /app

COPY . .

EXPOSE 3013

CMD ["php", "/app/server.php"]
