# Use uma imagem base com Ruby
FROM ruby:3.1

# Instale as dependências necessárias
RUN gem install eventmachine em-websocket json

# Defina o diretório de trabalho dentro do container
WORKDIR /app

# Copie os arquivos de código-fonte para o diretório de trabalho
COPY . .

# Exponha a porta em que o servidor WebSocket irá rodar
EXPOSE 3015

# Comando para iniciar o servidor
CMD ["ruby", "server.rb"]
