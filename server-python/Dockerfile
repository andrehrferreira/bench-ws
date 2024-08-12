# Use a imagem oficial do PyPy3.9 slim como base
FROM pypy:3.9-slim

# Instale as dependências do sistema necessárias
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libffi-dev \
    libssl-dev \
    llvm \
    libev-dev \
    python3-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Defina o diretório de trabalho dentro do container
WORKDIR /app

# Copie o arquivo requirements.txt para o diretório de trabalho
COPY requirements.txt ./

# Remova as dependências problemáticas do requirements.txt antes de instalar
RUN sed -i '/llvmlite/d' requirements.txt && \
    sed -i '/numba/d' requirements.txt

# Instale as dependências restantes
RUN pypy3 -m pip install --upgrade pip
RUN pypy3 -m pip install --no-cache-dir -r requirements.txt

# Copie o restante dos arquivos da aplicação para o diretório de trabalho
COPY . .

# Exponha a porta em que o servidor WebSocket irá rodar
EXPOSE 3007

# Defina o comando para iniciar o servidor
CMD ["pypy3", "server.py"]
