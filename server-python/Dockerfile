FROM pypy:3.9-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libffi-dev \
    libssl-dev \
    llvm \
    libev-dev \
    python3-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY requirements.txt ./

RUN sed -i '/llvmlite/d' requirements.txt && \
    sed -i '/numba/d' requirements.txt

RUN pypy3 -m pip install --upgrade pip
RUN pypy3 -m pip install --no-cache-dir -r requirements.txt

COPY . .

EXPOSE 3007

CMD ["pypy3", "server.py"]
