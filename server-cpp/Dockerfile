FROM gcc:11.3 as build

RUN apt-get update && apt-get install -y \
    cmake \
    libtbb-dev \
    git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

RUN git clone https://github.com/CrowCpp/Crow.git /app/crow

RUN git clone https://github.com/chriskohlhoff/asio.git /app/asio

COPY . .

RUN rm -rf build && mkdir build

RUN cd build && cmake -DCMAKE_CXX_FLAGS="-I/app/crow/include -I/app/asio/asio/include -pthread" .. && make

RUN ls -l /app/build

FROM debian:buster-slim

RUN apt-get update && apt-get install -y \
    libtbb2 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

EXPOSE 3010

CMD ["./app/build/server-cpp"]
