cmake_minimum_required(VERSION 3.10)

# Nome do projeto
project(Server)

# Definição do padrão de compilação (Release/Debug)
set(CMAKE_BUILD_TYPE Release)

# Adiciona as flags do compilador
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Lista de arquivos fonte
file(GLOB SOURCES "src/*.cpp")

# Adiciona o executável
add_executable(Server ${SOURCES})

# Definir o caminho das bibliotecas
set(CROW_INCLUDE_DIR "/app/crow/include")
set(ASIO_INCLUDE_DIR "/app/asio/asio/include")
set(TBB_INCLUDE_DIR "/usr/include/tbb")  # Caminho padrão do TBB no Linux

# Adicionar diretórios de include
include_directories(${CROW_INCLUDE_DIR} ${ASIO_INCLUDE_DIR} ${TBB_INCLUDE_DIR})

# Linkar bibliotecas
target_link_libraries(Server PRIVATE pthread tbb)

# Definir configurações de compilação adicionais (opcional)
if (MSVC)
    target_compile_options(Server PRIVATE /W4)
else()
    target_compile_options(Server PRIVATE -Wall -Wextra -pedantic)
endif()
