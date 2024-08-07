cmake_minimum_required(VERSION 3.10)

# Nome do projeto
project(Server)

# Definição do padrão de compilação (Release/Debug)
set(CMAKE_BUILD_TYPE Release)

# Adiciona as flags do compilador
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Configuração para buscar pacotes do vcpkg
set(CMAKE_TOOLCHAIN_FILE "/mnt/c/vcpkg/scripts/buildsystems/vcpkg.cmake" CACHE STRING "Vcpkg toolchain file")

# Lista de arquivos fonte
file(GLOB SOURCES "src/*.cpp")

# Adiciona o executável
add_executable(Server ${SOURCES})

# Encontrar pacotes necessários
find_package(Crow CONFIG REQUIRED)
find_package(TBB CONFIG REQUIRED)
find_package(asio CONFIG REQUIRED)

# Adiciona os caminhos do ASIO
set(ASIO_INCLUDE_DIR "/mnt/c/vcpkg/installed/x64-linux/include")
include_directories(${ASIO_INCLUDE_DIR})

# Adiciona o caminho da biblioteca ASIO
set(ASIO_LIBRARY "/mnt/c/vcpkg/installed/x64-linux/lib")
link_directories(${ASIO_LIBRARY})

# Linkar bibliotecas
target_link_libraries(Server PRIVATE Crow::Crow TBB::tbb TBB::tbbmalloc TBB::tbbmalloc_proxy asio::asio)

# Definir configurações de compilação adicionais (opcional)
if (MSVC)
    target_compile_options(Server PRIVATE /W4)
else()
    target_compile_options(Server PRIVATE -Wall -Wextra -pedantic)
endif()
