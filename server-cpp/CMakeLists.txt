cmake_minimum_required(VERSION 3.10)


# isso é gerado heuristicamente e pode não estar correto
  find_package(Crow CONFIG REQUIRED)
  target_link_libraries(main PRIVATE Crow::Crow)

find_package(TBB CONFIG REQUIRED)
  target_link_libraries(main PRIVATE TBB::tbb TBB::tbbmalloc TBB::tbbmalloc_proxy)


# Nome do projeto
project(Server)

# Definição do padrão de compilação (Release/Debug)
set(CMAKE_BUILD_TYPE Release)

# Adiciona as flags do compilador
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Inclui o diretório dos headers
include_directories(${CMAKE_SOURCE_DIR}/include)

# Lista de arquivos fonte
file(GLOB SOURCES "src/*.cpp")

# Adiciona o executável
add_executable(Server ${SOURCES})

# Biblioteca ou pacotes adicionais podem ser linkados aqui
# Por exemplo:
# find_package(SomeLibrary REQUIRED)
# target_link_libraries(Server SomeLibrary::SomeLibrary)

# Adiciona suporte ao ASIO se necessário
find_package(asio CONFIG REQUIRED)
target_link_libraries(Server asio::asio)

# Especifica diretórios para buscar bibliotecas e includes (se necessário)
# target_include_directories(Server PRIVATE ${CMAKE_SOURCE_DIR}/path/to/includes)
# target_link_directories(Server PRIVATE ${CMAKE_SOURCE_DIR}/path/to/libs)

# Configuração para buscar pacotes do vcpkg
set(CMAKE_TOOLCHAIN_FILE "${CMAKE_SOURCE_DIR}/vcpkg/scripts/buildsystems/vcpkg.cmake" CACHE STRING "Vcpkg toolchain file")

# Definir configurações de compilação adicionais (opcional)
if (MSVC)
    target_compile_options(Server PRIVATE /W4)
else()
    target_compile_options(Server PRIVATE -Wall -Wextra -pedantic)
endif()
