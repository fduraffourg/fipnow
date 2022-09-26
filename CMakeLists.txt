cmake_minimum_required(VERSION 3.10)
project(fipnow)
find_package(CURL REQUIRED)
find_package(nlohmann_json REQUIRED)

add_executable(fipnow fipnow.cpp)
include_directories(${CURL_INCLUDE_DIR})
target_link_libraries(fipnow ${CURL_LIBRARIES} nlohmann_json::nlohmann_json)
install(TARGETS fipnow DESTINATION bin)
