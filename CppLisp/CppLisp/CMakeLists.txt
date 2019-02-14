cmake_minimum_required (VERSION 2.6)

project (fuel)

add_subdirectory(FuelInterpreter)

add_executable(fuel FuelMain.cpp)

target_link_libraries(fuel FuelInterpreter)

install(TARGETS fuel DESTINATION bin)