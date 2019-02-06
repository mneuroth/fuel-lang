cmake_minimum_required (VERSION 2.6)

project (fuel)

file(GLOB fuel_src
    "*.h"
    "*.cpp"
)

add_executable(fuel ${fuel_src})