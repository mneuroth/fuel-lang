cmake_minimum_required (VERSION 3.1)

project (fuel)

set (CMAKE_CXX_STANDARD 11)

add_subdirectory(CppLispInterpreter)
add_subdirectory(CppLispDebugger)

add_executable(fuel CppLisp/FuelMain.cpp)

target_link_libraries(fuel FuelInterpreter ${CMAKE_DL_LIBS})

install(TARGETS fuel DESTINATION bin)

message("==============================")
message(${CMAKE_SYSTEM_NAME})

if (CMAKE_SYSTEM_NAME MATCHES "Android")
    add_custom_command(TARGET fuel POST_BUILD
            COMMAND "${ANDROID_TOOLCHAIN_PREFIX}strip" -g -S -d --strip-debug --verbose
            fuel
            COMMENT "Strip debug symbols done on final fuel binary.")
endif()
            