cmake_minimum_required (VERSION 2.8)

project (fuel)

add_subdirectory(CppLispInterpreter)
add_subdirectory(CppLispDebugger)

add_executable(fuel CppLisp/FuelMain.cpp)

target_link_libraries(fuel FuelInterpreter ${CMAKE_DL_LIBS})

install(TARGETS fuel DESTINATION bin)