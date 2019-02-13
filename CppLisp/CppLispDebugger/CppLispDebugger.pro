TEMPLATE    = lib
TARGET      = FuelDebugger
CONFIG      += c++11
CONFIG      -= app_bundle
CONFIG      -= qt

Release {
    SUBDIRTARGET = release/
}
Debug {
    SUBDIRTARGET = debug/
}

INCLUDEPATH += ../CppLispInterpreter

LIBS += -L$${OUT_PWD}/../CppLispInterpreter/$${SUBDIRTARGET} -lCppLispInterpreter

SOURCES     += Debugger.cpp

HEADERS     += Debugger.h
 
