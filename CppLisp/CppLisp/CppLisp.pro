TEMPLATE = app
TARGET = fuel
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt

Release {
    SUBDIRTARGET = release/
}
Debug {
    SUBDIRTARGET = debug/
}

INCLUDEPATH += ../CppLispInterpreter

LIBS += -L$${OUT_PWD}/../CppLispInterpreter/$${SUBDIRTARGET} -lCppLispInterpreter

SOURCES += \
        FuelMain.cpp
