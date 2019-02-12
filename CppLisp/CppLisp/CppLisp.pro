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

LIBS += $${OUT_PWD}/../CppLispInterpreter/$${SUBDIRTARGET}libCppLispInterpreter.a

SOURCES += \
        FuelMain.cpp
