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

win32 {
    LIBRARY_NAME = libCppLispInterpreter.a
}
macx {
    LIBRARY_NAME = libCppLispInterpreter.dylib
}
unix:!macx {
    LIBRARY_NAME = libCppLispInterpreter.so
}

INCLUDEPATH += ../CppLispInterpreter

LIBS += $${OUT_PWD}/../CppLispInterpreter/$${SUBDIRTARGET}$${LIBRARY_NAME}

SOURCES += \
        FuelMain.cpp
