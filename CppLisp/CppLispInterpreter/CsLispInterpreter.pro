TEMPLATE = app
TARGET = fuel
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt

include(CsLispInterpreter.pri)

SOURCES += \
        QtLisp.cpp \
        fuel.cpp
