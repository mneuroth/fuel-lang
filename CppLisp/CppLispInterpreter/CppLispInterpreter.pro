TEMPLATE = app
TARGET = fuel
CONFIG += console
CONFIG += c++11
CONFIG -= app_bundle
CONFIG -= qt

include(CppLispInterpreter.pri)

SOURCES += FuelMain.cpp
