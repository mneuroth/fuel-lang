#-------------------------------------------------
#
# Project created by QtCreator 2018-05-08T21:44:39
#
#-------------------------------------------------

QT       += testlib

QT       -= gui

TARGET = tst_qtlispinterpreterunitteststest
CONFIG   += console
CONFIG   += c++11
CONFIG   -= app_bundle

TEMPLATE = app

# The following define makes your compiler emit warnings if you use
# any feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# see: https://stackoverflow.com/questions/3984104/qmake-how-to-copy-a-file-to-the-output
# OTHER_FILES += ../../Library/fuellib.fuel
#fuellib.files = ../../Library/fuellib.fuel
#fuellib.path = ./library
#INSTALLS += fuellib

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

include(../CppLispInterpreter/CppLispInterpreter.pri)

unix {
    LIBS += -ldl
}

INCLUDEPATH += ../CppLispInterpreter

DEFINES += UNIT_TEST

SOURCES += \
        tst_qtlispinterpreterunitteststest.cpp\
        ../CppLispDebugger/Debugger.cpp

RESOURCES += \
        fueltesteresources.qrc

HEADERS += \
        tst_qtlisputils.h\
        ../CppLispDebugger/Debugger.h
