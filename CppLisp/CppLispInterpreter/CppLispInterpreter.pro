TEMPLATE    = lib
TARGET      = FuelInterpreter
CONFIG      += c++11
CONFIG      -= app_bundle
CONFIG      -= qt

unix {
    LIBS += -ldl
}

include(CppLispInterpreter.pri)
