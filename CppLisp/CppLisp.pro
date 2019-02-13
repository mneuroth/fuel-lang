TEMPLATE 	= subdirs

CONFIG          += ordered

SUBDIRS		= CppLispInterpreter \
                  CppLispDebugger \
                  CppLisp

CppLisp.depends = CppLispInterpreter
