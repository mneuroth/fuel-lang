TEMPLATE 	= subdirs

CONFIG          += ordered

SUBDIRS		= CppLispInterpreter \
                  CppLisp

CppLisp.depends = CppLispInterpreter
