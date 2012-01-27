TEMPLATE	= lib
VERSION		= 0.1.0
DESTDIR 	= .
TARGET		= qterminal

CONFIG		+= staticlib

QT += core gui

DEFINES 	+= HAVE_POSIX_OPENPT	    
#or DEFINES 	+= HAVE_GETPT

HEADERS  = BlockArray.h \
           Character.h \
           CharacterColor.h \
           Emulation.h \
           ExtendedDefaultTranslator.h \
           Filter.h \
           History.h \
           KeyboardTranslator.h \
           konsole_wcwidth.h \
           kpty.h \
           kpty_p.h \
           LineFont.h \
           QTerminal.h \
           Screen.h \
           ScreenWindow.h \
           TerminalCharacterDecoder.h \
           Vt102Emulation.h \
    SessionModel.h \
    SessionView.h \
    SelfListener.h
SOURCES  = BlockArray.cpp \
           Emulation.cpp \
           Filter.cpp \
           History.cpp \
           KeyboardTranslator.cpp \
           konsole_wcwidth.cpp \
           kpty.cpp \
           QTerminal.cpp \
           Screen.cpp \
           ScreenWindow.cpp \
           TerminalCharacterDecoder.cpp \
           Vt102Emulation.cpp \
    SessionModel.cpp \
    SessionView.cpp \
    SelfListener.cpp
