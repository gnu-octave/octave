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
           k3process.h \
           k3processcontroller.h \
           KeyboardTranslator.h \
           konsole_wcwidth.h \
           kpty.h \
           kpty_p.h \
           LineFont.h \
           Pty.h \
           QTerminal.h \
           Screen.h \
           ScreenWindow.h \
           Session.h \
           ShellCommand.h \
           TerminalCharacterDecoder.h \
           TerminalDisplay.h \
           Vt102Emulation.h
SOURCES  = BlockArray.cpp \
           Emulation.cpp \
           Filter.cpp \
           History.cpp \
           k3process.cpp \
           k3processcontroller.cpp \
           KeyboardTranslator.cpp \
           konsole_wcwidth.cpp \
           kpty.cpp \
           Pty.cpp \
           QTerminal.cpp \
           Screen.cpp \
           ScreenWindow.cpp \
           Session.cpp \
           ShellCommand.cpp \
           TerminalCharacterDecoder.cpp \
           TerminalDisplay.cpp \
           Vt102Emulation.cpp
