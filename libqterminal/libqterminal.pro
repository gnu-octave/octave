TEMPLATE	= lib
VERSION		= 0.1.0
DESTDIR 	= .
TARGET		= qterminal

CONFIG		+= staticlib

QT += core gui

DEFINES 	+= HAVE_POSIX_OPENPT	    
#or DEFINES 	+= HAVE_GETPT

HEADERS 	= TerminalCharacterDecoder.h \
                Character.h \
                CharacterColor.h \
		KeyboardTranslator.h \
		ExtendedDefaultTranslator.h \
                Screen.h \
                History.h \
                BlockArray.h \
                konsole_wcwidth.h \
		ScreenWindow.h \
		Emulation.h \
                Vt102Emulation.h \
                TerminalDisplay.h \
                Filter.h LineFont.h \
                Pty.h \
                kpty.h \
                kpty_p.h \
                k3process.h \
                k3processcontroller.h \
                Session.h \
                ShellCommand.h \
    QTerminal.h

SOURCES 	= TerminalCharacterDecoder.cpp \
		KeyboardTranslator.cpp \
                Screen.cpp \
                History.cpp \
                BlockArray.cpp \
                konsole_wcwidth.cpp \
		ScreenWindow.cpp \
		Emulation.cpp \
                Vt102Emulation.cpp \
                TerminalDisplay.cpp \
                Filter.cpp \
                Pty.cpp \
                kpty.cpp \
                k3process.cpp \
                k3processcontroller.cpp \
                Session.cpp \
                ShellCommand.cpp \
    QTerminal.cpp
