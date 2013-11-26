TEMPLATE	= lib
VERSION		= 0.1.0
TARGET		= qterminal

CONFIG		+= qt staticlib

QT += core gui

INCLUDEPATH	+= .

unix {

DEFINES 	+= HAVE_POSIX_OPENPT HAVE_PTY_H HAVE_OPENPTY
#or DEFINES 	+= HAVE_GETPT

HEADERS  = unix/BlockArray.h \
           unix/Character.h \
           unix/CharacterColor.h \
           unix/Emulation.h \
           unix/ExtendedDefaultTranslator.h \
           unix/Filter.h \
           unix/History.h \
           unix/KeyboardTranslator.h \
           unix/konsole_wcwidth.h \
           unix/kpty.h \
           unix/kpty_p.h \
           unix/LineFont.h \
           unix/QUnixTerminalImpl.h \
           unix/Screen.h \
           unix/ScreenWindow.h \
           unix/TerminalCharacterDecoder.h \
           unix/Vt102Emulation.h \
         unix/SelfListener.h \
           unix/TerminalModel.h \
           unix/TerminalView.h

SOURCES  = unix/BlockArray.cpp \
           unix/Emulation.cpp \
           unix/Filter.cpp \
           unix/History.cpp \
           unix/KeyboardTranslator.cpp \
           unix/konsole_wcwidth.cpp \
           unix/kpty.cpp \
           unix/QUnixTerminalImpl.cpp \
           unix/Screen.cpp \
           unix/ScreenWindow.cpp \
           unix/TerminalCharacterDecoder.cpp \
           unix/Vt102Emulation.cpp \
         unix/SelfListener.cpp \
           unix/TerminalModel.cpp \
           unix/TerminalView.cpp
}

win32 {
HEADERS  = win32/QTerminalColors.h \
       win32/QWinTerminalImpl.h

SOURCES  = win32/QTerminalColors.cpp \
       win32/QWinTerminalImpl.cpp
}

win32-msvc* {
  include(../msvc.pri)
}

HEADERS  += QTerminal.h \
       QTerminal \
    QTerminalInterface.h
