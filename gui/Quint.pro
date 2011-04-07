#-------------------------------------------------
#
# Project created by QtCreator 2011-04-04T12:17:52
#
#-------------------------------------------------

QT       += core gui webkit
OBJECTS_DIR = object-files
TARGET = Quint
TEMPLATE = app
DEFINES += HAVE_POSIX_OPENPT
SOURCES +=\
        TerminalCharacterDecoder.cpp \
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
        QTerminalWidget.cpp \
        TerminalMdiSubWindow.cpp \
    MainWindow.cpp \
    Quint.cpp

HEADERS += \
        TerminalCharacterDecoder.h \
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
        Filter.h \
        LineFont.h \
        Pty.h \
        kpty.h \
        kpty_p.h \
        k3process.h \
        k3processcontroller.h \
        Session.h \
        ShellCommand.h \
        QTerminalWidget.h \
        TerminalMdiSubWindow.h \
    MainWindow.h
