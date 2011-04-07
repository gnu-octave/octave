#-------------------------------------------------
#
# Project created by QtCreator 2011-04-04T12:17:52
#
#-------------------------------------------------

QT       += core gui webkit
UI_DIR = ui-files
MOC_DIR = moc-files
OBJECTS_DIR = object-files
TARGET = Quint
TEMPLATE = app
DEFINES += HAVE_POSIX_OPENPT
INCLUDEPATH += src
DESTDIR = bin
SOURCES +=\
        src/TerminalCharacterDecoder.cpp \
        src/KeyboardTranslator.cpp \
        src/Screen.cpp \
        src/History.cpp \
        src/BlockArray.cpp \
        src/konsole_wcwidth.cpp \
        src/ScreenWindow.cpp \
        src/Emulation.cpp \
        src/Vt102Emulation.cpp \
        src/TerminalDisplay.cpp \
        src/Filter.cpp \
        src/Pty.cpp \
        src/kpty.cpp \
        src/k3process.cpp \
        src/k3processcontroller.cpp \
        src/Session.cpp \
        src/ShellCommand.cpp \
        src/QTerminalWidget.cpp \
        src/TerminalMdiSubWindow.cpp \
        src/MainWindow.cpp \
        src/Quint.cpp

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
