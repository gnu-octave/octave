#-------------------------------------------------
#
# Project created by QtCreator 2011-04-04T12:17:52
#
#-------------------------------------------------

QT       += core gui webkit

TARGET = Quint
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    octaveterminal.cpp \
    clientmanager.cpp \
    client.cpp \
    terminalhighlighter.cpp \
    terminal.cpp

HEADERS  += mainwindow.h \
    octaveterminal.h \
    clientmanager.h \
    client.h \
    terminalhighlighter.h \
    terminal.h

LIBS += ../Quint/qtermwidget/libqtermwidget.a

INCLUDEPATH += qtermwidget/lib
