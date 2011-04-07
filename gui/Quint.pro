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
        terminal.cpp

HEADERS += mainwindow.h \
        terminal.h

LIBS += ../Quint/qtermwidget/libqtermwidget.a

INCLUDEPATH += qtermwidget/lib
