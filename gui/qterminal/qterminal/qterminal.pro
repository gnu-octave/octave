TEMPLATE	= app
DESTDIR 	= .

QT += core gui

SOURCES 	= main.cpp 
INCLUDEPATH 	= ../libqterminal

LIBS += -L../libqterminal -lqterminal -lutil



	
