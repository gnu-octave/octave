TEMPLATE	= app
DESTDIR 	= .

QT += core gui

SOURCES 	= main.cpp 
INCLUDEPATH 	= ../libqterminal

win32 {
	win32-msvc* {
		debug: LIBS += -L../libqterminal/debug
		release: LIBS += -L../libqterminal/release
	} else {
		LIBS += -L../libqterminal
	}
	LIBS += -lqterminal -luser32 -lkernel32
} else {
	LIBS += -L../libqterminal -lqterminal -lutil
}
