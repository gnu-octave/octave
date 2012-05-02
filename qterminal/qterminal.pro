TEMPLATE	= app

QT += core gui

SOURCES 	= main.cpp 
INCLUDEPATH 	= ../libqterminal

win32 {
	win32-msvc*: include(../msvc.pri)
	LIBS += -L../libqterminal/$$LIBDIR_SUFFIX \
		-lqterminal -luser32 -lkernel32
} else {
	LIBS += -L../libqterminal -lqterminal -lutil
}
