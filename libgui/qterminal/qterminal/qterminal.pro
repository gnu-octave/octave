TEMPLATE	= app

QT += core gui

SOURCES 	= main.cpp
INCLUDEPATH 	= ../libqterminal

win32 {
  win32-msvc*: include(../msvc.pri)
  LIBS += -L../libqterminal/$$LIBDIR_SUFFIX \
    -lqterminal -luser32 -lkernel32
} else {
  DEFINES += HAVE_PTY_H HAVE_OPENPTY
  LIBS += -L../libqterminal -lqterminal -lutil
}
