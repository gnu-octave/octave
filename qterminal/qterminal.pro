TEMPLATE	= app
DESTDIR 	= ..

CONFIG		+= qt debug_and_release warn_on build_all

QT += core gui

MOC_DIR 	= ../.moc

CONFIG(debug, debug|release) {
    OBJECTS_DIR = ../.objs_d
    TARGET 	= consoleq_d
    LIBS 	+= -L.. ../libqtermwidget_d.a
#    LIBS += -lqtermwidget
} else {
    OBJECTS_DIR = ../.objs
    TARGET 	= consoleq
    LIBS 	+= -L.. ../libqtermwidget.a
}

SOURCES 	= main.cpp 

INCLUDEPATH 	= ../lib

#LIBS 		+= -L.. -lqtermwidget


	