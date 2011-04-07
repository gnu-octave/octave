TEMPLATE	= lib
VERSION		= 0.1.0
DESTDIR 	= ..

TARGET		= qtermwidget

CONFIG		+= qt debug_and_release warn_on build_all staticlib #dll

QT += core gui

MOC_DIR 	= ../.moc

CONFIG(debug, debug|release) {
    OBJECTS_DIR = ../.objs_d
    TARGET 	= qtermwidget_d
} else {
    OBJECTS_DIR = ../.objs
    TARGET 	= qtermwidget
}

DEFINES 	+= HAVE_POSIX_OPENPT	    
#or DEFINES 	+= HAVE_GETPT

HEADERS 	= TerminalCharacterDecoder.h Character.h CharacterColor.h \
		KeyboardTranslator.h \
		ExtendedDefaultTranslator.h \
		Screen.h History.h BlockArray.h konsole_wcwidth.h \
		ScreenWindow.h \
		Emulation.h \
		Vt102Emulation.h TerminalDisplay.h Filter.h LineFont.h \
		Pty.h kpty.h kpty_p.h k3process.h k3processcontroller.h \
		Session.h ShellCommand.h \
		qtermwidget.h

SOURCES 	= TerminalCharacterDecoder.cpp \
		KeyboardTranslator.cpp \
		Screen.cpp History.cpp BlockArray.cpp konsole_wcwidth.cpp \
		ScreenWindow.cpp \
		Emulation.cpp \
		Vt102Emulation.cpp TerminalDisplay.cpp Filter.cpp \
		Pty.cpp kpty.cpp k3process.cpp k3processcontroller.cpp \
		Session.cpp ShellCommand.cpp \
		qtermwidget.cpp

	    


	