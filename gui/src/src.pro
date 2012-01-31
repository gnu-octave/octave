# OctaveGUI - A graphical user interface for Octave
# Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

# Basic settings:
QT                  += core gui webkit network	    # Qt modules
TEMPLATE            = app                           # Build as application
TARGET              = octave-gui                    # Name of the target binary

DESTDIR             = ../bin                           # Destination of the output

TRANSLATIONS        += languages/generic.ts \
                       languages/de-de.ts \
                       languages/pt-br.ts \
                       languages/es-es.ts \
                       languages/ru-ru.ts \
                       languages/uk-ua.ts           # Available translations
LIBS                += -lqscintilla2  \
                       -L../qirc/libqirc -lqirc \
					   -L../qterminal/libqterminal -lqterminal \
                        $$system(mkoctfile -p LIBS) \
                        $$system(mkoctfile -p OCTAVE_LIBS)

mac {
    CONFIG -= app_bundle
}

# Includepaths and libraries to link against:
INCLUDEPATH         += . backend ../qterminal/libqterminal ../qirc/libqirc \
                       $$system(mkoctfile -p INCFLAGS)
INCFLAGS            += $$system(mkoctfile -p INCFLAGS)
mac {
    INCFLAGS += -I/opt/local/include
}

QMAKE_LIBDIR        += $$system(octave-config -p OCTLIBDIR)

mac {
    LFLAGS += -L/opt/local/lib
}

unix {
    LIBS += -lutil
}

win32-g++ {
    QMAKE_LFLAGS += --enable-auto-import
}

win32-msvc* {
    DEFINES += QSCINTILLA_DLL
    #CONFIG += console
    include(msvc-debug.pri)
}

QMAKE_LFLAGS        += $$LFLAGS $$system(mkoctfile -p RLD_FLAG)
QMAKE_CXXFLAGS      += $$INCFLAGS

# Files associated with the project:
SOURCES +=\
    lexer/lexeroctavegui.cpp \
    MainWindow.cpp \
    WorkspaceView.cpp \
    HistoryDockWidget.cpp \
    FilesDockWidget.cpp \
    FileEditorMdiSubWindow.cpp \
    BrowserWidget.cpp \
    SettingsDialog.cpp \
    OctaveGUI.cpp \
    ResourceManager.cpp \
    CommandLineParser.cpp \
    backend/OctaveCallbackThread.cpp \
    backend/OctaveLink.cpp \
    backend/OctaveMainThread.cpp \
    backend/ReadlineAdapter.cpp \
    WelcomeWizard.cpp

HEADERS += \
    lexer/lexeroctavegui.h \
    MainWindow.h \
    WorkspaceView.h \
    HistoryDockWidget.h \
    FilesDockWidget.h \
    FileEditorMdiSubWindow.h \
    BrowserWidget.h \
    SettingsDialog.h \
    ResourceManager.h \
    CommandLineParser.h \
    backend/OctaveCallbackThread.h \
    backend/OctaveLink.h \
    backend/OctaveMainThread.h \
    backend/ReadlineAdapter.h \
    WelcomeWizard.h

FORMS += \
    SettingsDialog.ui \
    WelcomeWizard.ui
