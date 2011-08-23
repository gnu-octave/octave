# OctaveGUI - A graphical user interface for Octave
# Copyright (C) 2011 Jacob Dawid
# jacob.dawid@googlemail.com
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# Basic settings:
QT                  += core gui webkit network	    # Qt modules
TEMPLATE            = app                           # Build as application
TARGET              = octave-gui                    # Name of the target binary

DESTDIR             = bin                           # Destination of the output
UI_DIR              = ui-files                      # Folder for ui files
MOC_DIR             = moc-files                     # Folder for moc files
OBJECTS_DIR         = object-files                  # Folder for object files

TRANSLATIONS        += languages/generic.ts \
                       languages/de-de.ts \
                       languages/pt-br.ts \
                       languages/es-es.ts \
                       languages/ru-ru.ts \
                       languages/uk-ua.ts           # Available translations
LIBS                += -lqscintilla2

mac {
    CONFIG -= app_bundle
}

# Includepaths and libraries to link against:
INCLUDEPATH         += src src/terminal src/qirc src/backend
INCFLAGS            += $$system(mkoctfile -p INCFLAGS)
mac {
    INCFLAGS += -I/opt/local-native/include
}

LFLAGS              += $$system(mkoctfile -p LFLAGS) \
                       $$system(mkoctfile -p OCTAVE_LIBS) \
                       $$system(mkoctfile -p LIBS)
mac {
    LFLAGS += -L/opt/local-native/lib
}

QMAKE_LFLAGS        += $$LFLAGS -lutil $$system(mkoctfile -p RLD_FLAG)
QMAKE_CXXFLAGS      += $$INCFLAGS

# Files associated with the project:
SOURCES +=\
        src/lexer/lexeroctavegui.cpp \
        src/terminal/kpty.cpp \
        src/terminal/kptydevice.cpp \
        src/MainWindow.cpp \
    	  src/OctaveTerminal.cpp \
    	  src/VariablesDockWidget.cpp \
    	  src/HistoryDockWidget.cpp \
    	  src/FilesDockWidget.cpp \
    	  src/FileEditorMdiSubWindow.cpp \
    	  src/BrowserWidget.cpp \
    	  src/ImageViewerMdiSubWindow.cpp \
    src/IRCWidget.cpp \
    src/SettingsDialog.cpp \
    src/OctaveGUI.cpp \
    src/ResourceManager.cpp \
    src/CommandLineParser.cpp \
    src/backend/OctaveCallbackThread.cpp \
    src/backend/OctaveLink.cpp \
    src/backend/OctaveMainThread.cpp \
    src/qirc/IRCClientImpl.cpp \
    src/terminal/TerminalEmulation.cpp \
    src/terminal/LinuxTerminalEmulation.cpp \
    src/backend/ReadlineAdapter.cpp

HEADERS += \
        src/lexer/lexeroctavegui.h \
        src/terminal/kpty.h \
        src/terminal/kpty_p.h \
        src/terminal/kptydevice.h \
    	  src/MainWindow.h \
    	  src/OctaveTerminal.h \
    	  src/VariablesDockWidget.h \
    	  src/HistoryDockWidget.h \
    	  src/FilesDockWidget.h \
    	  src/FileEditorMdiSubWindow.h \
    	  src/BrowserWidget.h \
    	  src/ImageViewerMdiSubWindow.h \
    src/IRCWidget.h \
    src/SettingsDialog.h \
    src/ResourceManager.h \
    src/CommandLineParser.h \
    src/backend/OctaveCallbackThread.h \
    src/backend/OctaveLink.h \
    src/backend/OctaveMainThread.h \
    src/qirc/IRCClientInterface.h \
    src/qirc/IRCClientImpl.h \
    src/terminal/TerminalEmulation.h \
    src/terminal/LinuxTerminalEmulation.h \
    src/backend/ReadlineAdapter.h

FORMS += \
    src/SettingsDialog.ui
