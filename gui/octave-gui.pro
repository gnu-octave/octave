# OctaveGUI - A graphical user interface for Octave
# Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.


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
LIBS                += -lqscintilla2 $$system(mkoctfile -p LIBS) $$system(mkoctfile -p OCTAVE_LIBS)

mac {
    CONFIG -= app_bundle
}

# Includepaths and libraries to link against:
INCLUDEPATH         += src src/terminal src/qirc src/backend \
                       $$system(mkoctfile -p INCFLAGS)
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

unix {
    QMAKE_LFLAGS +=  -lutil
}

win32 {
    QMAKE_LFLAGS += --enable-auto-import
}

QMAKE_LFLAGS        += $$LFLAGS $$system(mkoctfile -p RLD_FLAG)
QMAKE_CXXFLAGS      += $$INCFLAGS

# Files associated with the project:
SOURCES +=\
    src/lexer/lexeroctavegui.cpp \
    src/MainWindow.cpp \
    src/WorkspaceView.cpp \
    src/HistoryDockWidget.cpp \
    src/FilesDockWidget.cpp \
    src/FileEditorMdiSubWindow.cpp \
    src/BrowserWidget.cpp \
    src/ImageViewerMdiSubWindow.cpp \
    src/irc/IRCWidget.cpp \
    src/SettingsDialog.cpp \
    src/OctaveGUI.cpp \
    src/ResourceManager.cpp \
    src/CommandLineParser.cpp \
    src/backend/OctaveCallbackThread.cpp \
    src/backend/OctaveLink.cpp \
    src/backend/OctaveMainThread.cpp \
    src/irc/IRCClientImpl.cpp \
    src/terminal/TerminalEmulation.cpp \
    src/backend/ReadlineAdapter.cpp \
    src/TerminalView.cpp \
    src/TerminalHighlighter.cpp \
    src/WelcomeWizard.cpp

unix {
SOURCES +=\
    src/terminal/LinuxTerminalEmulation.cpp \
    src/terminal/KPty.cpp \
    src/terminal/KPtyDevice.cpp
}

HEADERS += \
    src/lexer/lexeroctavegui.h \
    src/MainWindow.h \
    src/WorkspaceView.h \
    src/HistoryDockWidget.h \
    src/FilesDockWidget.h \
    src/FileEditorMdiSubWindow.h \
    src/BrowserWidget.h \
    src/ImageViewerMdiSubWindow.h \
    src/irc/IRCWidget.h \
    src/SettingsDialog.h \
    src/ResourceManager.h \
    src/CommandLineParser.h \
    src/backend/OctaveCallbackThread.h \
    src/backend/OctaveLink.h \
    src/backend/OctaveMainThread.h \
    src/irc/IRCClientInterface.h \
    src/irc/IRCClientImpl.h \
    src/terminal/TerminalEmulation.h \
    src/backend/ReadlineAdapter.h \
    src/TerminalView.h \
    src/TerminalHighlighter.h \
    src/WelcomeWizard.h

unix {
HEADERS += \
    src/terminal/LinuxTerminalEmulation.h \
    src/terminal/KPtyDevice.h \
    src/terminal/KPty.h
}

FORMS += \
    src/SettingsDialog.ui \
    src/WelcomeWizard.ui
