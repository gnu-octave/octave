# Quint - A graphical user interface for Octave
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
QT                  += core gui webkit xml opengl   # Qt modules
TEMPLATE            = app                           # Build as application
TARGET              = Quint                         # Name of the target binary

DESTDIR             = bin                           # Destination of the output
UI_DIR              = ui-files                      # Folder for ui files
MOC_DIR             = moc-files                     # Folder for moc files
OBJECTS_DIR         = object-files                  # Folder for object files

TRANSLATIONS        += languages/german             # Available translations

# Includepaths and libraries to link against:
INCLUDEPATH         += src
INCFLAGS            += $$system(mkoctfile -p INCFLAGS)
LFLAGS              += $$system(mkoctfile -p LFLAGS) \
                       $$system(mkoctfile -p OCTAVE_LIBS) \
                       $$system(mkoctfile -p LIBS)
QMAKE_LFLAGS        += $$LFLAGS -lutil $$system(mkoctfile -p RLD_FLAG)
QMAKE_CXXFLAGS      += $$INCFLAGS

# Files associated with the project:
SOURCES +=\
        src/TerminalCharacterDecoder.cpp \
        src/KeyboardTranslator.cpp \
        src/Screen.cpp \
        src/History.cpp \
        src/BlockArray.cpp \
        src/konsole_wcwidth.cpp \
        src/ScreenWindow.cpp \
        src/Emulation.cpp \
        src/Vt102Emulation.cpp \
        src/TerminalDisplay.cpp \
        src/Filter.cpp \
        src/Pty.cpp \
        src/kpty.cpp \
        src/kptyprocess.cpp \
        src/kprocess.cpp \
        src/kptydevice.cpp \
        src/Session.cpp \
        src/ShellCommand.cpp \
        src/QTerminalWidget.cpp \
        src/MainWindow.cpp \
        src/Quint.cpp \
        src/OctaveLink.cpp \
        src/ProcessInfo.cpp \
    src/OctaveTerminal.cpp \
    src/VariablesDockWidget.cpp \
    src/HistoryDockWidget.cpp \
    src/FilesDockWidget.cpp \
    src/FileEditorMdiSubWindow.cpp \
    src/SyntaxHighlighter.cpp \
    src/BrowserWidget.cpp \
    src/NumberedCodeEdit.cpp \
    src/SimpleEditor.cpp \
    src/ImageViewerMdiSubWindow.cpp \
    src/PlotterWidget.cpp \
    src/Plot2dWidget.cpp

HEADERS += \
        src/TerminalCharacterDecoder.h \
        src/Character.h \
        src/CharacterColor.h \
        src/KeyboardTranslator.h \
        src/Screen.h \
        src/History.h \
        src/BlockArray.h \
        src/konsole_wcwidth.h \
        src/ScreenWindow.h \
        src/Emulation.h \
        src/Vt102Emulation.h \
        src/TerminalDisplay.h \
        src/Filter.h \
        src/LineFont.h \
        src/Pty.h \
        src/kpty.h \
        src/kpty_p.h \
        src/kptyprocess.h \
        src/kprocess.h \
        src/kprocess_p.h \
        src/kptydevice.h \
        src/Session.h \
        src/ShellCommand.h \
        src/QTerminalWidget.h \
    	src/MainWindow.h \
        src/OctaveLink.h \
        src/ProcessInfo.h \
    src/OctaveTerminal.h \
    src/VariablesDockWidget.h \
    src/HistoryDockWidget.h \
    src/FilesDockWidget.h \
    src/FileEditorMdiSubWindow.h \
    src/SyntaxHighlighter.h \
    src/BrowserWidget.h \
    src/NumberedCodeEdit.h \
    src/SimpleEditor.h \
    src/ImageViewerMdiSubWindow.h \
    src/PlotterWidget.h \
    src/Plot2dWidget.h

