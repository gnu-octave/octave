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

win32-msvc*: include(msvc.pri)

LIBS                += -lreadline -lqscintilla2  \
                       -L../qterminal/libqterminal/$$LIBDIR_SUFFIX -lqterminal -lm \
                       -L../../libcruft/.libs -lcruft \
                       -L../../liboctave/.libs -loctave \
                       -L../../src/.libs -loctinterp

mac {
    CONFIG -= app_bundle
}

# Includepaths and libraries to link against:
INCLUDEPATH         += . \
                       octave-adapter \
                       m-editor \
                       qtinfo \
                       ../qterminal/libqterminal \
                       /usr/include/qt4 \
                       ../.. \
                       ../../src \
                       ../../src/interpfcn \
                       ../../src/interp-core \
                       ../../src/octave-value \
                       ../../src/parse-tree \
                       ../../src/operators \
                       ../../liboctave \
                       ../../libcruft/misc

#QMAKE_LIBDIR        += $$system(octave-config -p OCTLIBDIR)
#unix {
#    QMAKE_RPATHDIR += $$system(octave-config -p OCTLIBDIR)
#}

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
    QMAKE_CXXFLAGS += -wd4244
}

QMAKE_LFLAGS        += $$LFLAGS
QMAKE_CXXFLAGS      += $$INCFLAGS

# Files associated with the project:
SOURCES +=\
    octave-adapter/octave-link.cc \
    octave-adapter/octave-main-thread.cc \
    m-editor/lexer-octave-gui.cc \
    m-editor/file-editor.cc \
    m-editor/file-editor-tab.cc \
    m-editor/find-dialog.cc \
    qtinfo/parser.cc \
    qtinfo/webinfo.cc \
    main-window.cc \
    workspace-view.cc \
    history-dockwidget.cc \
    files-dockwidget.cc \
    settings-dialog.cc \
    octave-gui.cc \
    resource-manager.cc \
    welcome-wizard.cc \
    workspace-model.cc \
    terminal-dockwidget.cc \
    octave-qt-event-listener.cc \
    documentation-dockwidget.cc

HEADERS += \
    octave-adapter/octave-link.h \
    octave-adapter/octave-main-thread.h \
    octave-adapter/octave-event.h \
    octave-adapter/octave-event-observer.h \
    octave-adapter/octave-event-listener.h \
    m-editor/lexer-octave-gui.h \
    m-editor/file-editor.h \
    m-editor/file-editor-interface.h \
    m-editor/file-editor-tab.h \
    m-editor/find-dialog.h \
    qtinfo/parser.h \
    qtinfo/webinfo.h \
    symbol-information.h \
    main-window.h \
    workspace-view.h \
    history-dockwidget.h \
    files-dockwidget.h \
    settings-dialog.h \
    resource-manager.h \
    welcome-wizard.h \
    workspace-model.h \
    terminal-dockwidget.h \
    octave-qt-event-listener.h \
    documentation-dockwidget.h

FORMS += \
    settings-dialog.ui \
    welcome-wizard.ui

RESOURCES += \
    resource.qrc
