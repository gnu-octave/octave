# Copyright (C) 2012 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/common.mk

QT_INCDIR = @QT_INCDIR@

QT_LIBDIR = @QT_LIBDIR@

QT_LIBS = -lQtCore -lQtGui -lQtNetwork -lqscintilla2

QT_LDFLAGS = -L$(QT_LIBDIR)

SRCDIRS = \
  m-editor \
  qtinfo \
  octave-adapter \
  ../qterminal/libqterminal/unix \
  ../qterminal/libqterminal/win32 \
  ../qterminal/libqterminal \
  ../qterminal

octave_gui_MOC = \
  m-editor/moc-file-editor-interface.cc \
  m-editor/moc-file-editor-tab.cc \
  m-editor/moc-file-editor.cc \
  m-editor/moc-find-dialog.cc \
  m-editor/moc-lexer-octave-gui.cc \
  moc-documentation-dockwidget.cc \
  moc-files-dockwidget.cc \
  moc-history-dockwidget.cc \
  moc-main-window.cc \
  moc-octave-qt-event-listener.cc \
  moc-settings-dialog.cc \
  moc-terminal-dockwidget.cc \
  moc-welcome-wizard.cc \
  moc-workspace-model.cc \
  moc-workspace-view.cc \
  octave-adapter/moc-octave-main-thread.cc \
  qtinfo/moc-parser.cc \
  qtinfo/moc-webinfo.cc

octave_gui_UI = \
  settings-dialog.ui \
  welcome-wizard.ui

octave_gui_UI_H = $(patsubst %.ui, ui-%.h, $(octave_gui_UI))

octave_gui_RC = qrc-resource.cc

octlib_LTLIBRARIES = liboctgui.la

liboctgui_la_SOURCES = \
  documentation-dockwidget.cc \
  files-dockwidget.cc \
  history-dockwidget.cc \
  m-editor/file-editor-tab.cc \
  m-editor/file-editor.cc \
  m-editor/find-dialog.cc \
  m-editor/lexer-octave-gui.cc \
  main-window.cc \
  octave-adapter/octave-event.cc \
  octave-adapter/octave-link.cc \
  octave-adapter/octave-main-thread.cc \
  octave-gui.cc \
  octave-qt-event-listener.cc \
  qtinfo/parser.cc \
  qtinfo/webinfo.cc \
  resource-manager.cc \
  settings-dialog.cc \
  symbol-information.cc \
  terminal-dockwidget.cc \
  welcome-wizard.cc \
  workspace-model.cc \
  workspace-view.cc

nodist_liboctgui_la_SOURCES = $(octave_gui_MOC) $(octave_gui_RC)

liboctgui_la_CPPFLAGS = \
  -I$(QT_INCDIR) \
  -I$(QT_INCDIR)/QtCore \
  -I$(QT_INCDIR)/QtGui \
  -I$(QT_INCDIR)/QtNetwork \
  -I$(srcdir)/../qterminal/libqterminal \
  -I$(srcdir)/m-editor \
  -I$(srcdir)/octave-adapter \
  -I$(srcdir)/qtinfo \
  -I$(srcdir)/../../libcruft/misc \
  -I../../liboctave \
  -I$(srcdir)/../../liboctave \
  -I../../libinterp \
  -I../../libinterp/interp-core \
  -I../../libinterp/interpfcn \
  -I../../libinterp/parse-tree \
  -I$(srcdir)/../../libinterp \
  -I$(srcdir)/../../libinterp/interp-core \
  -I$(srcdir)/../../libinterp/interpfcn \
  -I$(srcdir)/../../libinterp/octave-value \
  -I$(srcdir)/../../libtinerp/operators \
  -I$(srcdir)/../../libinterp/parse-tree

include link-deps.mk

liboctgui_la_LIBADD = \
  libqterminal.la \
  ../../libinterp/liboctinterp.la \
  ../../liboctave/liboctave.la \
  ../../libcruft/libcruft.la \
  $(LIBOCTGUI_LINK_DEPS)

# Increment these as needed and according to the rules in the libtool manual:
liboctgui_current = 0
liboctgui_revision = 0
liboctgui_age = 0

liboctgui_version_info = $(liboctgui_current):$(liboctgui_revision):$(liboctgui_age)

liboctgui_la_LDFLAGS = \
  -version-info $(liboctgui_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  -bindir $(bindir) \
  $(LIBOCTGUI_LINK_OPTS)

noinst_HEADERS = \
  documentation-dockwidget.h \
  files-dockwidget.h \
  history-dockwidget.h \
  m-editor/file-editor-interface.h \
  m-editor/file-editor-tab.h \
  m-editor/file-editor.h \
  m-editor/find-dialog.h \
  m-editor/lexer-octave-gui.h \
  main-window.h \
  octave-adapter/octave-event-listener.h \
  octave-adapter/octave-event-observer.h \
  octave-adapter/octave-event.h \
  octave-adapter/octave-link.h \
  octave-adapter/octave-main-thread.h \
  octave-gui.h \
  octave-qt-event-listener.h \
  qtinfo/parser.h \
  qtinfo/webinfo.h \
  resource-manager.h \
  settings-dialog.h \
  symbol-information.h \
  terminal-dockwidget.h \
  welcome-wizard.h \
  workspace-model.h \
  workspace-view.h

CLEANFILES = $(octave_gui_MOC) $(octave_gui_UI_H) $(octave_gui_RC)

clean-local:
	-if test "$(srcdir)" != "."; then \
	   for d in $(SRCDIRS); do test -d $$d && rmdir $$d; done \
	 fi

BUILT_SOURCES = $(octave_gui_UI_H)

octave_gui_ICONS = \
  icons/arrow_right.png \
  icons/artsbuilderexecute.png \
  icons/bookmark.png \
  icons/bp_next.png \
  icons/bp_prev.png \
  icons/bp_rm_all.png \
  icons/bp_toggle.png \
  icons/chat.png \
  icons/configure.png \
  icons/db_cont.png \
  icons/db_step_in.png \
  icons/db_step_out.png \
  icons/db_step.png \
  icons/db_stop.png \
  icons/editcopy.png \
  icons/editcut.png \
  icons/editpaste.png \
  icons/filenew.png \
  icons/fileopen.png \
  icons/filesaveas.png \
  icons/filesave.png \
  icons/find.png \
  icons/help_index.png \
  icons/icons_license \
  icons/jabber_protocol.png \
  icons/logo.png \
  icons/question.png \
  icons/redled.png \
  icons/redo.png \
  icons/search.png \
  icons/star.png \
  icons/stop.png \
  icons/terminal.png \
  icons/undo.png \
  icons/up.png \
  icons/zoom-in.png \
  icons/zoom-out.png

EXTRA_DIST = \
  $(octave_gui_UI) \
  $(octave_gui_ICONS) \
  resource.qrc

noinst_LTLIBRARIES = libqterminal.la

noinst_HEADERS += \
  ../qterminal/libqterminal/QTerminal.h \
  ../qterminal/libqterminal/QTerminalInterface.h \
  ../qterminal/libqterminal/win32/QTerminalColors.h \
  ../qterminal/libqterminal/win32/QWinTerminalImpl.h \
  ../qterminal/libqterminal/unix/BlockArray.h \
  ../qterminal/libqterminal/unix/Character.h \
  ../qterminal/libqterminal/unix/CharacterColor.h \
  ../qterminal/libqterminal/unix/Emulation.h \
  ../qterminal/libqterminal/unix/ExtendedDefaultTranslator.h \
  ../qterminal/libqterminal/unix/Filter.h \
  ../qterminal/libqterminal/unix/History.h \
  ../qterminal/libqterminal/unix/KeyboardTranslator.h \
  ../qterminal/libqterminal/unix/konsole_wcwidth.h \
  ../qterminal/libqterminal/unix/kpty.h \
  ../qterminal/libqterminal/unix/kpty_p.h \
  ../qterminal/libqterminal/unix/LineFont.h \
  ../qterminal/libqterminal/unix/QUnixTerminalImpl.h \
  ../qterminal/libqterminal/unix/Screen.h \
  ../qterminal/libqterminal/unix/ScreenWindow.h \
  ../qterminal/libqterminal/unix/TerminalCharacterDecoder.h \
  ../qterminal/libqterminal/unix/Vt102Emulation.h \
  ../qterminal/libqterminal/unix/SelfListener.h \
  ../qterminal/libqterminal/unix/TerminalModel.h \
  ../qterminal/libqterminal/unix/TerminalView.h

libqterminal_la_CPPFLAGS = \
  -I$(QT_INCDIR) \
  -I$(QT_INCDIR)/QtCore \
  -I$(QT_INCDIR)/QtGui \
  -I$(srcdir)/../qterminal/libqterminal

libqterminal_la_MOC = \
  ../qterminal/libqterminal/moc-QTerminal.cc \
  ../qterminal/libqterminal/moc-QTerminalInterface.cc

nodist_libqterminal_la_SOURCES = $(libqterminal_la_MOC)

if WIN32_TERMINAL

libqterminal_la_SOURCES = \
  ../qterminal/libqterminal/win32/QTerminalColors.cpp \
  ../qterminal/libqterminal/win32/QWinTerminalImpl.cpp

libqterminal_la_MOC += ../qterminal/libqterminal/win32/moc-QWinTerminalImpl.cc

else

libqterminal_la_SOURCES = \
  ../qterminal/libqterminal/unix/BlockArray.cpp \
  ../qterminal/libqterminal/unix/Emulation.cpp \
  ../qterminal/libqterminal/unix/Filter.cpp \
  ../qterminal/libqterminal/unix/History.cpp \
  ../qterminal/libqterminal/unix/KeyboardTranslator.cpp \
  ../qterminal/libqterminal/unix/konsole_wcwidth.cpp \
  ../qterminal/libqterminal/unix/kpty.cpp \
  ../qterminal/libqterminal/unix/QUnixTerminalImpl.cpp \
  ../qterminal/libqterminal/unix/Screen.cpp \
  ../qterminal/libqterminal/unix/ScreenWindow.cpp \
  ../qterminal/libqterminal/unix/TerminalCharacterDecoder.cpp \
  ../qterminal/libqterminal/unix/Vt102Emulation.cpp \
  ../qterminal/libqterminal/unix/SelfListener.cpp \
  ../qterminal/libqterminal/unix/TerminalModel.cpp \
  ../qterminal/libqterminal/unix/TerminalView.cpp

libqterminal_la_MOC += \
  ../qterminal/libqterminal/unix/moc-Emulation.cc \
  ../qterminal/libqterminal/unix/moc-Filter.cc \
  ../qterminal/libqterminal/unix/moc-QUnixTerminalImpl.cc \
  ../qterminal/libqterminal/unix/moc-ScreenWindow.cc \
  ../qterminal/libqterminal/unix/moc-SelfListener.cc \
  ../qterminal/libqterminal/unix/moc-TerminalModel.cc \
  ../qterminal/libqterminal/unix/moc-TerminalView.cc \
  ../qterminal/libqterminal/unix/moc-Vt102Emulation.cc

endif

moc-%.cc: %.h
	test -d $(@D) || mkdir -p $(@D)
	@MOC@ -o$@ $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(liboctgui_la_CPPFLAGS) $<

ui-%.h: %.ui
	@UIC@ -o $@ $<

qrc-%.cc: %.qrc
	@RCC@ -o $@ $<

CLEANFILES += $(libqterminal_la_MOC)
