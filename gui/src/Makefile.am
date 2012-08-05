define MOC_template =
$(1)/moc_%.cpp: $(1)/%.h
	test -d $(1) || mkdir -p $(1)
	@MOC@ -o$$@ $$(DEFS) $$(DEFAULT_INCLUDES) $$(INCLUDES) $$(AM_CPPFLAGS) $$(CPPFLAGS) $$(octave_gui_CPPFLAGS) $$<
endef

SRCDIRS = m-editor qtinfo octave-adapter \
	  ../qterminal/libqterminal/unix ../qterminal/libqterminal/win32 ../qterminal/libqterminal ../qterminal
$(foreach dir,. $(SRCDIRS),$(eval $(call MOC_template,$(dir))))

ui_%.h: %.ui
	@UIC@ -o $@ $<

qrc_%.cpp: %.qrc
	@RCC@ -o $@ $<

bin_PROGRAMS = octave-gui

octave_gui_MOC = \
		 moc_documentation-dockwidget.cpp \
		 moc_files-dockwidget.cpp \
		 moc_history-dockwidget.cpp \
		 moc_main-window.cpp \
		 moc_octave-qt-event-listener.cpp \
		 moc_settings-dialog.cpp \
		 moc_terminal-dockwidget.cpp \
		 moc_welcome-wizard.cpp \
		 moc_workspace-model.cpp \
		 moc_workspace-view.cpp \
		 m-editor/moc_file-editor.cpp \
		 m-editor/moc_file-editor-interface.cpp \
		 m-editor/moc_file-editor-tab.cpp \
		 m-editor/moc_find-dialog.cpp \
		 m-editor/moc_lexer-octave-gui.cpp \
		 octave-adapter/moc_octave-main-thread.cpp \
		 qtinfo/moc_parser.cpp \
		 qtinfo/moc_webinfo.cpp
octave_gui_UI = \
		settings-dialog.ui \
		welcome-wizard.ui
octave_gui_UI_H = $(patsubst %.ui,ui_%.h,$(octave_gui_UI))

octave_gui_RC = qrc_resource.cpp

octave_gui_SOURCES = \
		     documentation-dockwidget.cc \
		     files-dockwidget.cc \
		     history-dockwidget.cc \
		     main-window.cc \
		     octave-gui.cc \
		     octave-qt-event-listener.cc \
		     resource-manager.cc \
		     settings-dialog.cc \
		     terminal-dockwidget.cc \
		     welcome-wizard.cc \
		     workspace-model.cc \
		     workspace-view.cc \
		     m-editor/file-editor.cc \
		     m-editor/file-editor-tab.cc \
		     m-editor/find-dialog.cc \
		     m-editor/lexer-octave-gui.cc \
		     octave-adapter/octave-link.cc \
		     octave-adapter/octave-main-thread.cc \
		     qtinfo/parser.cc \
		     qtinfo/webinfo.cc
nodist_octave_gui_SOURCES = $(octave_gui_MOC) $(octave_gui_RC)

octave_gui_CPPFLAGS = -I@QT_INCDIR@/QtCore \
		      -I@QT_INCDIR@/QtGui \
		      -I@QT_INCDIR@/QtNetwork \
		      -I$(srcdir)/../qterminal/libqterminal \
		      -I$(srcdir)/m-editor \
		      -I$(srcdir)/octave-adapter \
		      -I$(srcdir)/qtinfo \
		      -I$(srcdir)/../../libcruft/misc \
		      -I../../liboctave \
		      -I$(srcdir)/../../liboctave \
		      -I../../src/interp-core \
		      -I$(srcdir)/../../src \
		      -I$(srcdir)/../../src/interp-core \
		      -I$(srcdir)/../../src/interpfcn \
		      -I$(srcdir)/../../src/octave-value \
		      -I$(srcdir)/../../src/operators \
		      -I$(srcdir)/../../src/parse-tree

octave_gui_LDADD = libqterminal.la \
		   ../../src/liboctinterp.la
octave_gui_LDFLAGS = -L@QT_LIBDIR@ -lQtCore -lQtGui -lQtNetwork -lqscintilla2

noinst_HEADERS = \
		 documentation-dockwidget.h \
		 files-dockwidget.h \
		 history-dockwidget.h \
		 main-window.h \
		 octave-qt-event-listener.h \
		 resource-manager.h \
		 settings-dialog.h \
		 symbol-information.h \
		 terminal-dockwidget.h \
		 welcome-wizard.h \
		 workspace-model.h \
		 workspace-view.h \
		 m-editor/file-editor.h \
		 m-editor/file-editor-interface.h \
		 m-editor/file-editor-tab.h \
		 m-editor/find-dialog.h \
		 m-editor/lexer-octave-gui.h \
		 octave-adapter/octave-event.h \
		 octave-adapter/octave-event-observer.h \
		 octave-adapter/octave-event-listener.h \
		 octave-adapter/octave-link.h \
		 octave-adapter/octave-main-thread.h \
		 qtinfo/parser.h \
		 qtinfo/webinfo.h

CLEANFILES = $(octave_gui_MOC) $(octave_gui_UI_H) $(octave_gui_RC)

clean-local:
	-if test "$(srcdir)" != "."; then \
		for d in $(SRCDIRS); do test -d $$d && rmdir $$d; done \
	 fi

BUILT_SOURCES = $(octave_gui_UI_H)

EXTRA_DIST = $(octave_gui_UI) resource.qrc

noinst_LTLIBRARIES = libqterminal.la

noinst_HEADERS += ../qterminal/libqterminal/QTerminal.h \
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

libqterminal_la_CPPFLAGS = $(DEFS)  \
			   -I@QT_INCDIR@/QtCore \
			   -I@QT_INCDIR@/QtGui \
			   -I$(srcdir)/../qterminal/libqterminal

libqterminal_la_MOC = \
		      ../qterminal/libqterminal/moc_QTerminal.cpp \
		      ../qterminal/libqterminal/moc_QTerminalInterface.cpp

nodist_libqterminal_la_SOURCES = $(libqterminal_la_MOC)

if WIN32_TERMINAL
libqterminal_la_SOURCES = \
			  ../qterminal/libqterminal/win32/QTerminalColors.cpp \
			  ../qterminal/libqterminal/win32/QWinTerminalImpl.cpp
libqterminal_la_MOC += ../qterminal/libqterminal/win32/moc_QWinTerminalImpl.cpp
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
		       ../qterminal/libqterminal/unix/moc_Emulation.cpp \
		       ../qterminal/libqterminal/unix/moc_Filter.cpp \
		       ../qterminal/libqterminal/unix/moc_QUnixTerminalImpl.cpp \
		       ../qterminal/libqterminal/unix/moc_ScreenWindow.cpp \
		       ../qterminal/libqterminal/unix/moc_SelfListener.cpp \
		       ../qterminal/libqterminal/unix/moc_TerminalModel.cpp \
		       ../qterminal/libqterminal/unix/moc_TerminalView.cpp \
		       ../qterminal/libqterminal/unix/moc_Vt102Emulation.cpp
endif

CLEANFILES += $(libqterminal_la_MOC)
