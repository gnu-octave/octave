octave_gui_ICONS = \
  libgui/src/icons/applications-system.png \
  libgui/src/icons/bp-next.png \
  libgui/src/icons/bp-prev.png \
  libgui/src/icons/bp-rm-all.png \
  libgui/src/icons/bp-toggle.png \
  libgui/src/icons/db-cont.png \
  libgui/src/icons/db-step-in.png \
  libgui/src/icons/db-step-out.png \
  libgui/src/icons/db-step.png \
  libgui/src/icons/db-stop.png \
  libgui/src/icons/document-new.png \
  libgui/src/icons/document-open.png \
  libgui/src/icons/document-print.png \
  libgui/src/icons/document-save.png \
  libgui/src/icons/document-save-as.png \
  libgui/src/icons/edit-copy.png \
  libgui/src/icons/edit-cut.png \
  libgui/src/icons/edit-delete.png \
  libgui/src/icons/edit-find.png \
  libgui/src/icons/edit-find-replace.png \
  libgui/src/icons/edit-paste.png \
  libgui/src/icons/edit-redo.png \
  libgui/src/icons/edit-undo.png \
  libgui/src/icons/folder.png \
  libgui/src/icons/folder-new.png \
  libgui/src/icons/go-first.png \
  libgui/src/icons/go-last.png \
  libgui/src/icons/go-up.png \
  libgui/src/icons/graphic_logo_DocumentationDockWidget.png \
  libgui/src/icons/graphic_logo_FileEditor.png \
  libgui/src/icons/graphic_logo_FilesDockWidget.png \
  libgui/src/icons/graphic_logo_HistoryDockWidget.png \
  libgui/src/icons/graphic_logo_NewsDockWidget.png \
  libgui/src/icons/graphic_logo_TerminalDockWidget.png \
  libgui/src/icons/graphic_logo_WorkspaceView.png \
  libgui/src/icons/graphic_logo_ReleaseWidget.png \
  libgui/src/icons/icons_license \
  libgui/src/icons/letter_logo_DocumentationDockWidget.png \
  libgui/src/icons/letter_logo_FileEditor.png \
  libgui/src/icons/letter_logo_FilesDockWidget.png \
  libgui/src/icons/letter_logo_HistoryDockWidget.png \
  libgui/src/icons/letter_logo_NewsDockWidget.png \
  libgui/src/icons/letter_logo_TerminalDockWidget.png \
  libgui/src/icons/letter_logo_WorkspaceView.png \
  libgui/src/icons/letter_logo_ReleaseWidget.png \
  libgui/src/icons/logo.png \
  libgui/src/icons/preferences-system.png \
  libgui/src/icons/system-run.png \
  libgui/src/icons/user-home.png \
  libgui/src/icons/view-refresh.png \
  libgui/src/icons/widget-close.png \
  libgui/src/icons/widget-dock.png \
  libgui/src/icons/widget-undock.png \
  libgui/src/icons/widget-close-light.png \
  libgui/src/icons/widget-dock-light.png \
  libgui/src/icons/widget-undock-light.png \
  libgui/src/icons/zoom-in.png \
  libgui/src/icons/zoom-out.png

octave_gui_MOC =

if AMCOND_HAVE_QSCINTILLA

OCTAVE_GUI_SRC_M_EDITOR_MOC = \
  libgui/src/m-editor/moc-file-editor-interface.cc \
  libgui/src/m-editor/moc-file-editor-tab.cc \
  libgui/src/m-editor/moc-file-editor.cc \
  libgui/src/m-editor/moc-find-dialog.cc \
  libgui/src/m-editor/moc-octave-qscintilla.cc \
  libgui/src/m-editor/moc-octave-txt-lexer.cc

$(OCTAVE_GUI_SRC_M_EDITOR_MOC): | libgui/src/m-editor/$(octave_dirstamp)

octave_gui_MOC += \
  $(OCTAVE_GUI_SRC_M_EDITOR_MOC)

DIRSTAMP_FILES += \
  libgui/src/m-editor/$(octave_dirstamp)

endif

OCTAVE_GUI_SRC_MOC = \
  libgui/src/moc-dialog.cc \
  libgui/src/moc-documentation-dock-widget.cc \
  libgui/src/moc-files-dock-widget.cc \
  libgui/src/moc-history-dock-widget.cc \
  libgui/src/moc-main-window.cc \
  libgui/src/moc-octave-interpreter.cc \
  libgui/src/moc-octave-qt-link.cc \
  libgui/src/moc-settings-dialog.cc \
  libgui/src/moc-terminal-dock-widget.cc \
  libgui/src/moc-color-picker.cc \
  libgui/src/moc-resource-manager.cc \
  libgui/src/moc-shortcut-manager.cc \
  libgui/src/moc-welcome-wizard.cc \
  libgui/src/moc-workspace-model.cc \
  libgui/src/moc-workspace-view.cc \
  libgui/src/moc-find-files-dialog.cc \
  libgui/src/moc-find-files-model.cc \
  libgui/src/qtinfo/moc-parser.cc \
  libgui/src/qtinfo/moc-webinfo.cc \
  libgui/src/moc-octave-dock-widget.cc

octave_gui_MOC += \
  $(OCTAVE_GUI_SRC_MOC) \
  $(OCTAVE_GUI_EDITOR_MOC)

octave_gui_RC = libgui/src/qrc-resource.cc

$(octave_gui_RC): | libgui/src/$(octave_dirstamp)

DIRSTAMP_FILES += \
  libgui/src/$(octave_dirstamp)

octave_gui_UI = \
  libgui/src/settings-dialog.ui

octave_gui_UI_H = $(patsubst libgui/src/%.ui, libgui/src/ui-%.h, $(octave_gui_UI))

$(octave_gui_UI_H): | libgui/src/$(octave_dirstamp)

BUILT_SOURCES += $(octave_gui_UI_H)

noinst_HEADERS += \
  libgui/src/dialog.h \
  libgui/src/octave-dock-widget.h \
  libgui/src/documentation-dock-widget.h \
  libgui/src/files-dock-widget.h \
  libgui/src/history-dock-widget.h \
  libgui/src/m-editor/file-editor-interface.h \
  libgui/src/m-editor/file-editor-tab.h \
  libgui/src/m-editor/file-editor.h \
  libgui/src/m-editor/find-dialog.h \
  libgui/src/m-editor/octave-qscintilla.h \
  libgui/src/m-editor/octave-txt-lexer.h \
  libgui/src/main-window.h \
  libgui/src/octave-gui.h \
  libgui/src/octave-cmd.h \
  libgui/src/octave-interpreter.h \
  libgui/src/octave-qt-link.h \
  libgui/src/qtinfo/parser.h \
  libgui/src/qtinfo/webinfo.h \
  libgui/src/resource-manager.h \
  libgui/src/settings-dialog.h \
  libgui/src/shortcut-manager.h \
  libgui/src/thread-manager.h \
  libgui/src/terminal-dock-widget.h \
  libgui/src/color-picker.h \
  libgui/src/welcome-wizard.h \
  libgui/src/find-files-dialog.h \
  libgui/src/find-files-model.h \
  libgui/src/workspace-model.h \
  libgui/src/workspace-view.h

libgui_src_libgui_src_la_SOURCES = \
  libgui/src/dialog.cc \
  libgui/src/documentation-dock-widget.cc \
  libgui/src/files-dock-widget.cc \
  libgui/src/history-dock-widget.cc \
  libgui/src/m-editor/file-editor-tab.cc \
  libgui/src/m-editor/file-editor.cc \
  libgui/src/m-editor/find-dialog.cc \
  libgui/src/m-editor/octave-qscintilla.cc \
  libgui/src/m-editor/octave-txt-lexer.cc \
  libgui/src/main-window.cc \
  libgui/src/octave-cmd.cc \
  libgui/src/octave-dock-widget.cc \
  libgui/src/octave-gui.cc \
  libgui/src/octave-interpreter.cc \
  libgui/src/octave-qt-link.cc \
  libgui/src/qtinfo/parser.cc \
  libgui/src/qtinfo/webinfo.cc \
  libgui/src/resource-manager.cc \
  libgui/src/settings-dialog.cc \
  libgui/src/shortcut-manager.cc \
  libgui/src/thread-manager.cc \
  libgui/src/terminal-dock-widget.cc \
  libgui/src/color-picker.cc \
  libgui/src/welcome-wizard.cc \
  libgui/src/find-files-dialog.cc \
  libgui/src/find-files-model.cc \
  libgui/src/workspace-model.cc \
  libgui/src/workspace-view.cc

nodist_libgui_src_libgui_src_la_SOURCES = $(octave_gui_MOC) $(octave_gui_RC)

libgui_src_libgui_src_la_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  $(FT2_CPPFLAGS) \
  $(FONTCONFIG_CPPFLAGS) \
  @OCTGUI_DLL_DEFS@ \
  @QT_CPPFLAGS@ \
  -I$(srcdir)/libgui/qterminal/libqterminal \
  -Ilibgui/src -I$(srcdir)/libgui/src \
  -I$(srcdir)/libgui/src/m-editor \
  -I$(srcdir)/libgui/src/qtinfo \
  -I$(srcdir)/libgui/graphics \
  -I$(top_srcdir)/liboctave/cruft/misc \
  -I$(top_srcdir)/liboctave/array \
  -I$(top_builddir)/liboctave/numeric -I$(top_srcdir)/liboctave/numeric \
  -I$(top_builddir)/liboctave/operators -I$(top_srcdir)/liboctave/operators \
  -I$(top_srcdir)/liboctave/system \
  -I$(top_srcdir)/liboctave/util \
  -I$(top_builddir)/libinterp -I$(top_srcdir)/libinterp \
  -I$(top_builddir)/libinterp/parse-tree -I$(top_srcdir)/libinterp/parse-tree \
  -I$(top_builddir)/libinterp/corefcn -I$(top_srcdir)/libinterp/corefcn \
  -I$(top_srcdir)/libinterp/octave-value \
  -I$(top_builddir)/libgnu -I$(top_srcdir)/libgnu

libgui_src_libgui_src_la_CFLAGS = $(AM_CFLAGS) $(WARN_CFLAGS)

libgui_src_libgui_src_la_CXXFLAGS = $(AM_CXXFLAGS) $(WARN_CXXFLAGS)

noinst_LTLIBRARIES += libgui/src/libgui-src.la

libgui_EXTRA_DIST += \
  libgui/src/resource.qrc \
  $(octave_gui_UI) \
  $(octave_gui_ICONS)

libgui_CLEANFILES += \
  $(octave_gui_MOC) \
  $(octave_gui_UI_H) \
  $(octave_gui_RC)

