octave_gui_ICONS = \
  %reldir%/icons/applications-system.png \
  %reldir%/icons/bp-next.png \
  %reldir%/icons/bp-prev.png \
  %reldir%/icons/bp-rm-all.png \
  %reldir%/icons/bp-toggle.png \
  %reldir%/icons/db-cont.png \
  %reldir%/icons/db-step-in.png \
  %reldir%/icons/db-step-out.png \
  %reldir%/icons/db-step.png \
  %reldir%/icons/db-stop.png \
  %reldir%/icons/document-new.png \
  %reldir%/icons/document-open.png \
  %reldir%/icons/document-print.png \
  %reldir%/icons/document-save.png \
  %reldir%/icons/document-save-as.png \
  %reldir%/icons/edit-copy.png \
  %reldir%/icons/edit-cut.png \
  %reldir%/icons/edit-delete.png \
  %reldir%/icons/edit-find.png \
  %reldir%/icons/edit-find-replace.png \
  %reldir%/icons/edit-paste.png \
  %reldir%/icons/edit-redo.png \
  %reldir%/icons/edit-undo.png \
  %reldir%/icons/folder.png \
  %reldir%/icons/folder-new.png \
  %reldir%/icons/go-first.png \
  %reldir%/icons/go-last.png \
  %reldir%/icons/go-up.png \
  %reldir%/icons/graphic_logo_DocumentationDockWidget.png \
  %reldir%/icons/graphic_logo_FileEditor.png \
  %reldir%/icons/graphic_logo_FilesDockWidget.png \
  %reldir%/icons/graphic_logo_HistoryDockWidget.png \
  %reldir%/icons/graphic_logo_NewsDockWidget.png \
  %reldir%/icons/graphic_logo_TerminalDockWidget.png \
  %reldir%/icons/graphic_logo_WorkspaceView.png \
  %reldir%/icons/graphic_logo_ReleaseWidget.png \
  %reldir%/icons/graphic_logo_VariableEditor.png \
  %reldir%/icons/graphic_logo_DocumentationDockWidget.svg \
  %reldir%/icons/graphic_logo_FileEditor.svg \
  %reldir%/icons/graphic_logo_FilesDockWidget.svg \
  %reldir%/icons/graphic_logo_HistoryDockWidget.svg \
  %reldir%/icons/graphic_logo_NewsDockWidget.svg \
  %reldir%/icons/graphic_logo_TerminalDockWidget.svg \
  %reldir%/icons/graphic_logo_WorkspaceView.svg \
  %reldir%/icons/graphic_logo_ReleaseWidget.svg \
  %reldir%/icons/graphic_logo_VariableEditor.svg \
  %reldir%/icons/icons_license \
  %reldir%/icons/letter_logo_DocumentationDockWidget.png \
  %reldir%/icons/letter_logo_FileEditor.png \
  %reldir%/icons/letter_logo_FilesDockWidget.png \
  %reldir%/icons/letter_logo_HistoryDockWidget.png \
  %reldir%/icons/letter_logo_NewsDockWidget.png \
  %reldir%/icons/letter_logo_TerminalDockWidget.png \
  %reldir%/icons/letter_logo_WorkspaceView.png \
  %reldir%/icons/letter_logo_ReleaseWidget.png \
  %reldir%/icons/letter_logo_VariableEditor.png \
  %reldir%/icons/letter_logo_DocumentationDockWidget.svg \
  %reldir%/icons/letter_logo_FileEditor.svg \
  %reldir%/icons/letter_logo_FilesDockWidget.svg \
  %reldir%/icons/letter_logo_HistoryDockWidget.svg \
  %reldir%/icons/letter_logo_NewsDockWidget.svg \
  %reldir%/icons/letter_logo_TerminalDockWidget.svg \
  %reldir%/icons/letter_logo_WorkspaceView.svg \
  %reldir%/icons/letter_logo_ReleaseWidget.svg \
  %reldir%/icons/letter_logo_VariableEditor.svg \
  %reldir%/icons/logo.png \
  %reldir%/icons/plot-xy-curve.png \
  %reldir%/icons/preferences-system.png \
  %reldir%/icons/system-run.png \
  %reldir%/icons/user-home.png \
  %reldir%/icons/view-refresh.png \
  %reldir%/icons/widget-close.png \
  %reldir%/icons/widget-dock.png \
  %reldir%/icons/widget-undock.png \
  %reldir%/icons/widget-close-light.png \
  %reldir%/icons/widget-dock-light.png \
  %reldir%/icons/widget-undock-light.png \
  %reldir%/icons/zoom-in.png \
  %reldir%/icons/zoom-out.png

octave_gui_MOC =

if AMCOND_HAVE_QSCINTILLA

OCTAVE_GUI_SRC_M_EDITOR_MOC = \
  %reldir%/m-editor/moc-file-editor-interface.cc \
  %reldir%/m-editor/moc-file-editor-tab.cc \
  %reldir%/m-editor/moc-file-editor.cc \
  %reldir%/m-editor/moc-find-dialog.cc \
  %reldir%/m-editor/moc-octave-qscintilla.cc \
  %reldir%/m-editor/moc-octave-txt-lexer.cc \
  %reldir%/m-editor/moc-marker.cc

$(OCTAVE_GUI_SRC_M_EDITOR_MOC): | %reldir%/m-editor/$(octave_dirstamp)

octave_gui_MOC += \
  $(OCTAVE_GUI_SRC_M_EDITOR_MOC)

DIRSTAMP_FILES += \
  %reldir%/m-editor/$(octave_dirstamp)

endif

OCTAVE_GUI_SRC_MOC = \
  %reldir%/moc-external-editor-interface.cc \
  %reldir%/moc-dialog.cc \
  %reldir%/moc-documentation-dock-widget.cc \
  %reldir%/moc-documentation.cc \
  %reldir%/moc-files-dock-widget.cc \
  %reldir%/moc-history-dock-widget.cc \
  %reldir%/moc-main-window.cc \
  %reldir%/moc-octave-cmd.cc \
  %reldir%/moc-octave-qt-link.cc \
  %reldir%/moc-settings-dialog.cc \
  %reldir%/moc-terminal-dock-widget.cc \
  %reldir%/moc-color-picker.cc \
  %reldir%/moc-tab-bar.cc \
  %reldir%/moc-resource-manager.cc \
  %reldir%/moc-shortcut-manager.cc \
  %reldir%/moc-welcome-wizard.cc \
  %reldir%/moc-workspace-model.cc \
  %reldir%/moc-workspace-view.cc \
  %reldir%/moc-variable-editor.cc \
  %reldir%/moc-variable-editor-model.cc \
  %reldir%/moc-find-files-dialog.cc \
  %reldir%/moc-find-files-model.cc \
  %reldir%/qtinfo/moc-texinfo-parser.cc \
  %reldir%/qtinfo/moc-webinfo.cc \
  %reldir%/moc-octave-dock-widget.cc

octave_gui_MOC += \
  $(OCTAVE_GUI_SRC_MOC) \
  $(OCTAVE_GUI_EDITOR_MOC)

octave_gui_RC = %reldir%/qrc-resource.cc

$(octave_gui_RC): | %reldir%/$(octave_dirstamp)

DIRSTAMP_FILES += \
  %reldir%/$(octave_dirstamp)

octave_gui_UI = \
  %reldir%/settings-dialog.ui

octave_gui_UI_H = $(patsubst %reldir%/%.ui, %reldir%/ui-%.h, $(octave_gui_UI))

$(octave_gui_UI_H): | %reldir%/$(octave_dirstamp)

BUILT_SOURCES += $(octave_gui_UI_H)

noinst_HEADERS += \
  %reldir%/dialog.h \
  %reldir%/octave-dock-widget.h \
  %reldir%/documentation-dock-widget.h \
  %reldir%/documentation.h \
  %reldir%/external-editor-interface.h \
  %reldir%/files-dock-widget.h \
  %reldir%/history-dock-widget.h \
  %reldir%/m-editor/file-editor-interface.h \
  %reldir%/m-editor/file-editor-tab.h \
  %reldir%/m-editor/file-editor.h \
  %reldir%/m-editor/find-dialog.h \
  %reldir%/m-editor/octave-qscintilla.h \
  %reldir%/m-editor/octave-txt-lexer.h \
  %reldir%/m-editor/marker.h \
  %reldir%/main-window.h \
  %reldir%/octave-gui.h \
  %reldir%/octave-cmd.h \
  %reldir%/octave-qt-link.h \
  %reldir%/octave-settings.h \
  %reldir%/qtinfo/texinfo-parser.h \
  %reldir%/qtinfo/webinfo.h \
  %reldir%/resource-manager.h \
  %reldir%/settings-dialog.h \
  %reldir%/shortcut-manager.h \
  %reldir%/tab-bar.h \
  %reldir%/thread-manager.h \
  %reldir%/terminal-dock-widget.h \
  %reldir%/color-picker.h \
  %reldir%/welcome-wizard.h \
  %reldir%/find-files-dialog.h \
  %reldir%/find-files-model.h \
  %reldir%/workspace-model.h \
  %reldir%/workspace-view.h \
  %reldir%/variable-editor.h \
  %reldir%/variable-editor-model.h

%canon_reldir%_%canon_reldir%_la_SOURCES = \
  %reldir%/dialog.cc \
  %reldir%/documentation-dock-widget.cc \
  %reldir%/documentation.cc \
  %reldir%/external-editor-interface.cc \
  %reldir%/files-dock-widget.cc \
  %reldir%/history-dock-widget.cc \
  %reldir%/m-editor/file-editor-tab.cc \
  %reldir%/m-editor/file-editor.cc \
  %reldir%/m-editor/find-dialog.cc \
  %reldir%/m-editor/octave-qscintilla.cc \
  %reldir%/m-editor/octave-txt-lexer.cc \
  %reldir%/m-editor/marker.cc \
  %reldir%/main-window.cc \
  %reldir%/octave-cmd.cc \
  %reldir%/octave-dock-widget.cc \
  %reldir%/octave-gui.cc \
  %reldir%/octave-qt-link.cc \
  %reldir%/qtinfo/texinfo-parser.cc \
  %reldir%/qtinfo/webinfo.cc \
  %reldir%/resource-manager.cc \
  %reldir%/settings-dialog.cc \
  %reldir%/shortcut-manager.cc \
  %reldir%/tab-bar.cc \
  %reldir%/thread-manager.cc \
  %reldir%/terminal-dock-widget.cc \
  %reldir%/color-picker.cc \
  %reldir%/welcome-wizard.cc \
  %reldir%/find-files-dialog.cc \
  %reldir%/find-files-model.cc \
  %reldir%/workspace-model.cc \
  %reldir%/workspace-view.cc \
  %reldir%/variable-editor.cc \
  %reldir%/variable-editor-model.cc

nodist_%canon_reldir%_%canon_reldir%_la_SOURCES = \
  $(octave_gui_MOC) \
  $(octave_gui_RC)

%canon_reldir%_%canon_reldir%_la_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  $(FT2_CPPFLAGS) \
  $(FONTCONFIG_CPPFLAGS) \
  @OCTGUI_DLL_DEFS@ \
  @QT_CPPFLAGS@ \
  -I$(srcdir)/libgui/qterminal/libqterminal \
  -Ilibgui/src -I$(srcdir)/libgui/src \
  -I$(srcdir)/%reldir%/m-editor \
  -I$(srcdir)/%reldir%/qtinfo \
  -I$(srcdir)/libgui/graphics \
  -I$(srcdir)/liboctave/array \
  -Iliboctave/numeric -I$(srcdir)/liboctave/numeric \
  -Iliboctave/operators -I$(srcdir)/liboctave/operators \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util \
  -Ilibinterp -I$(srcdir)/libinterp \
  -Ilibinterp/parse-tree -I$(srcdir)/libinterp/parse-tree \
  -Ilibinterp/corefcn -I$(srcdir)/libinterp/corefcn \
  -I$(srcdir)/libinterp/octave-value \
  -I$(srcdir)/liboctave/wrappers

%canon_reldir%_%canon_reldir%_la_CFLAGS = $(AM_CFLAGS) $(WARN_CFLAGS)

%canon_reldir%_%canon_reldir%_la_CXXFLAGS = $(AM_CXXFLAGS) $(WARN_CXXFLAGS)

noinst_LTLIBRARIES += %reldir%/libgui-src.la

libgui_EXTRA_DIST += \
  %reldir%/resource.qrc \
  $(octave_gui_UI) \
  $(octave_gui_ICONS)

libgui_CLEANFILES += \
  $(octave_gui_MOC) \
  $(octave_gui_UI_H) \
  $(octave_gui_RC)
