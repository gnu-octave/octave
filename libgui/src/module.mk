octave_gui_ICONS = \
  %reldir%/icons/applications-system.png \
  %reldir%/icons/bookmark-new.png \
  %reldir%/icons/bottom_left_corner.png \
  %reldir%/icons/bottom_right_corner.png \
  %reldir%/icons/bottom_side.png \
  %reldir%/icons/bp-next.png \
  %reldir%/icons/bp-prev.png \
  %reldir%/icons/bp-rm-all.png \
  %reldir%/icons/bp-toggle.png \
  %reldir%/icons/circle.png \
  %reldir%/icons/cross.png \
  %reldir%/icons/db-cont.png \
  %reldir%/icons/db-step-in.png \
  %reldir%/icons/db-step-out.png \
  %reldir%/icons/db-step.png \
  %reldir%/icons/db-stop.png \
  %reldir%/icons/dialog-error.png \
  %reldir%/icons/dialog-information.png \
  %reldir%/icons/dialog-warning.png \
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
  %reldir%/icons/figure-axes.png \
  %reldir%/icons/figure-grid.png \
  %reldir%/icons/figure-pan.png \
  %reldir%/icons/figure-rotate.png \
  %reldir%/icons/figure-text.png \
  %reldir%/icons/figure-zoom-in.png \
  %reldir%/icons/figure-zoom-original.png \
  %reldir%/icons/figure-zoom-out.png \
  %reldir%/icons/fleur.png \
  %reldir%/icons/folder.png \
  %reldir%/icons/folder-new.png \
  %reldir%/icons/go-down.png \
  %reldir%/icons/go-first.png \
  %reldir%/icons/go-home.png \
  %reldir%/icons/go-last.png \
  %reldir%/icons/go-next.png \
  %reldir%/icons/go-previous.png \
  %reldir%/icons/go-up.png \
  %reldir%/icons/graphic_logo_DocumentationDockWidget.png \
  %reldir%/icons/graphic_logo_Figure.png \
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
  %reldir%/icons/hand2.png \
  %reldir%/icons/icons_license \
  %reldir%/icons/left_side.png \
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
  %reldir%/icons/right_side.png \
  %reldir%/icons/system-run.png \
  %reldir%/icons/top_left_corner.png \
  %reldir%/icons/top_right_corner.png \
  %reldir%/icons/top_side.png \
  %reldir%/icons/user-home.png \
  %reldir%/icons/view-refresh.png \
  %reldir%/icons/widget-close.png \
  %reldir%/icons/widget-dock.png \
  %reldir%/icons/widget-undock.png \
  %reldir%/icons/widget-close-light.png \
  %reldir%/icons/widget-dock-light.png \
  %reldir%/icons/widget-undock-light.png \
  %reldir%/icons/zoom-in.png \
  %reldir%/icons/zoom-original.png \
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
  %reldir%/moc-command-widget.cc \
  %reldir%/moc-community-news.cc \
  %reldir%/moc-dialog.cc \
  %reldir%/moc-documentation-dock-widget.cc \
  %reldir%/moc-documentation.cc \
  %reldir%/moc-documentation-bookmarks.cc \
  %reldir%/moc-dw-main-window.cc \
  %reldir%/moc-files-dock-widget.cc \
  %reldir%/moc-gui-settings.cc \
  %reldir%/moc-history-dock-widget.cc \
  %reldir%/moc-interpreter-qobject.cc \
  %reldir%/moc-led-indicator.cc \
  %reldir%/moc-main-window.cc \
  %reldir%/moc-news-reader.cc \
  %reldir%/moc-octave-qobject.cc \
  %reldir%/moc-release-notes.cc \
  %reldir%/moc-settings-dialog.cc \
  %reldir%/moc-terminal-dock-widget.cc \
  %reldir%/moc-color-picker.cc \
  %reldir%/moc-tab-bar.cc \
  %reldir%/moc-qt-interpreter-events.cc \
  %reldir%/moc-resource-manager.cc \
  %reldir%/moc-shortcut-manager.cc \
  %reldir%/moc-welcome-wizard.cc \
  %reldir%/moc-workspace-model.cc \
  %reldir%/moc-workspace-view.cc \
  %reldir%/moc-variable-editor.cc \
  %reldir%/moc-variable-editor-model.cc \
  %reldir%/moc-find-files-dialog.cc \
  %reldir%/moc-find-files-model.cc \
  %reldir%/moc-octave-dock-widget.cc \
  %reldir%/moc-set-path-dialog.cc \
  %reldir%/moc-set-path-model.cc

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
  %reldir%/command-widget.h \
  %reldir%/community-news.h \
  %reldir%/dialog.h \
  %reldir%/octave-dock-widget.h \
  %reldir%/documentation-dock-widget.h \
  %reldir%/documentation.h \
  %reldir%/documentation-bookmarks.h \
  %reldir%/dw-main-window.h \
  %reldir%/gui-preferences-all.h \
  %reldir%/gui-preferences-cs.h \
  %reldir%/gui-preferences-dc.h \
  %reldir%/gui-preferences-dw.h \
  %reldir%/gui-preferences-ed.h \
  %reldir%/gui-preferences-fb.h \
  %reldir%/gui-preferences-ff.h \
  %reldir%/gui-preferences-global.h \
  %reldir%/gui-preferences-gp.h \
  %reldir%/gui-preferences-hw.h \
  %reldir%/gui-preferences-mw.h \
  %reldir%/gui-preferences-nr.h \
  %reldir%/gui-preferences-pd.h \
  %reldir%/gui-preferences-sc.h \
  %reldir%/gui-preferences-sd.h \
  %reldir%/gui-preferences-ve.h \
  %reldir%/gui-preferences-ws.h \
  %reldir%/gui-preferences.h \
  %reldir%/gui-settings.h \
  %reldir%/external-editor-interface.h \
  %reldir%/files-dock-widget.h \
  %reldir%/graphics-init.h \
  %reldir%/history-dock-widget.h \
  %reldir%/interpreter-qobject.h \
  %reldir%/led-indicator.h \
  %reldir%/m-editor/file-editor-interface.h \
  %reldir%/m-editor/file-editor-tab.h \
  %reldir%/m-editor/file-editor.h \
  %reldir%/m-editor/find-dialog.h \
  %reldir%/m-editor/octave-qscintilla.h \
  %reldir%/m-editor/octave-txt-lexer.h \
  %reldir%/m-editor/marker.h \
  %reldir%/main-window.h \
  %reldir%/news-reader.h \
  %reldir%/octave-qobject.h \
  %reldir%/octave-qtutils.h \
  %reldir%/qt-application.h \
  %reldir%/qt-interpreter-events.h \
  %reldir%/qt-utils.h \
  %reldir%/release-notes.h \
  %reldir%/resource-manager.h \
  %reldir%/settings-dialog.h \
  %reldir%/shortcut-manager.h \
  %reldir%/tab-bar.h \
  %reldir%/terminal-dock-widget.h \
  %reldir%/color-picker.h \
  %reldir%/welcome-wizard.h \
  %reldir%/find-files-dialog.h \
  %reldir%/find-files-model.h \
  %reldir%/workspace-model.h \
  %reldir%/workspace-view.h \
  %reldir%/variable-editor.h \
  %reldir%/variable-editor-model.h \
  %reldir%/set-path-dialog.h \
  %reldir%/set-path-model.h \
  %reldir%/gui-utils.h


%canon_reldir%_%canon_reldir%_la_SOURCES = \
  %reldir%/command-widget.cc \
  %reldir%/community-news.cc \
  %reldir%/dialog.cc \
  %reldir%/documentation-dock-widget.cc \
  %reldir%/documentation.cc \
  %reldir%/documentation-bookmarks.cc \
  %reldir%/dw-main-window.cc \
  %reldir%/external-editor-interface.cc \
  %reldir%/files-dock-widget.cc \
  %reldir%/graphics-init.cc \
  %reldir%/gui-settings.cc \
  %reldir%/history-dock-widget.cc \
  %reldir%/interpreter-qobject.cc \
  %reldir%/led-indicator.cc \
  %reldir%/m-editor/file-editor-tab.cc \
  %reldir%/m-editor/file-editor.cc \
  %reldir%/m-editor/find-dialog.cc \
  %reldir%/m-editor/octave-qscintilla.cc \
  %reldir%/m-editor/octave-txt-lexer.cc \
  %reldir%/m-editor/marker.cc \
  %reldir%/main-window.cc \
  %reldir%/news-reader.cc \
  %reldir%/octave-dock-widget.cc \
  %reldir%/octave-qobject.cc \
  %reldir%/qt-interpreter-events.cc \
  %reldir%/qt-application.cc \
  %reldir%/release-notes.cc \
  %reldir%/resource-manager.cc \
  %reldir%/settings-dialog.cc \
  %reldir%/shortcut-manager.cc \
  %reldir%/tab-bar.cc \
  %reldir%/terminal-dock-widget.cc \
  %reldir%/color-picker.cc \
  %reldir%/welcome-wizard.cc \
  %reldir%/find-files-dialog.cc \
  %reldir%/find-files-model.cc \
  %reldir%/workspace-model.cc \
  %reldir%/workspace-view.cc \
  %reldir%/variable-editor.cc \
  %reldir%/variable-editor-model.cc \
  %reldir%/set-path-dialog.cc \
  %reldir%/set-path-model.cc \
  %reldir%/gui-utils.cc

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
  -Ilibgui/graphics -I$(srcdir)/libgui/graphics \
  -I$(srcdir)/%reldir%/m-editor \
  -Iliboctave \
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

noinst_LTLIBRARIES += %reldir%/libgui-src.la

libgui_EXTRA_DIST += \
  %reldir%/resource.qrc \
  $(octave_gui_UI) \
  $(octave_gui_ICONS)

libgui_CLEANFILES += \
  $(octave_gui_MOC) \
  $(octave_gui_UI_H) \
  $(octave_gui_RC)
