if AMCOND_BUILD_QT_GRAPHICS

OCTAVE_GUI_GRAPHICS_MOC = \
  %reldir%/moc-ButtonControl.cc \
  %reldir%/moc-ButtonGroup.cc \
  %reldir%/moc-Canvas.cc \
  %reldir%/moc-Container.cc \
  %reldir%/moc-ContextMenu.cc \
  %reldir%/moc-EditControl.cc \
  %reldir%/moc-Figure.cc \
  %reldir%/moc-FigureWindow.cc \
  %reldir%/moc-ListBoxControl.cc \
  %reldir%/moc-Menu.cc \
  %reldir%/moc-Object.cc \
  %reldir%/moc-ObjectProxy.cc \
  %reldir%/moc-PopupMenuControl.cc \
  %reldir%/moc-PushTool.cc \
  %reldir%/moc-SliderControl.cc \
  %reldir%/moc-Table.cc \
  %reldir%/moc-TextEdit.cc \
  %reldir%/moc-ToggleTool.cc \
  %reldir%/moc-ToolBar.cc \
  %reldir%/moc-annotation-dialog.cc \
  %reldir%/moc-qt-graphics-toolkit.cc

$(OCTAVE_GUI_GRAPHICS_MOC): | %reldir%/$(octave_dirstamp)

DIRSTAMP_FILES += \
  %reldir%/$(octave_dirstamp)

libgraphics_MOC = \
  $(OCTAVE_GUI_GRAPHICS_MOC)

libgraphics_MOC_H = $(libgraphics_MOC:.cc=.h)

libgraphics_UI = \
  %reldir%/annotation-dialog.ui

libgraphics_UI_H = $(patsubst %reldir%/%.ui, %reldir%/ui-%.h, $(libgraphics_UI))

$(libgraphics_UI_H): | %reldir%/$(octave_dirstamp)

BUILT_SOURCES += $(libgraphics_UI_H)

noinst_HEADERS += \
  %reldir%/BaseControl.h \
  %reldir%/ButtonControl.h \
  %reldir%/ButtonGroup.h \
  %reldir%/Canvas.h \
  %reldir%/CheckBoxControl.h \
  %reldir%/Container.h \
  %reldir%/ContextMenu.h \
  %reldir%/EditControl.h \
  %reldir%/Figure.h \
  %reldir%/FigureWindow.h \
  %reldir%/GLCanvas.h \
  %reldir%/GenericEventNotify.h \
  %reldir%/KeyMap.h \
  %reldir%/ListBoxControl.h \
  %reldir%/Logger.h \
  %reldir%/Menu.h \
  %reldir%/MenuContainer.h \
  %reldir%/Object.h \
  %reldir%/ObjectProxy.h \
  %reldir%/Panel.h \
  %reldir%/PopupMenuControl.h \
  %reldir%/PushButtonControl.h \
  %reldir%/PushTool.h \
  %reldir%/QtHandlesUtils.h \
  %reldir%/RadioButtonControl.h \
  %reldir%/SliderControl.h \
  %reldir%/Table.h \
  %reldir%/TextControl.h \
  %reldir%/TextEdit.h \
  %reldir%/ToggleButtonControl.h \
  %reldir%/ToggleTool.h \
  %reldir%/ToolBar.h \
  %reldir%/ToolBarButton.h \
  %reldir%/annotation-dialog.h \
  %reldir%/gl-select.h \
  %reldir%/qopengl-functions.h \
  %reldir%/qt-graphics-toolkit.h \
  $(TEMPLATE_SRC)

%canon_reldir%_libgraphics_la_SOURCES = \
  %reldir%/BaseControl.cc \
  %reldir%/ButtonControl.cc \
  %reldir%/ButtonGroup.cc \
  %reldir%/Canvas.cc \
  %reldir%/CheckBoxControl.cc \
  %reldir%/Container.cc \
  %reldir%/ContextMenu.cc \
  %reldir%/EditControl.cc \
  %reldir%/Figure.cc \
  %reldir%/FigureWindow.cc \
  %reldir%/GLCanvas.cc \
  %reldir%/KeyMap.cc \
  %reldir%/ListBoxControl.cc \
  %reldir%/Logger.cc \
  %reldir%/Menu.cc \
  %reldir%/Object.cc \
  %reldir%/ObjectProxy.cc \
  %reldir%/Panel.cc \
  %reldir%/PopupMenuControl.cc \
  %reldir%/PushButtonControl.cc \
  %reldir%/PushTool.cc \
  %reldir%/QtHandlesUtils.cc \
  %reldir%/RadioButtonControl.cc \
  %reldir%/SliderControl.cc \
  %reldir%/Table.cc \
  %reldir%/TextControl.cc \
  %reldir%/TextEdit.cc \
  %reldir%/ToggleButtonControl.cc \
  %reldir%/ToggleTool.cc \
  %reldir%/ToolBar.cc \
  %reldir%/annotation-dialog.cc \
  %reldir%/gl-select.cc \
  %reldir%/qt-graphics-toolkit.cc

TEMPLATE_SRC = \
  %reldir%/ToolBarButton.cc

nodist_%canon_reldir%_libgraphics_la_SOURCES = $(libgraphics_MOC)

%canon_reldir%_libgraphics_la_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  $(FT2_CPPFLAGS) \
  $(FONTCONFIG_CPPFLAGS) \
  $(HDF5_CPPFLAGS) \
  @OCTGUI_DLL_DEFS@ \
  @QT_OPENGL_CPPFLAGS@ \
  -Ilibgui/graphics -I$(srcdir)/libgui/graphics \
  -Ilibgui/src -I$(srcdir)/libgui/src \
  -Iliboctave \
  -I$(srcdir)/liboctave/array \
  -Iliboctave/numeric -I$(srcdir)/liboctave/numeric \
  -Iliboctave/operators -I$(srcdir)/liboctave/operators \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util \
  -Ilibinterp -I$(srcdir)/libinterp \
  -Ilibinterp/parse-tree -I$(srcdir)/libinterp/parse-tree \
  -Ilibinterp/corefcn -I$(srcdir)/libinterp/corefcn \
  -I$(srcdir)/libinterp/octave-value

noinst_LTLIBRARIES += %reldir%/libgraphics.la

libgui_EXTRA_DIST += \
  $(libgraphics_UI)

libgui_CLEANFILES += \
  $(GRAPHICS_OCT_FILES) \
  $(GRAPHICS_PKG_ADD_FILE) \
  $(libgraphics_MOC) \
  $(libgraphics_MOC_H) \
  $(libgraphics_UI_H)

endif
