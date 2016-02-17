OCTAVE_GUI_GRAPHICS_MOC = \
  libgui/graphics/moc-annotation-dialog.cc \
  libgui/graphics/moc-Backend.cc \
  libgui/graphics/moc-ButtonControl.cc \
  libgui/graphics/moc-ContextMenu.cc \
  libgui/graphics/moc-EditControl.cc \
  libgui/graphics/moc-Figure.cc \
  libgui/graphics/moc-FigureWindow.cc \
  libgui/graphics/moc-ListBoxControl.cc \
  libgui/graphics/moc-Menu.cc \
  libgui/graphics/moc-MouseModeActionGroup.cc \
  libgui/graphics/moc-Object.cc \
  libgui/graphics/moc-ObjectFactory.cc \
  libgui/graphics/moc-ObjectProxy.cc \
  libgui/graphics/moc-PopupMenuControl.cc \
  libgui/graphics/moc-PushTool.cc \
  libgui/graphics/moc-SliderControl.cc \
  libgui/graphics/moc-TextEdit.cc \
  libgui/graphics/moc-ToggleTool.cc \
  libgui/graphics/moc-ToolBar.cc

$(OCTAVE_GUI_GRAPHICS_MOC): | libgui/graphics/$(octave_dirstamp)

DIRSTAMP_FILES += \
  libgui/graphics/$(octave_dirstamp)

octave_gui_MOC += \
  $(OCTAVE_GUI_GRAPHICS_MOC)

octave_gui_graphics_UI = \
  libgui/graphics/annotation-dialog.ui

octave_gui_graphics_UI_H = $(patsubst libgui/graphics/%.ui, libgui/graphics/ui-%.h, $(octave_gui_graphics_UI))

$(octave_gui_graphics_UI_H): | libgui/graphics/$(octave_dirstamp)

BUILT_SOURCES += $(octave_gui_graphics_UI_H)

octave_gui_graphics_RC = libgui/graphics/qrc-qthandles.cc

$(octave_gui_graphics_RC): | libgui/graphics/$(octave_dirstamp)

noinst_HEADERS += \
  libgui/graphics/__init_qt__.h \
  libgui/graphics/annotation-dialog.h \
  libgui/graphics/Backend.h \
  libgui/graphics/BaseControl.h \
  libgui/graphics/ButtonControl.h \
  libgui/graphics/Canvas.h \
  libgui/graphics/CheckBoxControl.h \
  libgui/graphics/Container.h \
  libgui/graphics/ContextMenu.h \
  libgui/graphics/EditControl.h \
  libgui/graphics/Figure.h \
  libgui/graphics/FigureWindow.h \
  libgui/graphics/GenericEventNotify.h \
  libgui/graphics/GLCanvas.h \
  libgui/graphics/KeyMap.h \
  libgui/graphics/ListBoxControl.h \
  libgui/graphics/Logger.h \
  libgui/graphics/Menu.h \
  libgui/graphics/MenuContainer.h \
  libgui/graphics/MouseModeActionGroup.h \
  libgui/graphics/Object.h \
  libgui/graphics/ObjectFactory.h \
  libgui/graphics/ObjectProxy.h \
  libgui/graphics/Panel.h \
  libgui/graphics/PopupMenuControl.h \
  libgui/graphics/PushButtonControl.h \
  libgui/graphics/PushTool.h \
  libgui/graphics/QtHandlesUtils.h \
  libgui/graphics/RadioButtonControl.h \
  libgui/graphics/SliderControl.h \
  libgui/graphics/TextControl.h \
  libgui/graphics/TextEdit.h \
  libgui/graphics/ToggleButtonControl.h \
  libgui/graphics/ToggleTool.h \
  libgui/graphics/ToolBar.h \
  libgui/graphics/ToolBarButton.h \
  libgui/graphics/gl-select.h \
  $(TEMPLATE_SRC)

libgui_graphics_libgui_graphics_la_SOURCES = \
  libgui/graphics/__init_qt__.cc \
  libgui/graphics/annotation-dialog.cc \
  libgui/graphics/Backend.cc \
  libgui/graphics/BaseControl.cc \
  libgui/graphics/ButtonControl.cc \
  libgui/graphics/Canvas.cc \
  libgui/graphics/CheckBoxControl.cc \
  libgui/graphics/Container.cc \
  libgui/graphics/ContextMenu.cc \
  libgui/graphics/EditControl.cc \
  libgui/graphics/Figure.cc \
  libgui/graphics/FigureWindow.cc \
  libgui/graphics/GLCanvas.cc \
  libgui/graphics/KeyMap.cc \
  libgui/graphics/ListBoxControl.cc \
  libgui/graphics/Logger.cc \
  libgui/graphics/Menu.cc \
  libgui/graphics/MouseModeActionGroup.cc \
  libgui/graphics/Object.cc \
  libgui/graphics/ObjectFactory.cc \
  libgui/graphics/ObjectProxy.cc \
  libgui/graphics/Panel.cc \
  libgui/graphics/PopupMenuControl.cc \
  libgui/graphics/PushButtonControl.cc \
  libgui/graphics/PushTool.cc \
  libgui/graphics/QtHandlesUtils.cc \
  libgui/graphics/RadioButtonControl.cc \
  libgui/graphics/SliderControl.cc \
  libgui/graphics/TextControl.cc \
  libgui/graphics/TextEdit.cc \
  libgui/graphics/ToggleButtonControl.cc \
  libgui/graphics/ToggleTool.cc \
  libgui/graphics/ToolBar.cc \
  libgui/graphics/gl-select.cc

TEMPLATE_SRC = \
  libgui/graphics/ToolBarButton.cc

nodist_libgui_graphics_libgui_graphics_la_SOURCES = $(octave_gui_graphics_MOC) $(octave_gui_graphics_RC)

libgui_graphics_libgui_graphics_la_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  $(FT2_CPPFLAGS) \
  $(FONTCONFIG_CPPFLAGS) \
  $(HDF5_CPPFLAGS) \
  @OCTGUI_DLL_DEFS@ \
  @QT_CPPFLAGS@ \
  -Ilibgui/graphics -I$(srcdir)/libgui/graphics \
  -Isrc -I$(srcdir)/libgui/src \
  -I$(srcdir)/liboctave/cruft/misc \
  -I$(srcdir)/liboctave/array \
  -Iliboctave/numeric -I$(srcdir)/liboctave/numeric \
  -Iliboctave/operators -I$(srcdir)/liboctave/operators \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util \
  -Ilibinterp -I$(srcdir)/libinterp \
  -Ilibinterp/parse-tree -I$(srcdir)/libinterp/parse-tree \
  -Ilibinterp/corefcn -I$(srcdir)/libinterp/corefcn \
  -I$(srcdir)/libinterp/octave-value

libgui_graphics_libgui_graphics_la_CFLAGS = $(AM_CFLAGS) $(WARN_CFLAGS)

libgui_graphics_libgui_graphics_la_CXXFLAGS = $(AM_CXXFLAGS) $(WARN_CXXFLAGS)

noinst_LTLIBRARIES += libgui/graphics/libgui-graphics.la

libgui_EXTRA_DIST += \
  libgui/graphics/qthandles.qrc \
  libgui/graphics/images/README \
  libgui/graphics/images/pan.png \
  libgui/graphics/images/rotate.png \
  libgui/graphics/images/select.png \
  libgui/graphics/images/zoom-in.png \
  libgui/graphics/images/zoom-out.png \
  $(octave_gui_graphics_UI)

libgui_CLEANFILES += \
  $(octave_gui_graphics_MOC) \
  $(octave_gui_graphics_RC) \
  $(octave_gui_graphics_UI_H)
