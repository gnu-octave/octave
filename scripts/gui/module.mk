FCN_FILE_DIRS += \
  scripts/gui \
  scripts/gui/private

scripts_gui_PRIVATE_FCN_FILES = \
  scripts/gui/private/__file_filter__.m \
  scripts/gui/private/__fltk_file_filter__.m \
  scripts/gui/private/__get_funcname__.m \
  scripts/gui/private/__is_function__.m \
  scripts/gui/private/__uigetdir_fltk__.m \
  scripts/gui/private/__uigetfile_fltk__.m \
  scripts/gui/private/__uiobject_split_args__.m \
  scripts/gui/private/__uiputfile_fltk__.m

scripts_gui_FCN_FILES = \
  scripts/gui/errordlg.m \
  scripts/gui/guidata.m \
  scripts/gui/guihandles.m \
  scripts/gui/helpdlg.m \
  scripts/gui/inputdlg.m \
  scripts/gui/listdlg.m \
  scripts/gui/msgbox.m \
  scripts/gui/questdlg.m \
  scripts/gui/uicontextmenu.m \
  scripts/gui/uicontrol.m \
  scripts/gui/uigetdir.m \
  scripts/gui/uigetfile.m \
  scripts/gui/uimenu.m \
  scripts/gui/uipanel.m \
  scripts/gui/uipushtool.m \
  scripts/gui/uiputfile.m \
  scripts/gui/uiresume.m \
  scripts/gui/uitoggletool.m \
  scripts/gui/uitoolbar.m \
  scripts/gui/uiwait.m \
  scripts/gui/waitbar.m \
  scripts/gui/waitforbuttonpress.m \
  scripts/gui/warndlg.m

scripts_guidir = $(fcnfiledir)/gui

scripts_gui_DATA = $(scripts_gui_FCN_FILES)

scripts_gui_privatedir = $(fcnfiledir)/gui/private

scripts_gui_private_DATA = $(scripts_gui_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_gui_FCN_FILES) \
  $(scripts_gui_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/gui/PKG_ADD

DIRSTAMP_FILES += scripts/gui/$(octave_dirstamp)
