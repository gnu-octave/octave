FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__file_filter__.m \
  %reldir%/private/__fltk_file_filter__.m \
  %reldir%/private/__get_funcname__.m \
  %reldir%/private/__is_function__.m \
  %reldir%/private/__ok_cancel_dlg__.m \
  %reldir%/private/__uigetdir_fltk__.m \
  %reldir%/private/__uigetfile_fltk__.m \
  %reldir%/private/__uiobject_split_args__.m \
  %reldir%/private/__uiputfile_fltk__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/dialog.m \
  %reldir%/errordlg.m \
  %reldir%/getappdata.m \
  %reldir%/getpixelposition.m \
  %reldir%/guidata.m \
  %reldir%/guihandles.m \
  %reldir%/helpdlg.m \
  %reldir%/inputdlg.m \
  %reldir%/isappdata.m \
  %reldir%/listfonts.m \
  %reldir%/listdlg.m \
  %reldir%/movegui.m \
  %reldir%/msgbox.m \
  %reldir%/questdlg.m \
  %reldir%/rmappdata.m \
  %reldir%/setappdata.m \
  %reldir%/uibuttongroup.m \
  %reldir%/uicontextmenu.m \
  %reldir%/uicontrol.m \
  %reldir%/uigetdir.m \
  %reldir%/uigetfile.m \
  %reldir%/uifigure.m \
  %reldir%/uimenu.m \
  %reldir%/uipanel.m \
  %reldir%/uipushtool.m \
  %reldir%/uiputfile.m \
  %reldir%/uiresume.m \
  %reldir%/uisetfont.m \
  %reldir%/uitable.m \
  %reldir%/uitoggletool.m \
  %reldir%/uitoolbar.m \
  %reldir%/uiwait.m \
  %reldir%/waitbar.m \
  %reldir%/waitforbuttonpress.m \
  %reldir%/warndlg.m

%canon_reldir%dir = $(fcnfiledir)/gui

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/gui/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
