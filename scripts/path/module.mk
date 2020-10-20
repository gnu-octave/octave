FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/getsavepath.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/import.m \
  %reldir%/matlabroot.m \
  %reldir%/pathdef.m \
  %reldir%/savepath.m

%canon_reldir%dir = $(fcnfiledir)/path

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/path/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
