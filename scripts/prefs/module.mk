FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/loadprefs.m \
  %reldir%/private/prefsfile.m \
  %reldir%/private/saveprefs.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/addpref.m \
  %reldir%/getpref.m \
  %reldir%/ispref.m \
  %reldir%/prefdir.m \
  %reldir%/preferences.m \
  %reldir%/rmpref.m \
  %reldir%/setpref.m

%canon_reldir%dir = $(fcnfiledir)/prefs

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/prefs/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
