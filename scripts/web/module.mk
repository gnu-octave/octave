FCN_FILE_DIRS += %reldir%

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/web.m \
  %reldir%/weboptions.m \
  %reldir%/webread.m \
  %reldir%/webwrite.m

%canon_reldir%dir = $(fcnfiledir)/web

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
