FCN_FILE_DIRS += scripts/deprecated

%canon_reldir%_FCN_FILES = \
  %reldir%/chop.m \
  %reldir%/comma.m \
  %reldir%/desktop.m \
  %reldir%/java2mat.m \
  %reldir%/paren.m \
  %reldir%/semicolon.m \
  %reldir%/tmpnam.m \
  %reldir%/toascii.m

%canon_reldir%dir = $(fcnfiledir)/deprecated

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
