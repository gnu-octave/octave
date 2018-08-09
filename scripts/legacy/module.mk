FCN_FILE_DIRS += scripts/legacy

%canon_reldir%_FCN_FILES = \
  %reldir%/findstr.m \
  %reldir%/flipdim.m \
  %reldir%/isequalwithequalnans.m \
  %reldir%/isstr.m \
  %reldir%/setstr.m \
  %reldir%/strmatch.m

%canon_reldir%dir = $(fcnfiledir)/legacy

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
