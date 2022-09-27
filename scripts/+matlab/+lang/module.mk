FCN_FILE_DIRS += %reldir%

%canon_reldir%_FCN_FILES = \
  %reldir%/makeUniqueStrings.m \
  %reldir%/makeValidName.m \
  %reldir%/MemoizedFunction.m

%canon_reldir%dir = $(fcnfiledir)/+matlab/+lang

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
