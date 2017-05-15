FCN_FILE_DIRS += \
  scripts/set \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = %reldir%/private/validsetargs.m

%canon_reldir%_FCN_FILES = \
  %reldir%/intersect.m \
  %reldir%/ismember.m \
  %reldir%/powerset.m \
  %reldir%/setdiff.m \
  %reldir%/setxor.m \
  %reldir%/union.m \
  %reldir%/unique.m

%canon_reldir%dir = $(fcnfiledir)/set

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/set/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
