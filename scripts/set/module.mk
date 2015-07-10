FCN_FILE_DIRS += scripts/set

scripts_set_FCN_FILES = \
  scripts/set/intersect.m \
  scripts/set/ismember.m \
  scripts/set/powerset.m \
  scripts/set/setdiff.m \
  scripts/set/setxor.m \
  scripts/set/union.m \
  scripts/set/unique.m \
  scripts/set/private/validsetargs.m

FCN_FILES += $(scripts_set_FCN_FILES)

PKG_ADD_FILES += scripts/set/PKG_ADD

DIRSTAMP_FILES += scripts/set/$(octave_dirstamp)
