FCN_FILE_DIRS += \
  scripts/set \
  scripts/set/private

scripts_set_PRIVATE_FCN_FILES = scripts/set/private/validsetargs.m

scripts_set_FCN_FILES = \
  scripts/set/intersect.m \
  scripts/set/ismember.m \
  scripts/set/powerset.m \
  scripts/set/setdiff.m \
  scripts/set/setxor.m \
  scripts/set/union.m \
  scripts/set/unique.m

scripts_setdir = $(fcnfiledir)/set

scripts_set_DATA = $(scripts_set_FCN_FILES)

scripts_set_privatedir = $(fcnfiledir)/set/private

scripts_set_private_DATA = $(scripts_set_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_set_FCN_FILES) \
  $(scripts_set_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/set/PKG_ADD

DIRSTAMP_FILES += scripts/set/$(octave_dirstamp)
