FCN_FILE_DIRS += scripts/+containers

scripts_containers_FCN_FILES = \
  scripts/+containers/Map.m

scripts_containersdir = $(fcnfiledir)/strings

scripts_containers_DATA = $(scripts_containers_FCN_FILES)

FCN_FILES += $(scripts_containers_FCN_FILES)

PKG_ADD_FILES += scripts/+containers/PKG_ADD

DIRSTAMP_FILES += scripts/+containers/$(octave_dirstamp)
