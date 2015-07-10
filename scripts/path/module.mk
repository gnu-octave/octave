FCN_FILE_DIRS += scripts/path

scripts_path_PRIVATE_FCN_FILES = \
  scripts/path/private/getsavepath.m

scripts_path_FCN_FILES = \
  scripts/path/matlabroot.m \
  scripts/path/pathdef.m \
  scripts/path/savepath.m \
  $(scripts_path_PRIVATE_FCN_FILES)

FCN_FILES += $(scripts_path_FCN_FILES)

PKG_ADD_FILES += scripts/path/PKG_ADD

DIRSTAMP_FILES += scripts/path/$(octave_dirstamp)
