FCN_FILE_DIRS += path

path_FCN_FILES = \
  path/__extractpath__.m \
  path/matlabroot.m \
  path/pathdef.m \
  path/savepath.m

FCN_FILES += $(path_FCN_FILES)

PKG_ADD_FILES += path/PKG_ADD
