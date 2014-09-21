FCN_FILE_DIRS += deprecated

deprecated_FCN_FILES = \
  deprecated/bicubic.m \
  deprecated/find_dir_in_path.m \
  deprecated/finite.m \
  deprecated/flipdim.m \
  deprecated/fmod.m \
  deprecated/fnmatch.m \
  deprecated/isstr.m \
  deprecated/nfields.m \
  deprecated/strmatch.m \
  deprecated/syl.m \
  deprecated/usage.m

FCN_FILES += $(deprecated_FCN_FILES)

PKG_ADD_FILES += deprecated/PKG_ADD

DIRSTAMP_FILES += deprecated/$(octave_dirstamp)
