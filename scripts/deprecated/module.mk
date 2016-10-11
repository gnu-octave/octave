FCN_FILE_DIRS += scripts/deprecated

scripts_deprecated_FCN_FILES = \
  scripts/deprecated/bitmax.m \
  scripts/deprecated/comma.m \
  scripts/deprecated/isstr.m \
  scripts/deprecated/mahalanobis.m \
  scripts/deprecated/md5sum.m \
  scripts/deprecated/octave_config_info.m \
  scripts/deprecated/onenormest.m \
  scripts/deprecated/paren.m \
  scripts/deprecated/semicolon.m \
  scripts/deprecated/sleep.m \
  scripts/deprecated/usleep.m \
  scripts/deprecated/wavread.m \
  scripts/deprecated/wavwrite.m

scripts_deprecateddir = $(fcnfiledir)/deprecated

scripts_deprecated_DATA = $(scripts_deprecated_FCN_FILES)

FCN_FILES += $(scripts_deprecated_FCN_FILES)

PKG_ADD_FILES += scripts/deprecated/PKG_ADD

DIRSTAMP_FILES += scripts/deprecated/$(octave_dirstamp)
