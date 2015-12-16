FCN_FILE_DIRS += scripts/deprecated

scripts_deprecated_FCN_FILES = \
  scripts/deprecated/bicubic.m \
  scripts/deprecated/bitmax.m \
  scripts/deprecated/delaunay3.m \
  scripts/deprecated/dump_prefs.m \
  scripts/deprecated/find_dir_in_path.m \
  scripts/deprecated/finite.m \
  scripts/deprecated/fmod.m \
  scripts/deprecated/fnmatch.m \
  scripts/deprecated/gmap40.m \
  scripts/deprecated/isstr.m \
  scripts/deprecated/loadaudio.m \
  scripts/deprecated/luinc.m \
  scripts/deprecated/mahalanobis.m \
  scripts/deprecated/md5sum.m \
  scripts/deprecated/mouse_wheel_zoom.m \
  scripts/deprecated/nfields.m \
  scripts/deprecated/octave_tmp_file_name.m \
  scripts/deprecated/playaudio.m \
  scripts/deprecated/saveaudio.m \
  scripts/deprecated/setaudio.m \
  scripts/deprecated/syl.m \
  scripts/deprecated/usage.m \
  scripts/deprecated/wavread.m \
  scripts/deprecated/wavwrite.m

scripts_deprecateddir = $(fcnfiledir)/deprecated

scripts_deprecated_DATA = $(scripts_deprecated_FCN_FILES)

FCN_FILES += $(scripts_deprecated_FCN_FILES)

PKG_ADD_FILES += scripts/deprecated/PKG_ADD

DIRSTAMP_FILES += scripts/deprecated/$(octave_dirstamp)
