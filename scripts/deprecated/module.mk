FCN_FILE_DIRS += deprecated

deprecated_FCN_FILES = \
  deprecated/bicubic.m \
  deprecated/delaunay3.m \
  deprecated/dump_prefs.m \
  deprecated/find_dir_in_path.m \
  deprecated/finite.m \
  deprecated/fmod.m \
  deprecated/fnmatch.m \
  deprecated/gmap40.m \
  deprecated/isstr.m \
  deprecated/loadaudio.m \
  deprecated/luinc.m \
  deprecated/mouse_wheel_zoom.m \
  deprecated/nfields.m \
  deprecated/octave_tmp_file_name.m \
  deprecated/playaudio.m \
  deprecated/saveaudio.m \
  deprecated/setaudio.m \
  deprecated/syl.m \
  deprecated/usage.m

FCN_FILES += $(deprecated_FCN_FILES)

PKG_ADD_FILES += deprecated/PKG_ADD

DIRSTAMP_FILES += deprecated/$(octave_dirstamp)
