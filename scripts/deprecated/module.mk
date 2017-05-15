FCN_FILE_DIRS += scripts/deprecated

%canon_reldir%_FCN_FILES = \
  %reldir%/bitmax.m \
  %reldir%/comma.m \
  %reldir%/isstr.m \
  %reldir%/mahalanobis.m \
  %reldir%/md5sum.m \
  %reldir%/octave_config_info.m \
  %reldir%/onenormest.m \
  %reldir%/paren.m \
  %reldir%/semicolon.m \
  %reldir%/sleep.m \
  %reldir%/usleep.m \
  %reldir%/wavread.m \
  %reldir%/wavwrite.m

%canon_reldir%dir = $(fcnfiledir)/deprecated

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
