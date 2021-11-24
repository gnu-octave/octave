FCN_FILE_DIRS += %reldir%

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/gallery.m \
  %reldir%/hadamard.m \
  %reldir%/hankel.m \
  %reldir%/hilb.m \
  %reldir%/invhilb.m \
  %reldir%/magic.m \
  %reldir%/pascal.m \
  %reldir%/rosser.m \
  %reldir%/toeplitz.m \
  %reldir%/vander.m \
  %reldir%/wilkinson.m

%canon_reldir%dir = $(fcnfiledir)/special-matrix

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
