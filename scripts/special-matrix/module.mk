FCN_FILE_DIRS += scripts/special-matrix

scripts_special_matrix_FCN_FILES = \
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

scripts_special_matrix_DATA = $(scripts_special_matrix_FCN_FILES)

FCN_FILES += $(scripts_special_matrix_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
