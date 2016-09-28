FCN_FILE_DIRS += scripts/special-matrix

scripts_special_matrix_FCN_FILES = \
  scripts/special-matrix/gallery.m \
  scripts/special-matrix/hadamard.m \
  scripts/special-matrix/hankel.m \
  scripts/special-matrix/hilb.m \
  scripts/special-matrix/invhilb.m \
  scripts/special-matrix/magic.m \
  scripts/special-matrix/pascal.m \
  scripts/special-matrix/rosser.m \
  scripts/special-matrix/toeplitz.m \
  scripts/special-matrix/vander.m \
  scripts/special-matrix/wilkinson.m

scripts_special_matrixdir = $(fcnfiledir)/special-matrix

scripts_special_matrix_DATA = $(scripts_special_matrix_FCN_FILES)

FCN_FILES += $(scripts_special_matrix_FCN_FILES)

PKG_ADD_FILES += scripts/special-matrix/PKG_ADD

DIRSTAMP_FILES += scripts/special-matrix/$(octave_dirstamp)
