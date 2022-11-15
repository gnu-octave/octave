FCN_FILE_DIRS += %reldir%

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/disable_diagonal_matrix.m \
  %reldir%/disable_permutation_matrix.m \
  %reldir%/disable_range.m \
  %reldir%/shift.m \
  %reldir%/sparse_auto_mutate.m

%canon_reldir%dir = $(fcnfiledir)/deprecated

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
