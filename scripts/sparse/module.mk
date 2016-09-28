FCN_FILE_DIRS += \
  scripts/sparse \
  scripts/sparse/private

scripts_sparse_PRIVATE_FCN_FILES = \
  scripts/sparse/private/__sprand__.m

scripts_sparse_FCN_FILES = \
  scripts/sparse/bicg.m \
  scripts/sparse/bicgstab.m \
  scripts/sparse/cgs.m \
  scripts/sparse/colperm.m \
  scripts/sparse/eigs.m \
  scripts/sparse/etreeplot.m \
  scripts/sparse/gmres.m \
  scripts/sparse/gplot.m \
  scripts/sparse/ichol.m \
  scripts/sparse/ilu.m \
  scripts/sparse/nonzeros.m \
  scripts/sparse/pcg.m \
  scripts/sparse/pcr.m \
  scripts/sparse/qmr.m \
  scripts/sparse/spaugment.m \
  scripts/sparse/spconvert.m \
  scripts/sparse/spdiags.m \
  scripts/sparse/speye.m \
  scripts/sparse/spfun.m \
  scripts/sparse/spones.m \
  scripts/sparse/sprand.m \
  scripts/sparse/sprandn.m \
  scripts/sparse/sprandsym.m \
  scripts/sparse/spstats.m \
  scripts/sparse/spy.m \
  scripts/sparse/svds.m \
  scripts/sparse/treelayout.m \
  scripts/sparse/treeplot.m

scripts_sparsedir = $(fcnfiledir)/sparse

scripts_sparse_DATA = $(scripts_sparse_FCN_FILES)

scripts_sparse_privatedir = $(fcnfiledir)/sparse/private

scripts_sparse_private_DATA = $(scripts_sparse_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_sparse_FCN_FILES) \
  $(scripts_sparse_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/sparse/PKG_ADD

DIRSTAMP_FILES += scripts/sparse/$(octave_dirstamp)
