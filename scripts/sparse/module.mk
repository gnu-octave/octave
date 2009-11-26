FCN_FILE_DIRS += sparse

sparse_FCN_FILES = \
  sparse/bicgstab.m \
  sparse/cgs.m \
  sparse/colperm.m \
  sparse/etreeplot.m \
  sparse/gplot.m \
  sparse/nonzeros.m \
  sparse/pcg.m \
  sparse/pcr.m \
  sparse/spalloc.m \
  sparse/spaugment.m \
  sparse/spconvert.m \
  sparse/spdiags.m \
  sparse/speye.m \
  sparse/spfun.m \
  sparse/sphcat.m \
  sparse/spones.m \
  sparse/sprand.m \
  sparse/sprandn.m \
  sparse/sprandsym.m \
  sparse/spstats.m \
  sparse/spvcat.m \
  sparse/spy.m \
  sparse/svds.m \
  sparse/treelayout.m \
  sparse/treeplot.m

FCN_FILES += $(sparse_FCN_FILES)

PKG_ADD_FILES += sparse/PKG_ADD
