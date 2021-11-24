FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__alltohandles__.m \
  %reldir%/private/__default__input__.m \
  %reldir%/private/__sprand__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/bicg.m \
  %reldir%/bicgstab.m \
  %reldir%/cgs.m \
  %reldir%/colperm.m \
  %reldir%/eigs.m \
  %reldir%/etreeplot.m \
  %reldir%/gmres.m \
  %reldir%/gplot.m \
  %reldir%/ichol.m \
  %reldir%/ilu.m \
  %reldir%/nonzeros.m \
  %reldir%/pcg.m \
  %reldir%/pcr.m \
  %reldir%/qmr.m \
  %reldir%/spaugment.m \
  %reldir%/spconvert.m \
  %reldir%/spdiags.m \
  %reldir%/speye.m \
  %reldir%/spfun.m \
  %reldir%/spones.m \
  %reldir%/sprand.m \
  %reldir%/sprandn.m \
  %reldir%/sprandsym.m \
  %reldir%/spstats.m \
  %reldir%/spy.m \
  %reldir%/svds.m \
  %reldir%/tfqmr.m \
  %reldir%/treelayout.m \
  %reldir%/treeplot.m

%canon_reldir%dir = $(fcnfiledir)/sparse

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/sparse/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
