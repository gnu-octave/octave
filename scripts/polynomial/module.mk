FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__splinefit__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/compan.m \
  %reldir%/conv.m \
  %reldir%/deconv.m \
  %reldir%/mkpp.m \
  %reldir%/mpoles.m \
  %reldir%/padecoef.m \
  %reldir%/pchip.m \
  %reldir%/poly.m \
  %reldir%/polyaffine.m \
  %reldir%/polyder.m \
  %reldir%/polyeig.m \
  %reldir%/polyfit.m \
  %reldir%/polygcd.m \
  %reldir%/polyint.m \
  %reldir%/polyout.m \
  %reldir%/polyreduce.m \
  %reldir%/polyval.m \
  %reldir%/polyvalm.m \
  %reldir%/ppder.m \
  %reldir%/ppint.m \
  %reldir%/ppjumps.m \
  %reldir%/ppval.m \
  %reldir%/residue.m \
  %reldir%/roots.m \
  %reldir%/spline.m \
  %reldir%/splinefit.m \
  %reldir%/unmkpp.m

%canon_reldir%dir = $(fcnfiledir)/polynomial

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/polynomial/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
