FCN_FILE_DIRS += scripts/polynomial

scripts_polynomial_PRIVATE_FCN_FILES = \
  scripts/polynomial/private/__splinefit__.m

scripts_polynomial_FCN_FILES = \
  scripts/polynomial/compan.m \
  scripts/polynomial/conv.m \
  scripts/polynomial/deconv.m \
  scripts/polynomial/mkpp.m \
  scripts/polynomial/mpoles.m \
  scripts/polynomial/pchip.m \
  scripts/polynomial/poly.m \
  scripts/polynomial/polyaffine.m \
  scripts/polynomial/polyder.m \
  scripts/polynomial/polyeig.m \
  scripts/polynomial/polyfit.m \
  scripts/polynomial/polygcd.m \
  scripts/polynomial/polyint.m \
  scripts/polynomial/polyout.m \
  scripts/polynomial/polyreduce.m \
  scripts/polynomial/polyval.m \
  scripts/polynomial/polyvalm.m \
  scripts/polynomial/ppval.m \
  scripts/polynomial/ppder.m \
  scripts/polynomial/ppint.m \
  scripts/polynomial/ppjumps.m \
  scripts/polynomial/residue.m \
  scripts/polynomial/roots.m \
  scripts/polynomial/spline.m \
  scripts/polynomial/splinefit.m \
  scripts/polynomial/unmkpp.m \
  $(scripts_polynomial_PRIVATE_FCN_FILES)

FCN_FILES += $(scripts_polynomial_FCN_FILES)

PKG_ADD_FILES += scripts/polynomial/PKG_ADD

DIRSTAMP_FILES += scripts/polynomial/$(octave_dirstamp)
