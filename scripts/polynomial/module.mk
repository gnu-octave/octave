FCN_FILE_DIRS += polynomial

polynomial_FCN_FILES = \
  polynomial/compan.m \
  polynomial/conv.m \
  polynomial/convn.m \
  polynomial/deconv.m \
  polynomial/mkpp.m \
  polynomial/mpoles.m \
  polynomial/pchip.m \
  polynomial/poly.m \
  polynomial/polyaffine.m \
  polynomial/polyder.m \
  polynomial/polyderiv.m \
  polynomial/polyfit.m \
  polynomial/polygcd.m \
  polynomial/polyint.m \
  polynomial/polyout.m \
  polynomial/polyreduce.m \
  polynomial/polyval.m \
  polynomial/polyvalm.m \
  polynomial/ppval.m \
  polynomial/ppval.m \
  polynomial/ppint.m \
  polynomial/ppjumps.m \
  polynomial/residue.m \
  polynomial/roots.m \
  polynomial/spline.m \
  polynomial/unmkpp.m

FCN_FILES += $(polynomial_FCN_FILES)

PKG_ADD_FILES += polynomial/PKG_ADD

DIRSTAMP_FILES += polynomial/$(octave_dirstamp)
