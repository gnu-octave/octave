FCN_FILE_DIRS += deprecated

deprecated_FCN_FILES = \
  deprecated/autocor.m \
  deprecated/autocov.m \
  deprecated/betai.m \
  deprecated/cellidx.m \
  deprecated/clg.m \
  deprecated/cquad.m \
  deprecated/dispatch.m \
  deprecated/fstat.m \
  deprecated/gammai.m \
  deprecated/glpkmex.m \
  deprecated/intwarning.m \
  deprecated/is_duplicate_entry.m \
  deprecated/is_global.m \
  deprecated/isstr.m \
  deprecated/krylovb.m \
  deprecated/perror.m \
  deprecated/replot.m \
  deprecated/saveimage.m \
  deprecated/setstr.m \
  deprecated/sphcat.m \
  deprecated/spvcat.m \
  deprecated/strerror.m \
  deprecated/values.m \
  deprecated/weibcdf.m \
  deprecated/weibinv.m \
  deprecated/weibpdf.m \
  deprecated/weibrnd.m

FCN_FILES += $(deprecated_FCN_FILES)

PKG_ADD_FILES += deprecated/PKG_ADD

DIRSTAMP_FILES += deprecated/$(octave_dirstamp)
