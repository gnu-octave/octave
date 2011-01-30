FCN_FILE_DIRS += deprecated

deprecated_FCN_FILES = \
  deprecated/autocor.m \
  deprecated/autocov.m \
  deprecated/betai.m \
  deprecated/cellidx.m \
  deprecated/clg.m \
  deprecated/complement.m \
  deprecated/create_set.m \
  deprecated/dispatch.m \
  deprecated/dmult.m \
  deprecated/fstat.m \
  deprecated/gammai.m \
  deprecated/glpkmex.m \
  deprecated/intwarning.m \
  deprecated/iscommand.m \
  deprecated/is_global.m \
  deprecated/israwcommand.m \
  deprecated/isstr.m \
  deprecated/lchol.m \
  deprecated/loadimage.m \
  deprecated/krylovb.m \
  deprecated/mark_as_command.m \
  deprecated/mark_as_rawcommand.m \
  deprecated/replot.m \
  deprecated/saveimage.m \
  deprecated/setstr.m \
  deprecated/spatan2.m \
  deprecated/spchol2inv.m \
  deprecated/spcholinv.m \
  deprecated/spchol.m \
  deprecated/spcumprod.m \
  deprecated/spcumsum.m \
  deprecated/spdet.m \
  deprecated/spdiag.m \
  deprecated/spfind.m \
  deprecated/sphcat.m \
  deprecated/spinv.m \
  deprecated/spkron.m \
  deprecated/splchol.m \
  deprecated/split.m \
  deprecated/splu.m \
  deprecated/spmax.m \
  deprecated/spmin.m \
  deprecated/spprod.m \
  deprecated/spqr.m \
  deprecated/spsum.m \
  deprecated/spsumsq.m \
  deprecated/spvcat.m \
  deprecated/str2mat.m \
  deprecated/unmark_command.m \
  deprecated/unmark_rawcommand.m \
  deprecated/values.m \
  deprecated/weibcdf.m \
  deprecated/weibinv.m \
  deprecated/weibpdf.m \
  deprecated/weibrnd.m

FCN_FILES += $(deprecated_FCN_FILES)

PKG_ADD_FILES += deprecated/PKG_ADD

DIRSTAMP_FILES += deprecated/$(octave_dirstamp)
