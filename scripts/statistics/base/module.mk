FCN_FILE_DIRS += scripts/statistics/base

%canon_reldir%_FCN_FILES = \
  %reldir%/center.m \
  %reldir%/cloglog.m \
  %reldir%/corr.m \
  %reldir%/corrcoef.m \
  %reldir%/cov.m \
  %reldir%/gls.m \
  %reldir%/histc.m \
  %reldir%/iqr.m \
  %reldir%/kendall.m \
  %reldir%/kurtosis.m \
  %reldir%/logit.m \
  %reldir%/lscov.m \
  %reldir%/mean.m \
  %reldir%/meansq.m \
  %reldir%/median.m \
  %reldir%/mode.m \
  %reldir%/moment.m \
  %reldir%/ols.m \
  %reldir%/ppplot.m \
  %reldir%/prctile.m \
  %reldir%/probit.m \
  %reldir%/qqplot.m \
  %reldir%/quantile.m \
  %reldir%/range.m \
  %reldir%/ranks.m \
  %reldir%/run_count.m \
  %reldir%/runlength.m \
  %reldir%/skewness.m \
  %reldir%/spearman.m \
  %reldir%/statistics.m \
  %reldir%/std.m \
  %reldir%/table.m \
  %reldir%/var.m \
  %reldir%/zscore.m

%canon_reldir%dir = $(fcnfiledir)/statistics/base

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
