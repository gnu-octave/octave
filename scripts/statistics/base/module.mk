FCN_FILE_DIRS += scripts/statistics/base

scripts_statistics_base_FCN_FILES = \
  scripts/statistics/base/center.m \
  scripts/statistics/base/cloglog.m \
  scripts/statistics/base/corr.m \
  scripts/statistics/base/cov.m \
  scripts/statistics/base/gls.m \
  scripts/statistics/base/histc.m \
  scripts/statistics/base/iqr.m \
  scripts/statistics/base/kendall.m \
  scripts/statistics/base/kurtosis.m \
  scripts/statistics/base/logit.m \
  scripts/statistics/base/lscov.m \
  scripts/statistics/base/mean.m \
  scripts/statistics/base/meansq.m \
  scripts/statistics/base/median.m \
  scripts/statistics/base/mode.m \
  scripts/statistics/base/moment.m \
  scripts/statistics/base/ols.m \
  scripts/statistics/base/ppplot.m \
  scripts/statistics/base/prctile.m \
  scripts/statistics/base/probit.m \
  scripts/statistics/base/qqplot.m \
  scripts/statistics/base/quantile.m \
  scripts/statistics/base/range.m \
  scripts/statistics/base/ranks.m \
  scripts/statistics/base/run_count.m \
  scripts/statistics/base/runlength.m \
  scripts/statistics/base/skewness.m \
  scripts/statistics/base/spearman.m \
  scripts/statistics/base/statistics.m \
  scripts/statistics/base/std.m \
  scripts/statistics/base/table.m \
  scripts/statistics/base/var.m \
  scripts/statistics/base/zscore.m

scripts_statistics_basedir = $(fcnfiledir)/statistics/base

scripts_statistics_base_DATA = $(scripts_statistics_base_FCN_FILES)

FCN_FILES += $(scripts_statistics_base_FCN_FILES)

PKG_ADD_FILES += scripts/statistics/base/PKG_ADD

DIRSTAMP_FILES += scripts/statistics/base/$(octave_dirstamp)
