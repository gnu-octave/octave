FCN_FILE_DIRS += scripts/statistics/tests

scripts_statistics_tests_FCN_FILES = \
  scripts/statistics/tests/anova.m \
  scripts/statistics/tests/bartlett_test.m \
  scripts/statistics/tests/chisquare_test_homogeneity.m \
  scripts/statistics/tests/chisquare_test_independence.m \
  scripts/statistics/tests/cor_test.m \
  scripts/statistics/tests/f_test_regression.m \
  scripts/statistics/tests/hotelling_test.m \
  scripts/statistics/tests/hotelling_test_2.m \
  scripts/statistics/tests/kolmogorov_smirnov_test.m \
  scripts/statistics/tests/kolmogorov_smirnov_test_2.m \
  scripts/statistics/tests/kruskal_wallis_test.m \
  scripts/statistics/tests/manova.m \
  scripts/statistics/tests/mcnemar_test.m \
  scripts/statistics/tests/prop_test_2.m \
  scripts/statistics/tests/run_test.m \
  scripts/statistics/tests/sign_test.m \
  scripts/statistics/tests/t_test.m \
  scripts/statistics/tests/t_test_2.m \
  scripts/statistics/tests/t_test_regression.m \
  scripts/statistics/tests/u_test.m \
  scripts/statistics/tests/var_test.m \
  scripts/statistics/tests/welch_test.m \
  scripts/statistics/tests/wilcoxon_test.m \
  scripts/statistics/tests/z_test.m \
  scripts/statistics/tests/z_test_2.m

scripts_statistics_testsdir = $(fcnfiledir)/statistics/tests

scripts_statistics_tests_DATA = $(scripts_statistics_tests_FCN_FILES)

FCN_FILES += $(scripts_statistics_tests_FCN_FILES)

PKG_ADD_FILES += scripts/statistics/tests/PKG_ADD

DIRSTAMP_FILES += scripts/statistics/tests/$(octave_dirstamp)
