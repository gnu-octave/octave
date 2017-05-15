FCN_FILE_DIRS += scripts/statistics/tests

%canon_reldir%_FCN_FILES = \
  %reldir%/anova.m \
  %reldir%/bartlett_test.m \
  %reldir%/chisquare_test_homogeneity.m \
  %reldir%/chisquare_test_independence.m \
  %reldir%/cor_test.m \
  %reldir%/f_test_regression.m \
  %reldir%/hotelling_test.m \
  %reldir%/hotelling_test_2.m \
  %reldir%/kolmogorov_smirnov_test.m \
  %reldir%/kolmogorov_smirnov_test_2.m \
  %reldir%/kruskal_wallis_test.m \
  %reldir%/manova.m \
  %reldir%/mcnemar_test.m \
  %reldir%/prop_test_2.m \
  %reldir%/run_test.m \
  %reldir%/sign_test.m \
  %reldir%/t_test.m \
  %reldir%/t_test_2.m \
  %reldir%/t_test_regression.m \
  %reldir%/u_test.m \
  %reldir%/var_test.m \
  %reldir%/welch_test.m \
  %reldir%/wilcoxon_test.m \
  %reldir%/z_test.m \
  %reldir%/z_test_2.m

%canon_reldir%dir = $(fcnfiledir)/statistics/tests

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
