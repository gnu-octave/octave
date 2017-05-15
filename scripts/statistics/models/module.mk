FCN_FILE_DIRS += \
  scripts/statistics/models \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/logistic_regression_derivatives.m \
  %reldir%/private/logistic_regression_likelihood.m

%canon_reldir%_FCN_FILES = \
  %reldir%/logistic_regression.m

%canon_reldir%dir = $(fcnfiledir)/statistics/models

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/statistics/models/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
