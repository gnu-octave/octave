FCN_FILE_DIRS += \
  scripts/statistics/models \
  scripts/statistics/models/private

scripts_statistics_models_PRIVATE_FCN_FILES = \
  scripts/statistics/models/private/logistic_regression_derivatives.m \
  scripts/statistics/models/private/logistic_regression_likelihood.m

scripts_statistics_models_FCN_FILES = \
  scripts/statistics/models/logistic_regression.m

scripts_statistics_modelsdir = $(fcnfiledir)/statistics/models

scripts_statistics_models_DATA = $(scripts_statistics_models_FCN_FILES)

scripts_statistics_models_privatedir = $(fcnfiledir)/statistics/models/private

scripts_statistics_models_private_DATA = $(scripts_statistics_models_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_statistics_models_FCN_FILES) \
  $(scripts_statistics_models_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/statistics/models/PKG_ADD

DIRSTAMP_FILES += scripts/statistics/models/$(octave_dirstamp)
