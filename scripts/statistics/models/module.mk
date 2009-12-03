FCN_FILE_DIRS += statistics/models

statistics_models_FCN_FILES = \
  statistics/models/logistic_regression.m \
  statistics/models/logistic_regression_derivatives.m \
  statistics/models/logistic_regression_likelihood.m

FCN_FILES += $(statistics_models_FCN_FILES)

PKG_ADD_FILES += statistics/models/PKG_ADD

DIRSTAMP_FILES += statistics/models/$(octave_dirstamp)
