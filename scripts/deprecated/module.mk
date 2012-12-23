FCN_FILE_DIRS += deprecated

deprecated_FCN_FILES = \
  deprecated/__error_text__.m \
  deprecated/cor.m \
  deprecated/corrcoef.m \
  deprecated/cut.m \
  deprecated/java_debug.m \
  deprecated/error_text.m \
  deprecated/isstr.m \
  deprecated/java_convert_matrix.m \
  deprecated/java_get.m \
  deprecated/java_invoke.m \
  deprecated/java_new.m \
  deprecated/java_unsigned_conversion.m \
  deprecated/java_set.m \
  deprecated/javafields.m \
  deprecated/javamethods.m \
  deprecated/polyderiv.m \
  deprecated/setstr.m \
  deprecated/shell_cmd.m \
  deprecated/studentize.m \
  deprecated/sylvester_matrix.m

FCN_FILES += $(deprecated_FCN_FILES)

PKG_ADD_FILES += deprecated/PKG_ADD

DIRSTAMP_FILES += deprecated/$(octave_dirstamp)
