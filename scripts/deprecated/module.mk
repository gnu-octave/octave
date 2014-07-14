FCN_FILE_DIRS += deprecated

deprecated_FCN_FILES = \
  deprecated/default_save_options.m \
  deprecated/gen_doc_cache.m \
  deprecated/interp1q.m \
  deprecated/isequalwithequalnans.m \
  deprecated/isstr.m \
  deprecated/java_convert_matrix.m \
  deprecated/java_debug.m \
  deprecated/java_invoke.m \
  deprecated/java_new.m \
  deprecated/java_unsigned_conversion.m \
  deprecated/javafields.m \
  deprecated/javamethods.m \
  deprecated/re_read_readline_init_file.m \
  deprecated/read_readline_init_file.m \
  deprecated/saving_history.m

FCN_FILES += $(deprecated_FCN_FILES)

PKG_ADD_FILES += deprecated/PKG_ADD

DIRSTAMP_FILES += deprecated/$(octave_dirstamp)
