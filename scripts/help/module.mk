FCN_FILE_DIRS += help

help_PRIVATE_FCN_FILES = \
  help/private/__additional_help_message__.m

help_FCN_FILES = \
  help/__makeinfo__.m \
  help/__strip_html_tags__.m \
  help/doc.m \
  help/gen_doc_cache.m \
  help/get_first_help_sentence.m \
  help/help.m \
  help/lookfor.m \
  help/print_usage.m \
  help/type.m \
  help/which.m \
  $(help_PRIVATE_FCN_FILES)

FCN_FILES += $(help_FCN_FILES)

PKG_ADD_FILES += help/PKG_ADD

DIRSTAMP_FILES += help/$(octave_dirstamp)
