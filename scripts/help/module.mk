FCN_FILE_DIRS += \
  scripts/help \
  scripts/help/private

scripts_help_PRIVATE_FCN_FILES = \
  scripts/help/private/__additional_help_message__.m \
  scripts/help/private/__strip_html_tags__.m

scripts_help_FCN_FILES = \
  scripts/help/__gripe_missing_component__.m \
  scripts/help/__makeinfo__.m \
  scripts/help/__unimplemented__.m \
  scripts/help/ans.m \
  scripts/help/comma.m \
  scripts/help/doc.m \
  scripts/help/doc_cache_create.m \
  scripts/help/get_first_help_sentence.m \
  scripts/help/error_ids.m \
  scripts/help/help.m \
  scripts/help/lookfor.m \
  scripts/help/paren.m \
  scripts/help/print_usage.m \
  scripts/help/semicolon.m \
  scripts/help/type.m \
  scripts/help/warning_ids.m \
  scripts/help/which.m

scripts_helpdir = $(fcnfiledir)/help

scripts_help_DATA = $(scripts_help_FCN_FILES)

scripts_help_privatedir = $(fcnfiledir)/help/private

scripts_help_private_DATA = $(scripts_help_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_help_FCN_FILES) \
  $(scripts_help_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/help/PKG_ADD

DIRSTAMP_FILES += scripts/help/$(octave_dirstamp)
