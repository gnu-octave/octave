FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__additional_help_message__.m \
  %reldir%/private/__strip_html_tags__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/__gripe_missing_component__.m \
  %reldir%/__makeinfo__.m \
  %reldir%/__unimplemented__.m \
  %reldir%/ans.m \
  %reldir%/bessel.m \
  %reldir%/debug.m \
  %reldir%/doc.m \
  %reldir%/doc_cache_create.m \
  %reldir%/error_ids.m \
  %reldir%/get_first_help_sentence.m \
  %reldir%/help.m \
  %reldir%/lookfor.m \
  %reldir%/print_usage.m \
  %reldir%/slash.m \
  %reldir%/type.m \
  %reldir%/warning_ids.m \
  %reldir%/which.m

%canon_reldir%dir = $(fcnfiledir)/help

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/help/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
