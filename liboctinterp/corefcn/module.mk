COREFCN_INC = \
  $(COREFCN_NUMERIC_INC) \
  $(COREFCN_SYSTEM_INC) \
  $(COREFCN_UTIL_INC)

include %reldir%/numeric/module.mk
include %reldir%/system/module.mk
include %reldir%/util/module.mk
