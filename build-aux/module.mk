## utility rules to aid development

EXTRA_DIST += \
  %reldir%/update-bug-status.sh

ALL_TEST_FILES = \
  $(addprefix $(srcdir)/, $(LIBINTERP_TST_FILES_SRC)) \
  $(addprefix $(srcdir)/, $(FCN_FILES_WITH_TESTS)) \
  $(addprefix $(srcdir)/, $(TEST_FILES))

## Tag bug IDs in tests as fixed
update-bug-status:
	$(SHELL) $(srcdir)/%reldir%/update-bug-status.sh $(ALL_TEST_FILES)
.PHONY: update-bug-status
