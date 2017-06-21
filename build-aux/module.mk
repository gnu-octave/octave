EXTRA_DIST += \
  %reldir%/OctJavaQry.class \
  %reldir%/OctJavaQry.java \
  %reldir%/changelog.tmpl \
  %reldir%/check-subst-vars.in.sh \
  %reldir%/find-defun-files.sh \
  %reldir%/find-files-with-tests.sh \
  %reldir%/mk-builtins.pl \
  %reldir%/mk-default-qt-settings.in.sh \
  %reldir%/mk-f77-def.in.sh \
  %reldir%/mk-hg-id.sh \
  %reldir%/mk-mxarray-h.in.sh \
  %reldir%/mk-octave-config-h.sh \
  %reldir%/mk-opts.pl \
  %reldir%/mk-version-h.in.sh \
  %reldir%/move-if-change \
  %reldir%/stl_algo.h-fixed \
  %reldir%/subst-config-vals.in.sh \
  %reldir%/subst-cross-config-vals.in.sh \
  %reldir%/subst-default-vals.in.sh \
  %reldir%/subst-script-vals.in.sh \
  %reldir%/update-bug-status.sh

GEN_CONFIG_SHELL = \
  %reldir%/mk-default-qt-settings.sh \
  %reldir%/mk-f77-def.sh \
  %reldir%/mk-mxarray-h.sh \
  %reldir%/mk-version-h.sh \
  %reldir%/subst-config-vals.sh \
  %reldir%/subst-cross-config-vals.sh \
  %reldir%/subst-default-vals.sh \
  %reldir%/subst-script-vals.sh

$(GEN_CONFIG_SHELL) : %.sh : %.in.sh config.status
	$(AM_V_GEN)$(SHELL) config.status $@-tmp $@

GEN_CONFIG_INC = \
  oct-conf-post.h

$(GEN_CONFIG_INC) : %.h : %.in.h config.status
	$(AM_V_GEN)$(SHELL) config.status $@-tmp $@

### utility rules to aid development

ALL_TEST_FILES = \
  $(addprefix $(srcdir)/, $(LIBINTERP_TST_FILES_SRC)) \
  $(addprefix $(srcdir)/, $(FCN_FILES_WITH_TESTS)) \
  $(addprefix $(srcdir)/, $(TEST_FILES))

## Tag bug IDs in tests as fixed
update-bug-status:
	$(SHELL) $(srcdir)/%reldir%/update-bug-status.sh $(ALL_TEST_FILES)
.PHONY: update-bug-status
