EXTRA_DIST += \
  %reldir%/OctJavaQry.class \
  %reldir%/OctJavaQry.java \
  %reldir%/changelog.tmpl \
  %reldir%/check-subst-vars.in.sh \
  %reldir%/find-defun-files.sh \
  %reldir%/find-files-with-tests.sh \
  %reldir%/get-source-mtime.sh \
  %reldir%/inplace_edit.pl \
  %reldir%/mk-hg-id.sh \
  %reldir%/mk-octave-config-h.sh \
  %reldir%/mk-opts.pl \
  %reldir%/mk-pkg-add.sh \
  %reldir%/move-if-change \
  %reldir%/stl_algo.h-fixed \
  %reldir%/subst-config-vals.in.sh \
  %reldir%/subst-cross-config-vals.in.sh \
  %reldir%/subst-script-vals.in.sh \
  %reldir%/update-bug-status.sh

GEN_CONFIG_SHELL += \
  %reldir%/subst-config-vals.sh \
  %reldir%/subst-cross-config-vals.sh \
  %reldir%/subst-script-vals.sh

$(GEN_CONFIG_SHELL) : %.sh : %.in.sh config.status
	$(AM_V_GEN)$(SHELL) config.status $@-tmp $@

GEN_CONFIG_INC = \
  oct-conf-post-private.h \
  oct-conf-post-public.h

$(GEN_CONFIG_INC) : %.h : %.in.h config.status
	$(AM_V_GEN)$(SHELL) config.status $@-tmp $@

### utility rules to aid development

ALL_TEST_FILES = \
  $(addprefix $(srcdir)/, $(LIBOCTAVE_TST_FILES_SRC)) \
  $(addprefix $(srcdir)/, $(LIBINTERP_TST_FILES_SRC)) \
  $(addprefix $(srcdir)/, $(FCN_FILES_WITH_TESTS)) \
  $(addprefix $(srcdir)/, $(TEST_FILES))

## Tag bug IDs in tests as fixed
update-bug-status:
	$(SHELL) $(srcdir)/%reldir%/update-bug-status.sh $(ALL_TEST_FILES)
.PHONY: update-bug-status
