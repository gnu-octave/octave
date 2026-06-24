EXTRA_DIST += \
  %reldir%/OctJavaQry.class \
  %reldir%/OctJavaQry.java \
  %reldir%/changelog.tmpl \
  %reldir%/find-defun-files.sh \
  %reldir%/find-files-with-tests.sh \
  %reldir%/get-source-mtime.sh \
  %reldir%/inplace-edit.pl \
  %reldir%/mk-hg-id.sh \
  %reldir%/mk-octave-config-h.sh \
  %reldir%/mk-opts.pl \
  %reldir%/move-if-change \
  %reldir%/subst-config-vals.in.sh \
  %reldir%/subst-cross-config-vals.in.sh \
  %reldir%/subst-script-vals.in.sh \
  %reldir%/update-bug-status.sh

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

## utility rules to aid development

ALL_TEST_FILES := \
  $(addprefix $(srcdir)/, $(LIBOCTAVE_TST_FILES_SRC)) \
  $(addprefix $(srcdir)/, $(LIBOCTINTERP_TST_FILES_SRC)) \
  $(addprefix $(srcdir)/, $(FCN_FILES_WITH_TESTS))

## Tag bug IDs in tests as fixed
update-bug-status:
	$(SHELL) $(srcdir)/%reldir%/update-bug-status.sh $(ALL_TEST_FILES)
	cd test && $(MAKE) $(AM_MAKEFLAGS) $@
.PHONY: update-bug-status

DISTCLEANFILES += \
  %reldir%/subst-config-vals.sh \
  %reldir%/subst-cross-config-vals.sh \
  %reldir%/subst-script-vals.sh
