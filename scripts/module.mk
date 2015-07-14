## FIXME -- including scripts/@ftp/module.mk fails.  Is that an automake bug?

FCN_FILE_DIRS += scripts/@ftp

scripts_@ftp_FCN_FILES = \
  scripts/@ftp/ascii.m \
  scripts/@ftp/binary.m  \
  scripts/@ftp/cd.m  \
  scripts/@ftp/close.m  \
  scripts/@ftp/delete.m  \
  scripts/@ftp/dir.m  \
  scripts/@ftp/display.m  \
  scripts/@ftp/ftp.m  \
  scripts/@ftp/loadobj.m  \
  scripts/@ftp/mget.m  \
  scripts/@ftp/mkdir.m  \
  scripts/@ftp/mput.m  \
  scripts/@ftp/rename.m  \
  scripts/@ftp/rmdir.m  \
  scripts/@ftp/saveobj.m

scripts_@ftpdir = $(fcnfiledir)/@ftp/module.mk fails.  Is that an automake bug?

scripts_@ftp_DATA = $(scripts_@ftp_FCN_FILES)

FCN_FILES += $(scripts_@ftp_FCN_FILES)

PKG_ADD_FILES += scripts/@ftp/PKG_ADD

DIRSTAMP_FILES += scripts/@ftp/$(octave_dirstamp)

include scripts/audio/module.mk
include scripts/deprecated/module.mk
include scripts/elfun/module.mk
include scripts/general/module.mk
include scripts/geometry/module.mk
include scripts/gui/module.mk
include scripts/help/module.mk
include scripts/image/module.mk
include scripts/io/module.mk
include scripts/java/module.mk
include scripts/linear-algebra/module.mk
include scripts/miscellaneous/module.mk
include scripts/optimization/module.mk
include scripts/path/module.mk
include scripts/pkg/module.mk
include scripts/plot/appearance/module.mk
include scripts/plot/draw/module.mk
include scripts/plot/util/module.mk
include scripts/polynomial/module.mk
include scripts/prefs/module.mk
include scripts/set/module.mk
include scripts/signal/module.mk
include scripts/sparse/module.mk
include scripts/specfun/module.mk
include scripts/special-matrix/module.mk
include scripts/startup/module.mk
include scripts/statistics/base/module.mk
include scripts/statistics/distributions/module.mk
include scripts/statistics/models/module.mk
include scripts/statistics/tests/module.mk
include scripts/strings/module.mk
include scripts/testfun/module.mk
include scripts/time/module.mk

image_DATA += $(SCRIPTS_IMAGES)

FCN_FILES_IN = $(GEN_FCN_FILES:.m=.in)

ALL_LOCAL_TARGETS += \
  $(GEN_FCN_FILES) \
  $(PKG_ADD_FILES) \
  $(JAR_FILES)

if AMCOND_BUILD_DOCS
ALL_LOCAL_TARGETS += scripts/.DOCSTRINGS
endif

define PKG_ADD_FILE_TEMPLATE
$(1)/PKG_ADD: $$($(2)_FCN_FILES) $$($(2)_GEN_FCN_FILES) $(1)/$(octave_dirstamp) scripts/mk-pkg-add
	$$(AM_V_GEN)rm -f $$@-t $$@ && \
	$$(srcdir)/scripts/mk-pkg-add $(srcdir) $$($(2)_FCN_FILES) -- $$($(2)_GEN_FCN_FILES) > $$@-t && \
	mv $$@-t $$@
endef

$(foreach d, $(FCN_FILE_DIRS), $(eval $(call PKG_ADD_FILE_TEMPLATE, $(d),$(subst /,_,$(subst -,_,$(d))))))

define GEN_FCN_FILES_TEMPLATE
$(1): $(1:.m=.in) Makefile $(dir $(1))$(octave_dirstamp)
	$$(AM_V_GEN)$$(do_subst_config_vals)
endef

$(foreach f, $(GEN_FCN_FILES), $(eval $(call GEN_FCN_FILES_TEMPLATE, $(f))))

if AMCOND_BUILD_DOCS

scripts/.DOCSTRINGS: $(FCN_FILES) $(GEN_FCN_FILES) scripts/mkdoc.pl Makefile
	$(AM_V_GEN)rm -f $@-t $@ && \
	if [ "x$(srcdir)" != "x." ] && [ -f $(srcdir)/scripts/DOCSTRINGS ] && [ ! -f scripts/DOCSTRINGS ]; then \
		cp $(srcdir)/scripts/DOCSTRINGS scripts/DOCSTRINGS; \
		touch -r $(srcdir)/scripts/DOCSTRINGS scripts/DOCSTRINGS; \
	fi && \
	$(PERL) $(srcdir)/scripts/mkdoc.pl "$(srcdir)" $(FCN_FILES) -- $(GEN_FCN_FILES) > $@-t && \
	mv $@-t $@ && \
	$(top_srcdir)/build-aux/move-if-change $@ scripts/DOCSTRINGS && \
	touch $@

OCTAVE_INTERPRETER_TARGETS += \
  scripts/.DOCSTRINGS

endif

check-m-sources:
	@echo "checking whether files in source tree are listed in module.mk files..."; \
	for f in $$(find $(srcdir)/scripts -name '*.m'); do \
	  found=false; \
	  for m in $(FCN_FILES) $(GEN_FCN_FILES); do \
	    if [ "$$f" = $(srcdir)/scripts/"$$m" ]; then \
	      found=true; \
	      break; \
	    fi; \
	  done; \
	  if $$found; then \
	    true; \
	  else \
	    missing=$$(echo $$f | $(SED) "s|^$(srcdir)/scripts||"); \
	    echo "$$missing: not listed in SOURCES"; \
	  fi; \
	done; \
	if test -z "$$missing"; then \
	  echo "yes"; \
	fi
.PHONY: check-m-sources

check-missing-semicolon:
	@echo "checking for missing semicolons in .m files..."
	( echo "warning on Octave:missing-semicolon;"; \
	  for m in $(addprefix $(srcdir), $(FCN_FILES)) $(GEN_FCN_FILES); do \
	    echo "source ('$$m');"; \
	  done ) | ../run-octave -qf
.PHONY: check-missing-semicolon

## Add rule to generate ctags.
## Automake would normally generate such a rule, but only if there is a
## xxx_SOURCES target
ctags:
	ctags $(addprefix $(srcdir)/, $(FCN_FILES)) $(GEN_FCN_FILES)

install-data-local: install-startup-files install-pkg-add

uninstall-local: uninstall-startup-files uninstall-pkg-add

install-startup-files:
	$(MKDIR_P) $(DESTDIR)$(fcnfiledir)/startup
	if test -f $(DESTDIR)$(fcnfiledir)/startup/octaverc; then true; \
	else \
	  $(INSTALL_DATA) $(srcdir)/$(SYSTEM_STARTUP_FILE_SRC) \
	    $(DESTDIR)$(fcnfiledir)/startup/octaverc; \
	fi
	if test -f $(DESTDIR)$(fcnfiledir)/startup/inputrc; then true; \
	else \
	  $(INSTALL_DATA) $(srcdir)/$(SYSTEM_INPUTRC_FILE_SRC) \
	    $(DESTDIR)$(fcnfiledir)/startup/inputrc; \
	fi
	$(MKDIR_P) $(DESTDIR)$(localfcnfiledir)/startup
	if test -f $(DESTDIR)$(localfcnfiledir)/startup/octaverc; \
	then true; \
	else \
	  $(INSTALL_DATA) $(srcdir)/$(LOCAL_STARTUP_FILE_SRC) \
	    $(DESTDIR)$(localfcnfiledir)/startup/octaverc; \
	fi
.PHONY: install-startup-files

uninstall-startup-files:
	rm -f $(DESTDIR)$(fcnfiledir)/startup/octaverc
	rm -f $(DESTDIR)$(fcnfiledir)/startup/inputrc
	rm -f $(DESTDIR)$(localfcnfiledir)/startup/octaverc
.PHONY: uninstall-startup-files

install-pkg-add:
	for f in $(PKG_ADD_FILES); do \
	  if [ -n "`cat $$f`" ]; then \
	    $(MKDIR_P) $(DESTDIR)$(fcnfiledir)/`echo $$f | $(SED) 's,/[^/]*$$,,'`; \
	    $(INSTALL_DATA) $$f $(DESTDIR)$(fcnfiledir)/$$f; \
	  fi \
	done
.PHONY: install-pkg-add

uninstall-pkg-add:
	for f in $(PKG_ADD_FILES); do \
	  rm -f $(DESTDIR)$(fcnfiledir)/$$f; \
	done
.PHONY: uninstall-pkg-add

if AMCOND_HAVE_JAVA
scripts-dist-hook:
else
scripts-dist-hook:
	@echo "Packaging distribution requires Java." ; exit 1;
endif

EXTRA_DIST += \
  $(SCRIPTS_IMAGES) \
  $(FCN_FILES) \
  $(FCN_FILES_IN) \
  $(GEN_FCN_FILES) \
  scripts/DOCSTRINGS \
  scripts/mkdoc.pl \
  scripts/mk-pkg-add

DISTCLEANFILES += \
  scripts/.DOCSTRINGS \
  scripts/DOCSTRINGS \
  $(PKG_ADD_FILES) \
  $(DIRSTAMP_FILES) \
  $(GEN_FCN_FILES)

scripts-distclean:
	if [ "x$(srcdir)" != "x." ]; then \
	  rm -f $(java_JAVA_IMAGES); \
	fi
