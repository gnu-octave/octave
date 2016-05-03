scripts_EXTRA_DIST =

scripts_CLEANFILES =
scripts_DISTCLEANFILES =
scripts_MAINTAINERCLEANFILES =

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
include scripts/ode/module.mk
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

## include scripts/@ftp/module.mk
## The include above fails because Automake cannot process the '@' character.
## As a work around, the contents of scripts/@ftp/module.mk are placed directly
## in this module.mk file.
######################## include scripts/@ftp/module.mk ########################
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

scripts_@ftpdir = $(fcnfiledir)/@ftp

scripts_@ftp_DATA = $(scripts_@ftp_FCN_FILES)

FCN_FILES += $(scripts_@ftp_FCN_FILES)

PKG_ADD_FILES += scripts/@ftp/PKG_ADD

DIRSTAMP_FILES += scripts/@ftp/$(octave_dirstamp)
####################### end include scripts/@ftp/module.mk #####################

image_DATA += $(SCRIPTS_IMAGES)

GEN_FCN_FILES_IN = $(GEN_FCN_FILES:.m=.in)

ALL_LOCAL_TARGETS += \
  $(GEN_FCN_FILES) \
  $(PKG_ADD_FILES) \
  $(JAR_FILES)

define PKG_ADD_FILE_TEMPLATE
$(1)/PKG_ADD: $$($(2)_FCN_FILES) $$($(2)_GEN_FCN_FILES) $(1)/$(octave_dirstamp) scripts/mk-pkg-add
	$$(AM_V_GEN)rm -f $$@-t $$@ && \
	$$(SHELL) $$(srcdir)/scripts/mk-pkg-add $(srcdir) $$($(2)_FCN_FILES) -- $$($(2)_GEN_FCN_FILES) > $$@-t && \
	mv $$@-t $$@
endef

$(foreach d, $(FCN_FILE_DIRS), $(eval $(call PKG_ADD_FILE_TEMPLATE, $(d),$(subst /,_,$(subst -,_,$(d))))))

define GEN_FCN_FILES_TEMPLATE
$(1): $(1:.m=.in) build-aux/subst-config-vals.sh $(dir $(1))$(octave_dirstamp)
	$$(AM_V_GEN)$$(call simple-filter-rule,build-aux/subst-config-vals.sh)
endef

$(foreach f, $(GEN_FCN_FILES), $(eval $(call GEN_FCN_FILES_TEMPLATE, $(f))))

if AMCOND_BUILD_DOCS

DOCSTRING_FILES += $(srcdir)/scripts/DOCSTRINGS

$(srcdir)/scripts/DOCSTRINGS: $(FCN_FILES) $(GEN_FCN_FILES) | scripts/$(octave-dirstamp)
	$(AM_V_GEN)rm -f scripts/DOCSTRINGS-t && \
	$(PERL) $(srcdir)/scripts/mkdoc.pl "$(srcdir)" $(FCN_FILES) -- $(GEN_FCN_FILES) > scripts/DOCSTRINGS-t && \
	$(SHELL) $(srcdir)/build-aux/move-if-change scripts/DOCSTRINGS-t $@

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
	@( echo "warning on Octave:missing-semicolon;"; \
	  for m in $(addprefix $(srcdir)/, $(FCN_FILES)) $(GEN_FCN_FILES); do \
	    ! $(GREP) -q -E '^classdef' $$m || continue; \
	    ! $(GREP) -q -E '^  *\<function\>' $$m || continue; \
	    ! (echo $$m | $(GREP) -q __splinefit__.m) || continue; \
	    echo "source ('$$m');"; \
	  done ) | $(SHELL) run-octave --norc --silent --no-history
.PHONY: check-missing-semicolon

## Include m-files in list of sources when building tag files.
## Automake will not include these because there is no xxx_SOURCES target
TAGS_DEPENDENCIES = $(addprefix $(srcdir)/, $(FCN_FILES)) $(GEN_FCN_FILES)
TAGS_FILES = $(addprefix $(srcdir)/, $(FCN_FILES)) $(GEN_FCN_FILES)

install-data-local: install-startup-files install-pkg-add

uninstall-local: uninstall-startup-files uninstall-pkg-add

install-pkg-add:
	for f in $(PKG_ADD_FILES); do \
	  if [ -n "`cat $$f`" ]; then \
	    base=`echo $$f | $(SED) 's,^scripts/,,'`; \
	    $(MKDIR_P) $(DESTDIR)$(fcnfiledir)/`echo $$base | $(SED) 's,/[^/]*$$,,'`; \
	    $(INSTALL_DATA) $$f $(DESTDIR)$(fcnfiledir)/$$base; \
	  fi \
	done
.PHONY: install-pkg-add

uninstall-pkg-add:
	for f in $(PKG_ADD_FILES); do \
	  base=`echo $$f | $(SED) 's,^scripts/,,'`; \
	  rm -f $(DESTDIR)$(fcnfiledir)/$$base; \
	done
.PHONY: uninstall-pkg-add

if AMCOND_HAVE_JAVA
scripts-dist-hook:
else
scripts-dist-hook:
	@echo "Packaging distribution requires Java." ; exit 1;
endif

scripts_EXTRA_DIST += \
  $(SCRIPTS_IMAGES) \
  $(FCN_FILES) \
  $(GEN_FCN_FILES_IN) \
  $(srcdir)/scripts/DOCSTRINGS \
  scripts/mkdoc.pl \
  scripts/mk-pkg-add

EXTRA_DIST += $(scripts_EXTRA_DIST)

scripts_DISTCLEANFILES += \
  $(PKG_ADD_FILES) \
  $(DIRSTAMP_FILES) \
  $(GEN_FCN_FILES)

scripts_MAINTAINERCLEANFILES += \
  $(srcdir)/scripts/DOCSTRINGS

CLEANFILES += $(scripts_CLEANFILES)
DISTCLEANFILES += $(scripts_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(scripts_MAINTAINERCLEANFILES)

scripts-clean:
	rm -f $(scripts_CLEANFILES)

scripts-distclean: scripts-clean
	rm -f $(scripts_DISTCLEANFILES)

scripts-maintainer-clean: scripts-distclean
	rm -f $(scripts_MAINTAINERCLEANFILES)
