%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

include %reldir%/+containers/module.mk
include %reldir%/+matlab/+lang/module.mk
include %reldir%/+matlab/+net/module.mk
include %reldir%/audio/module.mk
include %reldir%/deprecated/module.mk
include %reldir%/elfun/module.mk
include %reldir%/general/module.mk
include %reldir%/geometry/module.mk
include %reldir%/gui/module.mk
include %reldir%/help/module.mk
include %reldir%/image/module.mk
include %reldir%/io/module.mk
include %reldir%/java/module.mk
include %reldir%/legacy/module.mk
include %reldir%/linear-algebra/module.mk
include %reldir%/miscellaneous/module.mk
include %reldir%/ode/module.mk
include %reldir%/optimization/module.mk
include %reldir%/path/module.mk
include %reldir%/pkg/module.mk
include %reldir%/plot/appearance/module.mk
include %reldir%/plot/draw/module.mk
include %reldir%/plot/util/module.mk
include %reldir%/polynomial/module.mk
include %reldir%/prefs/module.mk
include %reldir%/profiler/module.mk
include %reldir%/set/module.mk
include %reldir%/signal/module.mk
include %reldir%/sparse/module.mk
include %reldir%/specfun/module.mk
include %reldir%/special-matrix/module.mk
include %reldir%/startup/module.mk
include %reldir%/statistics/module.mk
include %reldir%/strings/module.mk
include %reldir%/testfun/module.mk
include %reldir%/time/module.mk
include %reldir%/web/module.mk

## include %reldir%/@ftp/module.mk
## The include above fails because Automake cannot process the '@' character.
## As a work around, the contents of %reldir%/@ftp/module.mk are placed directly
## in this module.mk file.
scripts_EXTRA_DIST += %reldir%/@ftp/module.mk
######################## include %reldir%/@ftp/module.mk ########################
FCN_FILE_DIRS += %reldir%/@ftp

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config

%canon_reldir%_@ftp_FCN_FILES = \
  %reldir%/@ftp/ascii.m \
  %reldir%/@ftp/binary.m  \
  %reldir%/@ftp/cd.m  \
  %reldir%/@ftp/close.m  \
  %reldir%/@ftp/delete.m  \
  %reldir%/@ftp/dir.m  \
  %reldir%/@ftp/disp.m  \
  %reldir%/@ftp/ftp.m  \
  %reldir%/@ftp/loadobj.m  \
  %reldir%/@ftp/mget.m  \
  %reldir%/@ftp/mkdir.m  \
  %reldir%/@ftp/mput.m  \
  %reldir%/@ftp/rename.m  \
  %reldir%/@ftp/rmdir.m  \
  %reldir%/@ftp/saveobj.m

%canon_reldir%_@ftpdir = $(fcnfiledir)/@ftp

%canon_reldir%_@ftp_DATA = $(%canon_reldir%_@ftp_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_@ftp_FCN_FILES)

PKG_ADD_FILES += %reldir%/@ftp/PKG_ADD

DIRSTAMP_FILES += %reldir%/@ftp/$(octave_dirstamp)
####################### end include %reldir%/@ftp/module.mk #####################

image_DATA += $(SCRIPTS_IMAGES)

GEN_FCN_FILES_IN = $(GEN_FCN_FILES:.m=.in.m)

ALL_LOCAL_TARGETS += \
  $(JAR_FILES)

OCTAVE_INTERPRETER_TARGETS += \
  $(GEN_FCN_FILES) \
  $(PKG_ADD_FILES)

FCN_FILES_WITH_TESTS = $(shell $(SHELL) $(srcdir)/build-aux/find-files-with-tests.sh "$(srcdir)" $(FCN_FILES) $(GEN_FCN_FILES_IN))

define PKG_ADD_FILE_TEMPLATE
$(1)/PKG_ADD: $$($(2)_FCN_FILES) $$($(2)_GEN_FCN_FILES) $(1)/$(octave_dirstamp) %reldir%/mk-pkg-add.sh
	$$(AM_V_GEN)rm -f $$@-t $$@ && \
	$$(SHELL) $$(srcdir)/%reldir%/mk-pkg-add.sh $(srcdir) $$($(2)_FCN_FILES) -- $$($(2)_GEN_FCN_FILES) > $$@-t && \
	mv $$@-t $$@
endef

$(foreach d, $(FCN_FILE_DIRS), $(eval $(call PKG_ADD_FILE_TEMPLATE, $(d),$(subst /,_,$(subst -,_,$(d))))))

define GEN_FCN_FILES_TEMPLATE
$(1): $(1:.m=.in.m) build-aux/subst-config-vals.sh $(dir $(1))$(octave_dirstamp)
	$$(AM_V_GEN)$$(call simple-filter-rule,build-aux/subst-config-vals.sh)
endef

$(foreach f, $(GEN_FCN_FILES), $(eval $(call GEN_FCN_FILES_TEMPLATE, $(f))))

DOCSTRING_FILES += %reldir%/DOCSTRINGS

%reldir%/DOCSTRINGS: $(FCN_FILES) $(GEN_FCN_FILES_IN) | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f %reldir%/DOCSTRINGS-t && \
	$(PERL) $(srcdir)/%reldir%/mk-doc.pl "$(srcdir)" $(FCN_FILES) $(GEN_FCN_FILES_IN) > %reldir%/DOCSTRINGS-t && \
	$(call move_if_change_rule,%reldir%/DOCSTRINGS-t,$@)

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

check-m-sources:
	@echo "checking whether files in source tree are listed in module.mk files..."; \
	for f in $$(find $(srcdir)/scripts -name '*.m'); do \
	  found=false; \
	  for m in $(FCN_FILES) $(GEN_FCN_FILES); do \
	    if [ "$$f" = $(srcdir)/%reldir%/"$$m" ]; then \
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
	    base=`echo $$f | $(SED) 's,^%reldir%/,,'`; \
	    $(MKDIR_P) $(DESTDIR)$(fcnfiledir)/`echo $$base | $(SED) 's,/[^/]*$$,,'`; \
	    $(INSTALL_DATA) $$f $(DESTDIR)$(fcnfiledir)/$$base; \
	  fi \
	done
.PHONY: install-pkg-add

uninstall-pkg-add:
	for f in $(PKG_ADD_FILES); do \
	  base=`echo $$f | $(SED) 's,^%reldir%/,,'`; \
	  rm -f $(DESTDIR)$(fcnfiledir)/$$base; \
	done
.PHONY: uninstall-pkg-add

if AMCOND_HAVE_JAVA
scripts-dist-hook:
else
scripts-dist-hook:
	@echo "Packaging distribution requires Java." ; exit 1;
endif

%canon_reldir%_EXTRA_DIST += \
  $(SCRIPTS_IMAGES) \
  $(FCN_FILES) \
  $(GEN_FCN_FILES_IN) \
  %reldir%/DOCSTRINGS \
  %reldir%/mk-doc.pl \
  %reldir%/mk-pkg-add.sh

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_CLEANFILES += \
  $(GEN_FCN_FILES) \
  $(PKG_ADD_FILES)

%canon_reldir%_DISTCLEANFILES += \
  $(DIRSTAMP_FILES)

%canon_reldir%_MAINTAINERCLEANFILES += \
  %reldir%/DOCSTRINGS

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

scripts-clean:
	rm -f $(%canon_reldir%_CLEANFILES)

scripts-distclean: scripts-clean
	rm -f $(%canon_reldir%_DISTCLEANFILES)

scripts-maintainer-clean: scripts-distclean
	rm -f $(%canon_reldir%_MAINTAINERCLEANFILES)
