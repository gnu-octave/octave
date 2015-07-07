# Makefile for Octave's scripts directory
#
# Copyright (C) 1993-2015 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/common.mk

EXTRA_DIST =

CLEANFILES =

DISTCLEANFILES =

FCN_FILE_DIRS =

FCN_FILES =

GEN_FCN_FILES =

PKG_ADD_FILES =

DIRSTAMP_FILES =

IMAGES =

JAR_FILES =

## Read subdirs to set all variables above
include @ftp/module.mk
include audio/module.mk
include deprecated/module.mk
include elfun/module.mk
include general/module.mk
include geometry/module.mk
include gui/module.mk
include help/module.mk
include image/module.mk
include io/module.mk
include java/module.mk
include linear-algebra/module.mk
include miscellaneous/module.mk
include optimization/module.mk
include path/module.mk
include pkg/module.mk
include plot/appearance/module.mk
include plot/draw/module.mk
include plot/util/module.mk
include polynomial/module.mk
include prefs/module.mk
include set/module.mk
include signal/module.mk
include sparse/module.mk
include specfun/module.mk
include special-matrix/module.mk
include startup/module.mk
include statistics/base/module.mk
include statistics/distributions/module.mk
include statistics/models/module.mk
include statistics/tests/module.mk
include strings/module.mk
include testfun/module.mk
include time/module.mk

nobase_fcnfile_DATA = $(FCN_FILES) $(GEN_FCN_FILES) $(JAR_FILES)

image_DATA = $(IMAGES)

FCN_FILES_IN = $(GEN_FCN_FILES:.m=.in)

if AMCOND_BUILD_DOCS
all-local: $(GEN_FCN_FILES) $(PKG_ADD_FILES) $(JAR_FILES) .DOCSTRINGS
else
all-local: $(GEN_FCN_FILES) $(PKG_ADD_FILES) $(JAR_FILES)
endif

octave_dirstamp = $(am__leading_dot)dirstamp

$(DIRSTAMP_FILES):
	$(AM_V_GEN)$(MKDIR_P) $(@D) && \
	: > $@

define PKG_ADD_FILE_TEMPLATE
$(1)/PKG_ADD: $$($(subst /,_,$(1))_FCN_FILES) $$($(subst /,_,$(1))_GEN_FCN_FILES) $(1)/$(octave_dirstamp) mk-pkg-add
	$$(AM_V_GEN)rm -f $$@-t $$@ && \
	$$(srcdir)/mk-pkg-add $(srcdir) $$($$(@D)_FCN_FILES) -- $$($$(@D)_GEN_FCN_FILES) > $$@-t && \
	mv $$@-t $$@
endef

$(foreach d, $(FCN_FILE_DIRS), $(eval $(call PKG_ADD_FILE_TEMPLATE, $(d))))

define GEN_FCN_FILES_TEMPLATE
$(1): $(1:.m=.in) Makefile $(dir $(1))$(octave_dirstamp)
	$$(AM_V_GEN)$$(do_subst_config_vals)
endef

$(foreach f, $(GEN_FCN_FILES), $(eval $(call GEN_FCN_FILES_TEMPLATE, $(f))))

if AMCOND_BUILD_DOCS

.DOCSTRINGS: $(FCN_FILES) $(GEN_FCN_FILES) mkdoc.pl Makefile
	$(AM_V_GEN)rm -f $@-t $@ && \
	if [ "x$(srcdir)" != "x." ] && [ -f $(srcdir)/DOCSTRINGS ] && [ ! -f DOCSTRINGS ]; then \
		cp $(srcdir)/DOCSTRINGS DOCSTRINGS; \
		touch -r $(srcdir)/DOCSTRINGS DOCSTRINGS; \
	fi && \
	$(PERL) $(srcdir)/mkdoc.pl "$(srcdir)" $(FCN_FILES) -- $(GEN_FCN_FILES) > $@-t && \
	mv $@-t $@ && \
	$(top_srcdir)/build-aux/move-if-change $@ DOCSTRINGS && \
	touch $@

endif

check-m-sources:
	@echo "checking whether files in source tree are listed in module.mk files..."; \
	for f in $$(find $(srcdir) -name '*.m'); do \
	  found=false; \
	  for m in $(FCN_FILES) $(GEN_FCN_FILES); do \
	    if [ "$$f" = $(srcdir)/"$$m" ]; then \
	      found=true; \
	      break; \
	    fi; \
	  done; \
	  if $$found; then \
	    true; \
	  else \
	    missing=$$(echo $$f | $(SED) "s|^$(srcdir)||"); \
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
	  for m in $(addprefix $(srcdir)/, $(FCN_FILES)) $(GEN_FCN_FILES); do \
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
else
dist-hook:
	@echo "Packaging distribution requires Java." ; exit 1;
endif

EXTRA_DIST += \
  $(IMAGES) \
  $(FCN_FILES) \
  $(FCN_FILES_IN) \
  $(GEN_FCN_FILES) \
  DOCSTRINGS \
  mkdoc.pl \
  mk-pkg-add

DISTCLEANFILES += \
  .DOCSTRINGS \
  DOCSTRINGS \
  $(PKG_ADD_FILES) \
  $(DIRSTAMP_FILES) \
  $(GEN_FCN_FILES)

distclean-local:
	if [ "x$(srcdir)" != "x." ]; then \
	  rm -f $(java_JAVA_IMAGES); \
	fi
