src_EXTRA_DIST =

src_CLEANFILES =
src_DISTCLEANFILES =
src_MAINTAINERCLEANFILES =

## Search local directories before those specified by the user.

SRC_DIR_CPPFLAGS = \
  -Iliboctave -I$(srcdir)/liboctave \
  -I$(srcdir)/liboctave/array \
  -I$(srcdir)/liboctave/cruft/misc \
  -I$(srcdir)/liboctave/numeric \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util \
  -Iliboctave/wrappers -I$(srcdir)/liboctave/wrappers \
  -Ilibinterp -I$(srcdir)/libinterp \
  -Ilibinterp/corefcn -I$(srcdir)/libinterp/corefcn \
  -I$(srcdir)/src \
  -Ilibgnu -I$(srcdir)/libgnu

EXTRA_DIST += \
  src/main.in.cc \
  src/mkoctfile.in.cc \
  src/octave-build-info.in.cc \
  src/octave-config.in.cc

DISTCLEANFILES += \
  src/main.cc \
  src/mkoctfile.cc \
  src/octave-config.cc

bin_PROGRAMS += \
  src/mkoctfile \
  src/octave \
  src/octave-cli \
  src/octave-config

OCTAVE_INTERPRETER_TARGETS += \
  $(bin_PROGRAMS) \
  $(OCTAVE_VERSION_LINKS)

octinclude_HEADERS += \
  src/octave-build-info.h

noinst_HEADERS += \
  src/display-available.h \
  src/shared-fcns.h

OCTAVE_VERSION_LINKS += src/octave-cli-$(version)$(EXEEXT)

if AMCOND_BUILD_QT_GUI
  archlib_PROGRAMS += src/octave-gui
  OCTAVE_VERSION_LINKS += src/octave-gui-$(version)$(EXEEXT)

  OCTAVE_INTERPRETER_TARGETS += src/octave-gui$(EXEEXT)
endif

OCTAVE_CORE_LIBS = \
  libinterp/liboctinterp.la \
  liboctave/liboctave.la \
  libgnu/libgnu.la

nodist_src_octave_SOURCES = src/main.cc

src_octave_SOURCES = src/display-available.c

src_octave_LDADD = \
  liboctave/wrappers/libwrappers.la \
  libgnu/libgnu.la \
  $(X11_LIBS) \
  $(CARBON_LIBS) \
  $(GNULIB_LINK_DEPS)

src_octave_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_LINK_OPTS) \
  $(WARN_LDFLAGS)

if AMCOND_BUILD_QT_GUI
  OCTAVE_CPPFLAGS = -DHAVE_OCTAVE_QT_GUI
endif

src_octave_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

src_octave_CXXFLAGS = \
  $(AM_CXXFLAGS) \
  $(WARN_CXXFLAGS)

src_octave_cli_SOURCES = src/main-cli.cc
nodist_src_octave_cli_SOURCES = src/octave-build-info.cc

src_octave_cli_LDADD = \
  $(OCTAVE_CORE_LIBS) \
  $(OCTAVE_LINK_DEPS)

src_octave_cli_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_LINK_OPTS) \
  $(WARN_LDFLAGS)

src_octave_cli_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

src_octave_cli_CXXFLAGS = \
  $(AM_CXXFLAGS) \
  $(WARN_CXXFLAGS)

if AMCOND_BUILD_QT_GUI
  src_octave_gui_SOURCES = src/main-gui.cc
  nodist_src_octave_gui_SOURCES = src/octave-build-info.cc
  OCTAVE_GUI_LIBS = libgui/liboctgui.la
  OCTAVE_GUI_CPPFLAGS = -Ilibgui/src -I$(srcdir)/libgui/src
endif

src_octave_gui_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_GUI_CPPFLAGS)

src_octave_gui_LDADD = \
  $(OCTAVE_GUI_LIBS) \
  $(OCTAVE_CORE_LIBS) \
  $(OCTAVE_GUI_LINK_DEPS)

src_octave_gui_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_GUI_LINK_OPTS) \
  $(WARN_LDFLAGS)

src_octave_gui_CXXFLAGS = \
  $(AM_CXXFLAGS) \
  $(WARN_CXXFLAGS)

src_mkoctfile_SOURCES =

nodist_src_mkoctfile_SOURCES = src/mkoctfile.cc

src_mkoctfile_LDADD = \
  libgnu/libgnu.la $(LIBS)

src_mkoctfile_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

src_mkoctfile_CXXFLAGS = \
  $(AM_CXXFLAGS) \
  $(WARN_CXXFLAGS)

src_octave_config_SOURCES =

nodist_src_octave_config_SOURCES = src/octave-config.cc

src_octave_config_LDADD = \
  libinterp/corefcn/libcorefcn.la \
  libgnu/libgnu.la \
  $(LIBS)

src_octave_config_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

src_octave_config_CXXFLAGS = \
  $(AM_CXXFLAGS) \
  $(WARN_CXXFLAGS)

DIRSTAMP_FILES += src/$(octave_dirstamp)

mostlyclean-local: src-mostlyclean-local
.PHONY: src-mostlyclean-local

if AMCOND_CROSS_TOOLS

## Building cross mkoctfile.

OCTAVE_CROSS_TOOLS += src/$(host_triplet)-mkoctfile$(BUILD_EXEEXT)

src/$(host_triplet)-mkoctfile$(BUILD_EXEEXT): src/$(host_triplet)-mkoctfile.cc
	$(BUILD_CXX) -o src/$(host_triplet)-mkoctfile$(BUILD_EXEEXT) -Dgnulib='' -Doctave_idx_type=int $(DEFAULT_INCLUDES) $(BUILD_CXXFLAGS) $(BUILD_LDFLAGS) -I$(srcdir)/src src/$(host_triplet)-mkoctfile.cc

src/$(host_triplet)-mkoctfile.cc: src/mkoctfile.in.cc build-aux/subst-cross-config-vals.sh | src/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-cross-config-vals.sh)

## Building cross octave-config.

OCTAVE_CROSS_TOOLS += src/$(host_triplet)-octave-config$(BUILD_EXEEXT)

src/$(host_triplet)-octave-config$(BUILD_EXEEXT): src/$(host_triplet)-octave-config.cc
	$(BUILD_CXX) -o src/$(host_triplet)-octave-config$(BUILD_EXEEXT) -Dgnulib='' -Doctave_idx_type=int $(DEFAULT_INCLUDES) $(BUILD_CXXFLAGS) $(BUILD_LDFLAGS) -I$(srcdir)/src src/$(host_triplet)-octave-config.cc

src/$(host_triplet)-octave-config.cc: src/octave-config.in.cc build-aux/subst-default-vals.sh | src/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-default-vals.sh)

src-mostlyclean-local:
	-rm -f $(OCTAVE_CROSS_TOOLS)

else

src-mostlyclean-local:

endif

src/octave-config.cc: src/octave-config.in.cc build-aux/subst-default-vals.sh | src/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-default-vals.sh)

src/mkoctfile.cc: src/mkoctfile.in.cc build-aux/subst-config-vals.sh | src/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

src/main.cc: src/main.in.cc build-aux/subst-default-vals.sh | src/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-default-vals.sh)

src/octave-build-info.cc: src/octave-build-info.in.cc HG-ID | src/$(octave-dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(SED) \
	  -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically by Makefile|" \
	  -e "s|%OCTAVE_HG_ID%|`cat $(builddir)/HG-ID`|" $< > $@-t && \
	$(simple_move_if_change_rule)

ALL_LOCAL_TARGETS += $(OCTAVE_VERSION_LINKS) $(OCTAVE_CROSS_TOOLS)

install-exec-hook: make-version-links

uninstall-local: remove-version-links

make-version-links:
	cd $(DESTDIR)$(bindir) && \
	for f in $(notdir $(basename $(bin_PROGRAMS))); do \
	  mv $$f$(EXEEXT) $$f-$(version)$(EXEEXT) && \
	    $(LN_S) $$f-$(version)$(EXEEXT) $$f$(EXEEXT); \
	done

remove-version-links:
	for f in $(notdir $(basename $(bin_PROGRAMS))); do \
	  rm -f $(DESTDIR)$(bindir)/$$f-$(version)$(EXEEXT); \
	done

.PHONY: make-version-links remove-version-links

## We need these filenames in the build tree because the wrapper
## program (main.cc) will try to invoke the versioned binaries.

src/octave-cli-$(version)$(EXEEXT): src/octave-cli$(EXEEXT)
	$(AM_V_GEN)rm -f $@ && \
	cd $(@D) && $(LN_S) $(<F) $(@F)

src/octave-gui-$(version)$(EXEEXT): src/octave-gui$(EXEEXT)
	$(AM_V_GEN)rm -f $@ && \
	cd $(@D) && $(LN_S) $(<F) $(@F)

src_CLEANFILES += $(OCTAVE_VERSION_LINKS)
src_DISTCLEANFILES += src/octave-build-info.cc

CLEANFILES += $(src_CLEANFILES)
DISTCLEANFILES += $(src_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(src_MAINTAINERCLEANFILES)

src-clean:
	rm -f $(src_CLEANFILES)

src-distclean: src-clean
	rm -f $(src_DISTCLEANFILES)

src-maintainer-clean: src-distclean
	rm -f $(src_MAINTAINERCLEANFILES)
