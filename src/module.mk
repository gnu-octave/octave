%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

## Search local directories before those specified by the user.

SRC_DIR_CPPFLAGS = \
  -Iliboctave -I$(srcdir)/liboctave \
  -I$(srcdir)/liboctave/array \
  -I$(srcdir)/liboctave/numeric \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util \
  -Iliboctave/wrappers -I$(srcdir)/liboctave/wrappers \
  -Ilibinterp -I$(srcdir)/libinterp \
  -Ilibinterp/corefcn -I$(srcdir)/libinterp/corefcn \
  -I$(srcdir)/src

EXTRA_DIST += \
  %reldir%/main.in.cc \
  %reldir%/mkoctfile.in.cc \
  %reldir%/octave-build-info.in.cc \
  %reldir%/octave-config.in.cc

bin_PROGRAMS += \
  %reldir%/mkoctfile \
  %reldir%/octave \
  %reldir%/octave-cli \
  %reldir%/octave-config

OCTAVE_INTERPRETER_TARGETS += \
  $(bin_PROGRAMS) \
  $(OCTAVE_VERSION_LINKS)

octinclude_HEADERS += \
  %reldir%/octave-build-info.h

noinst_HEADERS += \
  %reldir%/display-available.h \
  %reldir%/octave-qsvghandler.h \
  %reldir%/shared-fcns.h

OCTAVE_VERSION_LINKS += %reldir%/octave-cli-$(version)$(EXEEXT)

if AMCOND_BUILD_QT_GUI
  archlib_PROGRAMS += %reldir%/octave-gui
  OCTAVE_VERSION_LINKS += %reldir%/octave-gui-$(version)$(EXEEXT)

  OCTAVE_INTERPRETER_TARGETS += %reldir%/octave-gui$(EXEEXT)
endif

if AMCOND_BUILD_QT_GUI
  archlib_PROGRAMS += %reldir%/octave-svgconvert
  OCTAVE_INTERPRETER_TARGETS += %reldir%/octave-svgconvert$(EXEEXT)
endif

OCTAVE_CORE_LIBS = \
  libinterp/liboctinterp.la \
  liboctave/liboctave.la \
  libgnu/libgnu.la

nodist_%canon_reldir%_octave_SOURCES = %reldir%/main.cc

%canon_reldir%_octave_SOURCES = %reldir%/display-available.c

%canon_reldir%_octave_LDADD = \
  liboctave/wrappers/libwrappers.la \
  libgnu/libgnu.la \
  $(X11_LIBS) \
  $(CARBON_LIBS) \
  $(GNULIB_LINK_DEPS)

%canon_reldir%_octave_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_LINK_OPTS) \
  $(WARN_LDFLAGS) \
  $(OCTAVE_UNICODE_EXE_LDFLAGS)

if AMCOND_BUILD_QT_GUI
  OCTAVE_CPPFLAGS = -DHAVE_OCTAVE_QT_GUI
endif

%canon_reldir%_octave_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

%canon_reldir%_octave_cli_SOURCES = %reldir%/main-cli.cc
nodist_%canon_reldir%_octave_cli_SOURCES = %reldir%/octave-build-info.cc

%canon_reldir%_octave_cli_LDADD = \
  $(OCTAVE_CORE_LIBS) \
  $(OCTAVE_LINK_DEPS)

%canon_reldir%_octave_cli_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_LINK_OPTS) \
  $(WARN_LDFLAGS) \
  $(OCTAVE_UNICODE_EXE_LDFLAGS)

%canon_reldir%_octave_cli_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

if AMCOND_BUILD_QT_GUI
  %canon_reldir%_octave_gui_SOURCES = %reldir%/main-gui.cc
  nodist_%canon_reldir%_octave_gui_SOURCES = %reldir%/octave-build-info.cc
  OCTAVE_GUI_LIBS = libgui/liboctgui.la
  OCTAVE_GUI_CPPFLAGS = -I$(srcdir)/libgui -Ilibgui/src -I$(srcdir)/libgui/src
endif

%canon_reldir%_octave_gui_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_GUI_CPPFLAGS)

%canon_reldir%_octave_gui_LDADD = \
  $(OCTAVE_GUI_LIBS) \
  $(OCTAVE_CORE_LIBS) \
  $(OCTAVE_GUI_LINK_DEPS)

%canon_reldir%_octave_gui_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_GUI_LINK_OPTS) \
  $(WARN_LDFLAGS) \
  $(OCTAVE_UNICODE_EXE_LDFLAGS)

%canon_reldir%_octave_svgconvert_SOURCES = %reldir%/octave-svgconvert.cc

%canon_reldir%_octave_svgconvert_CPPFLAGS = $(QT_CPPFLAGS)

%canon_reldir%_octave_svgconvert_LDADD = $(QT_LIBS)

%canon_reldir%_octave_svgconvert_LDFLAGS = \
  $(QT_LDFLAGS) \
  $(OCTAVE_UNICODE_EXE_LDFLAGS)

%canon_reldir%_mkoctfile_SOURCES =

nodist_%canon_reldir%_mkoctfile_SOURCES = %reldir%/mkoctfile.cc

%canon_reldir%_mkoctfile_LDADD = \
  liboctave/wrappers/libwrappers.la \
  libgnu/libgnu.la $(LIBS)

%canon_reldir%_mkoctfile_LDFLAGS = \
  $(OCTAVE_UNICODE_EXE_LDFLAGS)

%canon_reldir%_mkoctfile_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

%canon_reldir%_octave_config_SOURCES =

nodist_%canon_reldir%_octave_config_SOURCES = %reldir%/octave-config.cc

%canon_reldir%_octave_config_LDADD = \
  libinterp/corefcn/libcorefcn.la \
  libgnu/libgnu.la \
  $(LIBS)

%canon_reldir%_octave_config_LDFLAGS = \
  $(OCTAVE_UNICODE_EXE_LDFLAGS)

%canon_reldir%_octave_config_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

mostlyclean-local: src-mostlyclean-local
.PHONY: src-mostlyclean-local

if AMCOND_CROSS_TOOLS

## Building cross mkoctfile.

OCTAVE_CROSS_TOOLS += %reldir%/$(host_triplet)-mkoctfile$(BUILD_EXEEXT)

if AMCOND_RELOCATE_ALL
  OCTAVE_REPLACE_PREFIX_CPPFLAGS = -DOCTAVE_REPLACE_PREFIX
endif

%reldir%/$(host_triplet)-mkoctfile$(BUILD_EXEEXT): %reldir%/$(host_triplet)-mkoctfile.cc
	$(BUILD_CXX) -o %reldir%/$(host_triplet)-mkoctfile$(BUILD_EXEEXT) $(OCTAVE_REPLACE_PREFIX_CPPFLAGS) -DCROSS=1 $(DEFAULT_INCLUDES) $(BUILD_CXXFLAGS) $(BUILD_LDFLAGS) -I$(srcdir)/src %reldir%/$(host_triplet)-mkoctfile.cc

%reldir%/$(host_triplet)-mkoctfile.cc: %reldir%/mkoctfile.in.cc build-aux/subst-cross-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-cross-config-vals.sh)

## Building cross octave-config.

OCTAVE_CROSS_TOOLS += %reldir%/$(host_triplet)-octave-config$(BUILD_EXEEXT)

%reldir%/$(host_triplet)-octave-config$(BUILD_EXEEXT): %reldir%/$(host_triplet)-octave-config.cc
	$(BUILD_CXX) -o %reldir%/$(host_triplet)-octave-config$(BUILD_EXEEXT) -DCROSS=1 $(DEFAULT_INCLUDES) $(BUILD_CXXFLAGS) $(BUILD_LDFLAGS) -I$(srcdir)/src %reldir%/$(host_triplet)-octave-config.cc

%reldir%/$(host_triplet)-octave-config.cc: %reldir%/octave-config.in.cc build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

src-mostlyclean-local:
	-rm -f $(OCTAVE_CROSS_TOOLS)

else

src-mostlyclean-local:

endif

%reldir%/octave-config.cc: %reldir%/octave-config.in.cc build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

%reldir%/mkoctfile.cc: %reldir%/mkoctfile.in.cc build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

%reldir%/main.cc: %reldir%/main.in.cc build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

%reldir%/octave-build-info.cc: %reldir%/octave-build-info.in.cc HG-ID | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(build-info-commands)

ALL_LOCAL_TARGETS += $(OCTAVE_CROSS_TOOLS)

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

%reldir%/octave-cli-$(version)$(EXEEXT): %reldir%/octave-cli$(EXEEXT)
	$(AM_V_GEN)rm -f $@ && \
	cd $(@D) && $(LN_S) $(<F) $(@F)

%reldir%/octave-gui-$(version)$(EXEEXT): %reldir%/octave-gui$(EXEEXT)
	$(AM_V_GEN)rm -f $@ && \
	cd $(@D) && $(LN_S) $(<F) $(@F)

%canon_reldir%_CLEANFILES += \
  $(OCTAVE_VERSION_LINKS) \
  %reldir%/main.cc \
  %reldir%/mkoctfile.cc \
  %reldir%/octave-build-info.cc \
  %reldir%/octave-config.cc

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

src-clean:
	rm -f $(%canon_reldir%_CLEANFILES)

src-distclean: src-clean
	rm -f $(%canon_reldir%_DISTCLEANFILES)

src-maintainer-clean: src-distclean
	rm -f $(%canon_reldir%_MAINTAINERCLEANFILES)
