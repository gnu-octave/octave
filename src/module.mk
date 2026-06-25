bin_PROGRAMS += \
  %reldir%/mkoctfile \
  %reldir%/octave \
  %reldir%/octave-cli \
  %reldir%/octave-config

## Search local directories before those specified by the user.
SRC_DIR_CPPFLAGS := \
  -I$(srcdir)/%reldir% \
  -Iliboctave -I$(srcdir)/liboctave \
  -I$(srcdir)/liboctave/array \
  -Iliboctave/numeric -I$(srcdir)/liboctave/numeric \
  -Iliboctave/operators -I$(srcdir)/liboctave/operators \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util \
  -I$(srcdir)/liboctave/wrappers \
  -Iliboctinterp -I$(srcdir)/liboctinterp \
  -I$(srcdir)/liboctinterp/corefcn \
  -I$(srcdir)/liboctinterp/corefcn/numeric \
  -I$(srcdir)/liboctinterp/corefcn/system \
  -Iliboctinterp/corefcn/util -I$(srcdir)/liboctinterp/corefcn/util \
  -Iliboctinterp/graphics -I$(srcdir)/liboctinterp/graphics \
  -Iliboctinterp/interp -I$(srcdir)/liboctinterp/interp \
  -I$(srcdir)/liboctinterp/load-save \
  -I$(srcdir)/liboctinterp/octave-value \
  -I$(srcdir)/liboctinterp/operators \
  -Iliboctinterp/parse-tree -I$(srcdir)/liboctinterp/parse-tree \
  -I$(srcdir)/liboctinterp/stream \
  -I$(srcdir)/liboctinterp/template-inst \
  -I$(srcdir)/liboctmex

OCTAVE_INTERPRETER_TARGETS += \
  $(bin_PROGRAMS) \
  $(OCTAVE_VERSION_LINKS)

octinclude_HEADERS += \
  %reldir%/octave-build-info.h

noinst_HEADERS += \
  %reldir%/display-available.h \
  %reldir%/octave-qsvghandler.h \
  %reldir%/shared-fcns.h \
  %reldir%/shared-sysdep.h

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

## Order is important here, last resolving symbols should be at the end.
OCTAVE_CORE_LIBS = \
  liboctmex/liboctmex.la \
  liboctinterp/liboctinterp.la \
  liboctave/liboctave.la \
  libgnu/libgnu.la

if AMCOND_BUILD_QT_GUI
  OCTAVE_CPPFLAGS = -DHAVE_OCTAVE_QT_GUI
endif

## Specify build of "octave"

nodist_%canon_reldir%_octave_SOURCES = %reldir%/main.cc

%canon_reldir%_octave_SOURCES = %reldir%/display-available.c

%canon_reldir%_octave_CPPFLAGS := \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS) \
  $(WAYLAND_CLIENT_CPPFLAGS)

%canon_reldir%_octave_LDFLAGS := \
  $(AM_LDFLAGS) \
  $(WARN_LDFLAGS) \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_UNICODE_EXE_LDFLAGS) \
  $(WAYLAND_CLIENT_LDFLAGS) \
  $(OCTAVE_LINK_OPTS)

%canon_reldir%_octave_LDADD := \
  liboctave/wrappers/libwrappers.la \
  libgnu/libgnu.la \
  $(X11_LIBS) \
  $(WAYLAND_CLIENT_LIBS) \
  $(CARBON_LIBS) \
  $(GNULIB_LINK_DEPS)

## Specify build of "octave-cli"

%canon_reldir%_octave_cli_SOURCES = %reldir%/main-cli.cc
nodist_%canon_reldir%_octave_cli_SOURCES = %reldir%/octave-build-info.cc

%canon_reldir%_octave_cli_CPPFLAGS := \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

%canon_reldir%_octave_cli_LDFLAGS := \
  $(WARN_LDFLAGS) \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_UNICODE_EXE_LDFLAGS) \
  $(OCTAVE_CLI_LTLDFLAGS) \
  $(OCTAVE_LINK_OPTS)

%canon_reldir%_octave_cli_LDADD := \
  $(OCTAVE_CORE_LIBS) \
  $(OCTAVE_LINK_DEPS)

## Specify build of "octave-gui"

if AMCOND_BUILD_QT_GUI
  %canon_reldir%_octave_gui_SOURCES = %reldir%/main-gui.cc
  nodist_%canon_reldir%_octave_gui_SOURCES = %reldir%/octave-build-info.cc
  OCTAVE_GUI_LIBS = liboctgui/liboctgui.la
  OCTAVE_GUI_CPPFLAGS := -I$(srcdir)/liboctgui -Iliboctgui/src -I$(srcdir)/liboctgui/src
endif

%canon_reldir%_octave_gui_CPPFLAGS := \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_GUI_CPPFLAGS)

%canon_reldir%_octave_gui_LDFLAGS := \
  $(WARN_LDFLAGS) \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_UNICODE_EXE_LDFLAGS) \
  $(OCTAVE_GUI_LTLDFLAGS) \
  $(OCTAVE_GUI_LINK_OPTS)

%canon_reldir%_octave_gui_LDADD := \
  $(OCTAVE_GUI_LIBS) \
  $(OCTAVE_CORE_LIBS) \
  $(OCTAVE_GUI_LINK_DEPS)

## Specify build of "octave-svgconvert"

%canon_reldir%_octave_svgconvert_SOURCES = %reldir%/octave-svgconvert.cc

%canon_reldir%_octave_svgconvert_CPPFLAGS := $(QT_CPPFLAGS)

%canon_reldir%_octave_svgconvert_LDFLAGS := \
  $(QT_LDFLAGS) \
  $(OCTAVE_UNICODE_EXE_LDFLAGS)

%canon_reldir%_octave_svgconvert_LDADD := $(QT_LIBS)

## Specify build of "mkoctfile"

%canon_reldir%_mkoctfile_SOURCES =

nodist_%canon_reldir%_mkoctfile_SOURCES = %reldir%/mkoctfile.cc

%canon_reldir%_mkoctfile_CPPFLAGS := \
  -DOCTAVE_MEX_SOVERSION="$(OCTAVE_LIBOCTMEX_SOVERSION_MAJOR)" \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

%canon_reldir%_mkoctfile_LDFLAGS := \
  $(OCTAVE_UNICODE_EXE_LDFLAGS)

%canon_reldir%_mkoctfile_LDADD := \
  liboctave/wrappers/libwrappers.la \
  libgnu/libgnu.la \
  $(LIBS)

## Specify build of "octave-config"

%canon_reldir%_octave_config_SOURCES =

nodist_%canon_reldir%_octave_config_SOURCES = %reldir%/octave-config.cc

%canon_reldir%_octave_config_CPPFLAGS := \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

%canon_reldir%_octave_config_LDFLAGS := \
  $(OCTAVE_UNICODE_EXE_LDFLAGS)

%canon_reldir%_octave_config_LDADD = \
  libgnu/libgnu.la \
  $(LIBS)

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
	$(BUILD_CXX) -o %reldir%/$(host_triplet)-mkoctfile$(BUILD_EXEEXT) $(OCTAVE_REPLACE_PREFIX_CPPFLAGS) -DOCTAVE_MEX_SOVERSION="$(OCTAVE_LIBOCTMEX_SOVERSION_MAJOR)" -DCROSS=1 $(DEFAULT_INCLUDES) -I$(srcdir)/src $(BUILD_CXXFLAGS) $(BUILD_LDFLAGS) %reldir%/$(host_triplet)-mkoctfile.cc

%reldir%/$(host_triplet)-mkoctfile.cc: %reldir%/mkoctfile.in.cc build-aux/subst-cross-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-cross-config-vals.sh)

## Building cross octave-config.

OCTAVE_CROSS_TOOLS += %reldir%/$(host_triplet)-octave-config$(BUILD_EXEEXT)

%reldir%/$(host_triplet)-octave-config$(BUILD_EXEEXT): %reldir%/$(host_triplet)-octave-config.cc
	$(BUILD_CXX) -o %reldir%/$(host_triplet)-octave-config$(BUILD_EXEEXT) -DCROSS=1 $(DEFAULT_INCLUDES) -I$(srcdir)/src $(BUILD_CXXFLAGS) $(BUILD_LDFLAGS) %reldir%/$(host_triplet)-octave-config.cc

%reldir%/$(host_triplet)-octave-config.cc: %reldir%/octave-config.in.cc build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

## Add CROSS_TOOLS to list of targets to build
ALL_LOCAL_TARGETS += $(OCTAVE_CROSS_TOOLS)

src-mostlyclean-local:
	-rm -f $(OCTAVE_CROSS_TOOLS)

else

src-mostlyclean-local:

endif

## Special rules:

%reldir%/octave-config.cc: %reldir%/octave-config.in.cc build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

%reldir%/mkoctfile.cc: %reldir%/mkoctfile.in.cc build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

%reldir%/main.cc: %reldir%/main.in.cc build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

%reldir%/octave-build-info.cc: %reldir%/octave-build-info.in.cc HG-ID | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(lib-build-info-commands)

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

EXTRA_DIST += \
  %reldir%/main.in.cc \
  %reldir%/mkoctfile.in.cc \
  %reldir%/octave-build-info.in.cc \
  %reldir%/octave-config.in.cc

%canon_reldir%_DISTCLEANFILES = \
  %reldir%/main.cc \
  %reldir%/mkoctfile.cc \
  %reldir%/octave-build-info.cc \
  %reldir%/octave-config.cc \
  $(OCTAVE_VERSION_LINKS)

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

src-clean:
	rm -f $(bin_PROGRAMS)
	rm -f $(archlib_PROGRAMS)
	rm -rf %reldir%/.libs %reldir%/_libs
	rm -f %reldir%/*.o

src-distclean: src-clean
	rm -f $(%canon_reldir%_DISTCLEANFILES)
