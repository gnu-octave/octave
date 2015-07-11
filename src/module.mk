## Search local directories before those specified by the user.

SRC_DIR_CPPFLAGS = \
  -I$(top_srcdir)/liboctave/array \
  -I$(top_srcdir)/liboctave/cruft/misc \
  -I$(top_srcdir)/liboctave/numeric \
  -I$(top_srcdir)/liboctave/system \
  -I$(top_srcdir)/liboctave/util \
  -I$(top_srcdir)/libinterp \
  -I$(top_builddir)/libinterp/corefcn -I$(top_srcdir)/libinterp/corefcn \
  -I$(top_srcdir)/src \
  -I$(top_builddir)/libgnu -I$(top_srcdir)/libgnu

EXTRA_DIST += \
  src/main.in.cc \
  src/mkoctfile.in.cc \
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
  $(bin_programs) \
  $(OCTAVE_VERSION_LINKS)

noinst_HEADERS += \
  src/display-available.h	\
  src/shared-fcns.h

OCTAVE_VERSION_LINKS += src/octave-cli-$(version)$(EXEEXT)

if AMCOND_BUILD_GUI
  archlib_PROGRAMS += src/octave-gui
  OCTAVE_VERSION_LINKS += src/octave-gui-$(version)$(EXEEXT)

  OCTAVE_INTERPRETER_TARGETS += src/octave-gui
endif

OCTAVE_CORE_LIBS = \
  $(top_builddir)/libinterp/liboctinterp.la \
  $(top_builddir)/liboctave/liboctave.la

nodist_src_octave_SOURCES = src/main.cc
src_octave_SOURCES = src/display-available.c

src_octave_LDADD = \
  $(top_builddir)/libgnu/libgnu.la \
  $(X11_LIBS) \
  $(CARBON_LIBS) \
  $(GNULIB_LINK_DEPS)

src_octave_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_LINK_OPTS)

if AMCOND_BUILD_GUI
  OCTAVE_CPPFLAGS = -DHAVE_OCTAVE_GUI
endif

src_octave_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

src_octave_CXXFLAGS = \
  $(AM_CXXFLAGS) \
  $(WARN_CXXFLAGS)

src_octave_cli_SOURCES = src/main-cli.cc

src_octave_cli_LDADD = \
  $(OCTAVE_CORE_LIBS) \
  $(OCTAVE_LINK_DEPS)

src_octave_cli_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_LINK_OPTS)

src_octave_cli_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

src_octave_cli_CXXFLAGS = \
  $(AM_CXXFLAGS) \
  $(WARN_CXXFLAGS)

if AMCOND_BUILD_GUI
  src_octave_gui_SOURCES = src/main-gui.cc
  OCTAVE_GUI_LIBS = $(top_builddir)/libgui/liboctgui.la
  OCTAVE_GUI_CPPFLAGS = -I$(top_srcdir)/libgui/src
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
  $(OCTAVE_GUI_LINK_OPTS)

src_octave_gui_CXXFLAGS = \
  $(AM_CXXFLAGS) \
  $(WARN_CXXFLAGS)

src_mkoctfile_SOURCES =

nodist_src_mkoctfile_SOURCES = src/mkoctfile.cc

src_mkoctfile_LDADD = $(top_builddir)/libgnu/libgnu.la $(LIBS)

src_mkoctfile_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

src_mkoctfile_CXXFLAGS = \
  $(AM_CXXFLAGS) \
  $(WARN_CXXFLAGS)

src_octave_config_SOURCES =

nodist_src_octave_config_SOURCES = src/octave-config.cc

src_octave_config_LDADD = \
  $(top_builddir)/libinterp/corefcn/libcorefcn.la \
  $(top_builddir)/libgnu/libgnu.la \
  $(LIBS)

src_octave_config_CPPFLAGS = \
  $(SRC_DIR_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

src_octave_config_CXXFLAGS = \
  $(AM_CXXFLAGS) \
  $(WARN_CXXFLAGS)

DIRSTAMP_FILES += src/$(octave_dirstamp)

if AMCOND_CROSS_TOOLS

## Building cross mkoctfile.

OCTAVE_CROSS_TOOLS += src/$(host_triplet)-mkoctfile$(BUILD_EXEEXT)

src/$(host_triplet)-mkoctfile$(BUILD_EXEEXT): src/$(host_triplet)-mkoctfile.cc
	$(BUILD_CXX) -o src/$(host_triplet)-mkoctfile$(BUILD_EXEEXT) -Dgnulib='' -Doctave_idx_type=int $(DEFAULT_INCLUDES) $(BUILD_CXXFLAGS) $(BUILD_LDFLAGS) src/$(host_triplet)-mkoctfile.cc

src/$(host_triplet)-mkoctfile.cc: src/mkoctfile.in.cc Makefile src/$(octave_dirstamp)
	$(AM_V_GEN)$(do_subst_cross_config_vals)

## Building cross octave-config.

OCTAVE_CROSS_TOOLS += src/$(host_triplet)-octave-config$(BUILD_EXEEXT)

src/$(host_triplet)-octave-config$(BUILD_EXEEXT): src/$(host_triplet)-octave-config.cc
	$(BUILD_CXX) -o src/$(host_triplet)-octave-config$(BUILD_EXEEXT) -Dgnulib='' -Doctave_idx_type=int $(DEFAULT_INCLUDES) $(BUILD_CXXFLAGS) $(BUILD_LDFLAGS) src/$(host_triplet)-octave-config.cc

src/$(host_triplet)-octave-config.cc: src/octave-config.in.cc Makefile src/$(octave_dirstamp)
	$(AM_V_GEN)$(do_subst_default_vals)

mostlyclean-local:
	-rm -f $(OCTAVE_CROSS_TOOLS)

endif

src/octave-config.cc: src/octave-config.in.cc Makefile src/$(octave_dirstamp)
	$(AM_V_GEN)$(do_subst_default_vals)

src/mkoctfile.cc: src/mkoctfile.in.cc Makefile src/$(octave_dirstamp)
	$(AM_V_GEN)$(do_subst_config_vals)

## main.cc must depend on Makefile.  Calling configure may change
## default/config values.  However, calling configure will also
## regenerate the Makefiles from Makefile.am and trigger the rules below.

src/main.cc: src/main.in.cc Makefile src/$(octave_dirstamp)
	$(AM_V_GEN)$(do_subst_default_vals)

ALL_LOCAL_TARGETS += $(OCTAVE_VERSION_LINKS) $(OCTAVE_CROSS_TOOLS)

install-exec-hook: make-version-links

uninstall-local: remove-version-links

make-version-links:
	cd $(DESTDIR)$(bindir) && \
	for f in $(basename $(bin_PROGRAMS)); do \
	  mv $$f$(EXEEXT) $$f-$(version)$(EXEEXT) && \
	    $(LN_S) $$f-$(version)$(EXEEXT) $$f$(EXEEXT); \
	done

remove-version-links:
	for f in $(basename $(bin_PROGRAMS)); do \
	  rm -f $(DESTDIR)$(bindir)/$$f-$(version)$(EXEEXT); \
	done

.PHONY: make-version-links remove-version-links

## We need these file names in the build tree because the wrapper
## program (main.cc) will try to invoke the versioned binaries.

src/octave-cli-$(version)$(EXEEXT): src/octave-cli$(EXEEXT)
	$(AM_V_GEN)rm -f $@ && \
	$(LN_S) $(<F) $@

src/octave-gui-$(version)$(EXEEXT): src/octave-gui$(EXEEXT)
	$(AM_V_GEN)rm -f $@ && \
	$(LN_S) $(<F) $@

CLEANFILES += \
  $(OCTAVE_VERSION_LINKS)
