if AMCOND_BUILD_QT_GUI

libgui_EXTRA_DIST =

libgui_CLEANFILES =
libgui_DISTCLEANFILES =
libgui_MAINTAINERCLEANFILES =

MOC_CPPFLAGS =

## Fix for bug #42839 where -mieee CFLAG option is added to CPPFLAGS by gnulib.
## Users may also pass other options in CPPFLAGS that moc does not understand.
## Only keep moc-compatible options -Idir, -Dmacro, and -Umacro.
MOC_OCTAVE_CPPFLAGS = $(filter -I% -D% -U%, $(AM_CPPFLAGS) $(CPPFLAGS))

octlib_LTLIBRARIES += libgui/liboctgui.la

TRANSLATIONS = \
  libgui/languages/be_BY.ts \
  libgui/languages/de_DE.ts \
  libgui/languages/en_US.ts \
  libgui/languages/es_ES.ts \
  libgui/languages/eu_ES.ts \
  libgui/languages/fr_FR.ts \
  libgui/languages/it_IT.ts \
  libgui/languages/ja_JP.ts \
  libgui/languages/nl_NL.ts \
  libgui/languages/pt_BR.ts \
  libgui/languages/pt_PT.ts \
  libgui/languages/ru_RU.ts \
  libgui/languages/uk_UA.ts \
  libgui/languages/zh_CN.ts

LOCALES = $(patsubst libgui/languages/%.ts, libgui/languages/%.qm, $(TRANSLATIONS))

include libgui/src/module.mk
include libgui/graphics/module.mk
include libgui/qterminal-module.mk

## liboctgui merely collects a bunch of compiled convenience libraries.
## It has no source code itself.
libgui_liboctgui_la_SOURCES =

# Dummy C++ source to force C++ linking.
EXTRA_libgui_liboctgui_la_SOURCES = libgui/.dummy_force_cxx_link.cc

libgui_liboctgui_la_LIBADD = \
  libgui/qterminal/libqterminal.la \
  libgui/src/libgui-src.la \
  libgui/graphics/libgui-graphics.la \
  libinterp/liboctinterp.la \
  liboctave/liboctave.la \
  $(LIBOCTGUI_LINK_DEPS)

# Increment these as needed and according to the rules in the libtool manual:
libgui_liboctgui_current = 1
libgui_liboctgui_revision = 0
libgui_liboctgui_age = 0

libgui_liboctgui_version_info = $(libgui_liboctgui_current):$(libgui_liboctgui_revision):$(libgui_liboctgui_age)

libgui_liboctgui_la_LDFLAGS = \
  -version-info $(libgui_liboctgui_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  -bindir $(bindir) \
  $(LIBOCTGUI_LINK_OPTS) \
  $(WARN_LDFLAGS)

octetc_DATA += libgui/default-qt-settings

octlocale_DATA += $(LOCALES)

libgui/default-qt-settings: libgui/default-qt-settings.in build-aux/mk-default-qt-settings.sh | libgui/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/mk-default-qt-settings.sh)

DIRSTAMP_FILES += \
  libgui/$(octave_dirstamp)

define moc-command
  rm -f $@-t $@ && \
  ( echo "#if defined (HAVE_CONFIG_H)"; \
    echo '#  include "config.h"'; \
    echo "#endif"; \
    $(MOC) $(MOCFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(MOC_OCTAVE_CPPFLAGS) $(MOC_CPPFLAGS) $(libgui_liboctgui_la_CPPFLAGS) $< ) > $@-t && \
  mv $@-t $@
endef

define rcc-command
  rm -f $@-t $@ && \
  ( echo "#if defined (HAVE_CONFIG_H)"; \
    echo '#  include "config.h"'; \
    echo "#endif"; \
    echo "// Ignore unused variable warnings in generated code."; \
    echo "#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)"; \
    echo "#pragma GCC diagnostic ignored \"-Wunused-variable\""; \
    echo "#endif"; \
    $(RCC) $(RCCFLAGS) -name $(@D) $< ) > $@-t && \
  mv $@-t $@
endef

moc-%.cc: %.h
	$(AM_V_GEN)$(moc-command)

ui-%.h: %.ui
	$(AM_V_GEN)$(UIC) $(UICFLAGS) -o $@ $<

qrc-%.cc: %.qrc
	$(AM_V_GEN)$(rcc-command)

AM_V_lrelease = $(am__v_lrelease_$(V))
am__v_lrelease_ = $(am__v_lrelease_$(AM_DEFAULT_VERBOSITY))
am__v_lrelease_0 = -silent
am__v_lrelease_1 =

%.qm: %.ts | libgui/languages/$(octave_dirstamp)
	$(AM_V_GEN)$(LRELEASE) $(LRELEASEFLAGS) $(AM_V_lrelease) -qm $@ $<

DIRSTAMP_FILES += \
  libgui/languages/$(octave_dirstamp)

libgui_EXTRA_DIST += \
  $(TRANSLATIONS) \
  libgui/default-qt-settings.in

EXTRA_DIST += $(libgui_EXTRA_DIST)

libgui_DISTCLEANFILES += \
  libgui/default-qt-settings \
  $(LOCALES)

CLEANFILES += $(libgui_CLEANFILES)
DISTCLEANFILES += $(libgui_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(libgui_MAINTAINERCLEANFILES)

libgui-clean:
	rm -f $(libgui_CLEANFILES)

libgui-distclean: libgui-clean
	rm -f $(libgui_DISTCLEANFILES)

libgui-maintainer-clean: libgui-distclean
	rm -f $(libgui_MAINTAINERCLEANFILES)
endif
