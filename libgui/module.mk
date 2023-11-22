if AMCOND_BUILD_QT_GUI

%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

MOC_CPPFLAGS =

## Fix for bug #42839 where -mieee CFLAG option is added to CPPFLAGS by gnulib.
## Users may also pass other options in CPPFLAGS that moc does not understand.
## Only keep moc-compatible options -Idir, -Dmacro, and -Umacro.
MOC_OCTAVE_CPPFLAGS = $(filter -I% -D% -U%, $(AM_CPPFLAGS) $(CPPFLAGS))

octlib_LTLIBRARIES += %reldir%/liboctgui.la

TRANSLATIONS = \
  %reldir%/languages/be_BY.ts \
  %reldir%/languages/ca_ES.ts \
  %reldir%/languages/de_DE.ts \
  %reldir%/languages/en_US.ts \
  %reldir%/languages/es_ES.ts \
  %reldir%/languages/eu_ES.ts \
  %reldir%/languages/fr_FR.ts \
  %reldir%/languages/hu_HU.ts \
  %reldir%/languages/it_IT.ts \
  %reldir%/languages/ja_JP.ts \
  %reldir%/languages/lt_LT.ts \
  %reldir%/languages/nl_NL.ts \
  %reldir%/languages/pt_BR.ts \
  %reldir%/languages/pt_PT.ts \
  %reldir%/languages/ru_RU.ts \
  %reldir%/languages/tr_TR.ts \
  %reldir%/languages/uk_UA.ts \
  %reldir%/languages/zh_CN.ts

LOCALES = $(patsubst %reldir%/languages/%.ts, %reldir%/languages/%.qm, $(TRANSLATIONS))

noinst_HEADERS += \
  %reldir%/liboctgui-build-info.h

include %reldir%/src/module.mk
include %reldir%/graphics/module.mk
include %reldir%/qterminal/module.mk

nodist_%canon_reldir%_liboctgui_la_SOURCES = \
  %reldir%/liboctgui-build-info.cc

%canon_reldir%_liboctgui_la_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  @OCTGUI_DLL_DEFS@ \
  -Ilibgui \
  -I$(srcdir)/libgui

%canon_reldir%_liboctgui_la_LIBADD = \
  %reldir%/qterminal/libqterminal.la \
  %reldir%/graphics/libgraphics.la \
  %reldir%/src/libgui-src.la \
  libinterp/liboctinterp.la \
  liboctave/liboctave.la \
  $(LIBOCTGUI_LINK_DEPS)

## Increment the following version numbers as needed and according
## to the rules in the etc/HACKING.md file:

%canon_reldir%_liboctgui_current = 9
%canon_reldir%_liboctgui_revision = 0
%canon_reldir%_liboctgui_age = 0

%canon_reldir%_liboctgui_version_info = $(%canon_reldir%_liboctgui_current):$(%canon_reldir%_liboctgui_revision):$(%canon_reldir%_liboctgui_age)

%canon_reldir%_liboctgui_la_LDFLAGS = \
  -version-info $(%canon_reldir%_liboctgui_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  -bindir $(bindir) \
  $(LIBOCTGUI_LINK_OPTS) \
  $(WARN_LDFLAGS)

octlocale_DATA += $(LOCALES)

DIRSTAMP_FILES += \
  %reldir%/$(octave_dirstamp)

define moc-command
  rm -f $@-t $@ && \
  ( echo "#if defined (HAVE_CONFIG_H)"; \
    echo '#  include "config.h"'; \
    echo "#endif"; \
    $(MOC) $(MOCFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(MOC_OCTAVE_CPPFLAGS) $(MOC_CPPFLAGS) $(%canon_reldir%_liboctgui_la_CPPFLAGS) $< ) > $@-t && \
  mv $@-t $@
endef

define moc-h-command
$(SED) -e 's/OCTAVE_BEGIN_NAMESPACE *(\([^)]*\))/namespace \1 {/' \
         -e 's/OCTAVE_END_NAMESPACE *([^)]*)/}/' $< > $@
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
    QT_HASH_SEED=0 $(RCC) $(RCCFLAGS) -name $(@D) $< ) > $@-t && \
  mv $@-t $@
endef

.PRECIOUS: moc-%.h

moc-%.h: %.h
	$(AM_V_GEN)$(moc-h-command)

moc-%.cc: moc-%.h
	$(AM_V_GEN)$(moc-command)

ui-%.h: %.ui
	$(AM_V_GEN)$(UIC) $(UICFLAGS) -o $@ $<

qrc-%.cc: %.qrc
	$(AM_V_GEN)$(rcc-command)

AM_V_lrelease = $(am__v_lrelease_$(V))
am__v_lrelease_ = $(am__v_lrelease_$(AM_DEFAULT_VERBOSITY))
am__v_lrelease_0 = -silent
am__v_lrelease_1 =

%.qm: %.ts | %reldir%/languages/$(octave_dirstamp)
	$(AM_V_GEN)$(LRELEASE) $(LRELEASEFLAGS) $(AM_V_lrelease) -qm $@ $<

DIRSTAMP_FILES += \
  %reldir%/languages/$(octave_dirstamp)

%canon_reldir%_EXTRA_DIST += \
  $(TRANSLATIONS) \
  %reldir%/liboctgui-build-info.in.cc

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_CLEANFILES += \
  $(LOCALES) \
  %reldir%/liboctgui-build-info.cc

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

libgui-clean:
	rm -f $(%canon_reldir%_CLEANFILES)

libgui-distclean: libgui-clean
	rm -f $(%canon_reldir%_DISTCLEANFILES)

libgui-maintainer-clean: libgui-distclean
	rm -f $(%canon_reldir%_MAINTAINERCLEANFILES)

%reldir%/liboctgui-build-info.cc: %reldir%/liboctgui-build-info.in.cc HG-ID | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(build-info-commands)

endif
