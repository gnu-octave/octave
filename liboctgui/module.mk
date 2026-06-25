## Entire file is used only if Qt GUI is enabled.
if AMCOND_BUILD_QT_GUI

%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

%canon_reldir%_liboctgui_la_LIBADD =

MOC_CPPFLAGS =

include %reldir%/graphics/module.mk
include %reldir%/qterminal/module.mk
include %reldir%/src/module.mk

nodist_%canon_reldir%_liboctgui_la_SOURCES = \
  %reldir%/liboctgui-build-info.cc

## Start library specification
octlib_LTLIBRARIES += %reldir%/liboctgui.la

%canon_reldir%_liboctgui_la_CPPFLAGS := \
  $(AM_CPPFLAGS) \
  @OCTGUI_DLL_DEFS@ \
  -I%reldir% -I$(srcdir)/%reldir%

%canon_reldir%_liboctgui_la_LIBADD += \
  liboctinterp/liboctinterp.la \
  liboctave/liboctave.la \
  $(LIBOCTGUI_LINK_DEPS)

noinst_HEADERS += \
  %reldir%/liboctgui-build-info.h

TRANSLATIONS = \
  %reldir%/languages/ast_ES.ts \
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
  %reldir%/languages/sv_SE.ts \
  %reldir%/languages/tr_TR.ts \
  %reldir%/languages/uk_UA.ts \
  %reldir%/languages/zh_CN.ts

LOCALES := $(patsubst %reldir%/languages/%.ts, %reldir%/languages/%.qm, $(TRANSLATIONS))

octlocale_DATA += $(LOCALES)

## Increment the following version numbers as needed and
## according to the rules in the etc/HACKING.md file:

%canon_reldir%_liboctgui_current = 14
%canon_reldir%_liboctgui_revision = 1
%canon_reldir%_liboctgui_age = 0

%canon_reldir%_liboctgui_version_info := $(%canon_reldir%_liboctgui_current):$(%canon_reldir%_liboctgui_revision):$(%canon_reldir%_liboctgui_age)

%canon_reldir%_liboctgui_la_LDFLAGS := \
  $(AM_LDFLAGS) \
  $(WARN_LDFLAGS) \
  $(NO_UNDEFINED_LDFLAG) \
  -version-info $(%canon_reldir%_liboctgui_version_info) \
  -bindir $(bindir) \
  $(LIBOCTGUI_LINK_OPTS)

## Special rules:

## Fix for bug #42839 where -mieee CFLAG option is added to CPPFLAGS by gnulib.
## Users may also pass other options in CPPFLAGS that moc does not understand.
## Only keep moc-compatible options -Idir, -Dmacro, and -Umacro.
MOC_OCTAVE_CPPFLAGS := $(filter -I% -D% -U%, $(AM_CPPFLAGS) $(CPPFLAGS))

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

%reldir%/liboctgui-build-info.cc: %reldir%/liboctgui-build-info.in.cc HG-ID | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(lib-build-info-commands)

DIRSTAMP_FILES += \
  %reldir%/$(octave_dirstamp) \
  %reldir%/languages/$(octave_dirstamp)

%canon_reldir%_EXTRA_DIST += \
  %reldir%/liboctgui-build-info.in.cc \
  $(TRANSLATIONS)

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_DISTCLEANFILES += \
  %reldir%/liboctgui-build-info.cc \
  $(LOCALES)

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

liboctgui-clean:
	$(FIND) %reldir% -type d \( -name '.libs' -o -name '_libs' \) -prune -exec rm -rf {} +
	$(FIND) %reldir% \( -name '*.o' -o -name '*.lo' -o -name '*.la' \) -delete
	$(FIND) %reldir% -name 'so_locations' -delete
	rm -f $(%canon_reldir%_CLEANFILES)

endif
