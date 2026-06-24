BUILT_SOURCES += \
  %reldir%/graphics.h

GRAPHICS_INC = \
  %reldir%/base-text-renderer.h \
  %reldir%/ft-text-renderer.h \
  %reldir%/gh-manager.h \
  %reldir%/gl-render.h \
  %reldir%/gl2ps-print.h \
  %reldir%/graphics-handle.h \
  %reldir%/graphics-toolkit.h \
  %reldir%/gtk-manager.h \
  %reldir%/latex-text-renderer.h \
  %reldir%/oct-handle.h \
  %reldir%/octave-default-image.h \
  %reldir%/text-engine.h \
  %reldir%/text-renderer.h

NOINSTALL_GRAPHICS_INC = \
  %reldir%/graphics-utils.h \
  %reldir%/oct-opengl.h

GRAPHICS_SRC = \
  %reldir%/base-text-renderer.cc \
  %reldir%/ft-text-renderer.cc \
  %reldir%/gh-manager.cc \
  %reldir%/gl-render.cc \
  %reldir%/gl2ps-print.cc \
  %reldir%/graphics-toolkit.cc \
  %reldir%/graphics-utils.cc \
  %reldir%/graphics.cc \
  %reldir%/gtk-manager.cc \
  %reldir%/latex-text-renderer.cc \
  %reldir%/oct-tex-lexer.ll \
  %reldir%/oct-tex-parser.yy \
  %reldir%/text-engine.cc \
  %reldir%/text-renderer.cc \
  $(NOINSTALL_GRAPHICS_INC)

noinst_LTLIBRARIES += %reldir%/libgraphics.la

%canon_reldir%_libgraphics_la_SOURCES := $(GRAPHICS_SRC)

%canon_reldir%_libgraphics_la_CPPFLAGS = \
  $(liboctinterp_liboctinterp_la_CPPFLAGS) \
  $(FONTCONFIG_CPPFLAGS) \
  $(FT2_CPPFLAGS) \
  $(OCTAVE_TEX_PARSER_CPPFLAGS) \
  $(WAYLAND_CLIENT_CPPFLAGS)

liboctinterp_liboctinterp_la_LIBADD += %reldir%/libgraphics.la

## Special rules for sources which must be built before rest of compilation.

%reldir%/graphics.h: %reldir%/graphics.in.h %reldir%/genprops.awk | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) -f $(srcdir)/%reldir%/genprops.awk $< > $@-t && \
	mv $@-t $@

# Dependency forces build of "graphics-props.cc" before #include
%reldir%/graphics.cc: %reldir%/graphics-props.cc

%reldir%/graphics-props.cc: %reldir%/graphics.in.h %reldir%/genprops.awk | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) -v emit_graphics_props=1 -f $(srcdir)/%reldir%/genprops.awk $< > $@-t && \
	mv $@-t $@

%reldir%/oct-tex-lexer.ll: %reldir%/oct-tex-lexer.in.ll %reldir%/oct-tex-symbols.in %reldir%/oct-tex-parser.h | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) 'BEGIN { print "/* DO NOT EDIT!  Generated automatically from oct-tex-lexer.in.ll and oct-tex-symbols.in by Make. */"; } /^@SYMBOL_RULES@$$/ { count = 0; while (getline < "$(srcdir)/%reldir%/oct-tex-symbols.in") { if ($$0 !~ /^#.*/ && NF == 3) { printf("\"\\\\%s\" { yylval->sym = %d; return SYM; }\n", $$1, count); count++; } } getline } ! /^@SYMBOL_RULES@$$/ { print }' $< > $@-t && \
	mv $@-t $@

# Dependency forces build of "oct-tex-symbols.cc" before #include
%reldir%/text-engine.cc: %reldir%/oct-tex-symbols.cc

%reldir%/oct-tex-symbols.cc: %reldir%/oct-tex-symbols.in | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) 'BEGIN { print "// DO NOT EDIT!  Generated automatically from oct-tex-symbols.in by Make."; print "static uint32_t symbol_codes[][2] = {"; count = 0; } END { print "};"; printf("static int num_symbol_codes = %d;\n", count); } !/^#/ && (NF == 3) { printf("  { %s, %s },\n", $$2, $$3); count++; }' $< > $@-t && \
	mv $@-t $@

## Set environment variable LEX_OUTPUT_ROOT only for compilation of this file.
%reldir%/oct-tex-lexer.cc: LEX_OUTPUT_ROOT := lex.octave_tex_

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

## Distribute generated files associated with parser (.h, .cc) to avoid
## needing extra tools to build Octave from a distribution tarball.
liboctinterp_EXTRA_DIST += \
  %reldir%/genprops.awk \
  %reldir%/graphics.in.h \
  %reldir%/oct-tex-lexer.in.ll \
  %reldir%/oct-tex-lexer.ll \
  %reldir%/oct-tex-parser.h \
  %reldir%/oct-tex-symbols.cc \
  %reldir%/oct-tex-symbols.in

liboctinterp_CLEANFILES += \
  %reldir%/oct-tex-parser.output

liboctinterp_DISTCLEANFILES += \
  %reldir%/graphics-props.cc \
  %reldir%/graphics.h

## Only remove BISON/LEX files when purging build directories
liboctinterp_MAINTAINERCLEANFILES += \
  %reldir%/oct-tex-lexer.ll \
  %reldir%/oct-tex-parser.h \
  %reldir%/oct-tex-symbols.cc
