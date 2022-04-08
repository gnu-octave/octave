## Options functions for Fortran packages like LSODE, DASPK.
## These are generated automagically by configure and Perl.
OPT_HANDLERS = \
  %reldir%/DASPK-opts.cc \
  %reldir%/DASRT-opts.cc \
  %reldir%/DASSL-opts.cc \
  %reldir%/LSODE-opts.cc \
  %reldir%/Quad-opts.cc

$(OPT_HANDLERS): %reldir%/%.cc : liboctave/numeric/%.in | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/build-aux/mk-opts.pl --opt-handler-fcns $< > $@-t && \
	mv $@-t $@

$(OPT_HANDLERS): $(srcdir)/build-aux/mk-opts.pl

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

COREFCN_INC = \
  %reldir%/auto-shlib.h \
  %reldir%/base-text-renderer.h \
  %reldir%/Cell.h \
  %reldir%/c-file-ptr-stream.h \
  %reldir%/call-stack.h \
  %reldir%/cdisplay.h \
  %reldir%/data.h \
  %reldir%/defaults.h \
  %reldir%/defun-dld.h \
  %reldir%/defun-int.h \
  %reldir%/defun.h \
  %reldir%/display.h \
  %reldir%/dynamic-ld.h \
  %reldir%/environment.h \
  %reldir%/error.h \
  %reldir%/errwarn.h \
  %reldir%/event-manager.h \
  %reldir%/event-queue.h \
  %reldir%/fcn-info.h \
  %reldir%/file-io.h \
  %reldir%/ft-text-renderer.h \
  %reldir%/gl-render.h \
  %reldir%/gl2ps-print.h \
  %reldir%/graphics-handle.h \
  %reldir%/graphics-toolkit.h \
  %reldir%/gtk-manager.h \
  %reldir%/help.h \
  %reldir%/hook-fcn.h \
  %reldir%/input.h \
  %reldir%/interpreter.h \
  %reldir%/latex-text-renderer.h \
  %reldir%/load-path.h \
  %reldir%/load-save.h \
  %reldir%/ls-ascii-helper.h \
  %reldir%/ls-hdf5.h \
  %reldir%/ls-mat-ascii.h \
  %reldir%/ls-mat4.h \
  %reldir%/ls-mat5.h \
  %reldir%/ls-oct-text.h \
  %reldir%/ls-oct-binary.h \
  %reldir%/ls-utils.h \
  %reldir%/mex.h \
  %reldir%/mexproto.h \
  %reldir%/mx-type-traits.h \
  %reldir%/mxarray.h \
  %reldir%/oct-errno.h \
  %reldir%/oct-fstrm.h \
  %reldir%/oct-handle.h \
  %reldir%/oct-hdf5-types.h \
  %reldir%/oct-hist.h \
  %reldir%/oct-iostrm.h \
  %reldir%/oct-map.h \
  %reldir%/oct-prcstrm.h \
  %reldir%/oct-procbuf.h \
  %reldir%/oct-process.h \
  %reldir%/oct-stdstrm.h \
  %reldir%/oct-stream.h \
  %reldir%/oct-strstrm.h \
  %reldir%/oct.h \
  %reldir%/octave-default-image.h \
  %reldir%/pager.h \
  %reldir%/pr-flt-fmt.h \
  %reldir%/pr-output.h \
  %reldir%/procstream.h \
  %reldir%/settings.h \
  %reldir%/sighandlers.h \
  %reldir%/sparse-xdiv.h \
  %reldir%/sparse-xpow.h \
  %reldir%/stack-frame.h \
  %reldir%/syminfo.h \
  %reldir%/symrec.h \
  %reldir%/symscope.h \
  %reldir%/symtab.h \
  %reldir%/sysdep.h \
  %reldir%/text-engine.h \
  %reldir%/text-renderer.h \
  %reldir%/url-handle-manager.h \
  %reldir%/utils.h \
  %reldir%/variables.h \
  %reldir%/xdiv.h \
  %reldir%/xnorm.h \
  %reldir%/xpow.h \
  %reldir%/gzfstream.h

NOINSTALL_COREFCN_INC = \
  %reldir%/interpreter-private.h \
  %reldir%/mex-private.h \
  %reldir%/oct-hdf5.h \
  %reldir%/oct-opengl.h

## oct-tex-parser.h is in the SRC list so that it will be distributed
## but not installed.

COREFCN_SRC = \
  %reldir%/Cell.cc \
  %reldir%/__betainc__.cc \
  %reldir%/__contourc__.cc \
  %reldir%/__dsearchn__.cc \
  %reldir%/__eigs__.cc \
  %reldir%/__expint__.cc \
  %reldir%/__ftp__.cc \
  %reldir%/__gammainc__.cc \
  %reldir%/__ichol__.cc \
  %reldir%/__ilu__.cc \
  %reldir%/__isprimelarge__.cc \
  %reldir%/__lin_interpn__.cc \
  %reldir%/__magick_read__.cc \
  %reldir%/__pchip_deriv__.cc \
  %reldir%/__qp__.cc \
  %reldir%/amd.cc \
  %reldir%/auto-shlib.cc \
  %reldir%/balance.cc \
  %reldir%/base-text-renderer.cc \
  %reldir%/besselj.cc \
  %reldir%/bitfcns.cc \
  %reldir%/bsxfun.cc \
  %reldir%/c-file-ptr-stream.cc \
  %reldir%/call-stack.cc \
  %reldir%/ccolamd.cc \
  %reldir%/cdisplay.c \
  %reldir%/cellfun.cc \
  %reldir%/chol.cc \
  %reldir%/coct-hdf5-types.c \
  %reldir%/colamd.cc \
  %reldir%/colloc.cc \
  %reldir%/conv2.cc \
  %reldir%/daspk.cc \
  %reldir%/dasrt.cc \
  %reldir%/dassl.cc \
  %reldir%/data.cc \
  %reldir%/debug.cc \
  %reldir%/defaults.cc \
  %reldir%/defun.cc \
  %reldir%/det.cc \
  %reldir%/dirfns.cc \
  %reldir%/display.cc \
  %reldir%/dlmread.cc \
  %reldir%/dmperm.cc \
  %reldir%/dot.cc \
  %reldir%/dynamic-ld.cc \
  %reldir%/eig.cc \
  %reldir%/ellipj.cc \
  %reldir%/environment.cc \
  %reldir%/error.cc \
  %reldir%/errwarn.cc \
  %reldir%/event-manager.cc \
  %reldir%/event-queue.cc \
  %reldir%/fcn-info.cc \
  %reldir%/fft.cc \
  %reldir%/fft2.cc \
  %reldir%/fftn.cc \
  %reldir%/file-io.cc \
  %reldir%/filter.cc \
  %reldir%/find.cc \
  %reldir%/ft-text-renderer.cc \
  %reldir%/gcd.cc \
  %reldir%/getgrent.cc \
  %reldir%/getpwent.cc \
  %reldir%/getrusage.cc \
  %reldir%/givens.cc \
  %reldir%/gl-render.cc \
  %reldir%/gl2ps-print.cc \
  %reldir%/graphics-toolkit.cc \
  %reldir%/graphics.cc \
  %reldir%/gsvd.cc \
  %reldir%/gtk-manager.cc \
  %reldir%/hash.cc \
  %reldir%/help.cc \
  %reldir%/hess.cc \
  %reldir%/hex2num.cc \
  %reldir%/hook-fcn.cc \
  %reldir%/input.cc \
  %reldir%/interpreter-private.cc \
  %reldir%/interpreter.cc \
  %reldir%/inv.cc \
  %reldir%/jsondecode.cc \
  %reldir%/jsonencode.cc \
  %reldir%/kron.cc \
  %reldir%/latex-text-renderer.cc \
  %reldir%/load-path.cc \
  %reldir%/load-save.cc \
  %reldir%/lookup.cc \
  %reldir%/ls-ascii-helper.cc \
  %reldir%/ls-hdf5.cc \
  %reldir%/ls-mat-ascii.cc \
  %reldir%/ls-mat4.cc \
  %reldir%/ls-mat5.cc \
  %reldir%/ls-oct-binary.cc \
  %reldir%/ls-oct-text.cc \
  %reldir%/ls-utils.cc \
  %reldir%/lsode.cc \
  %reldir%/lu.cc \
  %reldir%/mappers.cc \
  %reldir%/matrix_type.cc \
  %reldir%/max.cc \
  %reldir%/mex.cc \
  %reldir%/mgorth.cc \
  %reldir%/nproc.cc \
  %reldir%/oct-fstrm.cc \
  %reldir%/oct-hdf5-types.cc \
  %reldir%/oct-hist.cc \
  %reldir%/oct-iostrm.cc \
  %reldir%/oct-map.cc \
  %reldir%/oct-prcstrm.cc \
  %reldir%/oct-procbuf.cc \
  %reldir%/oct-process.cc \
  %reldir%/oct-stream.cc \
  %reldir%/oct-strstrm.cc \
  %reldir%/oct-tex-lexer.ll \
  %reldir%/oct-tex-parser.h \
  %reldir%/oct-tex-parser.yy \
  %reldir%/ordqz.cc \
  %reldir%/ordschur.cc \
  %reldir%/pager.cc \
  %reldir%/pinv.cc \
  %reldir%/pow2.cc \
  %reldir%/pr-flt-fmt.cc \
  %reldir%/pr-output.cc \
  %reldir%/procstream.cc \
  %reldir%/psi.cc \
  %reldir%/qr.cc \
  %reldir%/quad.cc \
  %reldir%/quadcc.cc \
  %reldir%/qz.cc \
  %reldir%/rand.cc \
  %reldir%/rcond.cc \
  %reldir%/regexp.cc \
  %reldir%/schur.cc \
  %reldir%/settings.cc \
  %reldir%/sighandlers.cc \
  %reldir%/sparse-xdiv.cc \
  %reldir%/sparse-xpow.cc \
  %reldir%/sparse.cc \
  %reldir%/spparms.cc \
  %reldir%/sqrtm.cc \
  %reldir%/stack-frame.cc \
  %reldir%/stream-euler.cc \
  %reldir%/strfind.cc \
  %reldir%/strfns.cc \
  %reldir%/sub2ind.cc \
  %reldir%/svd.cc \
  %reldir%/sylvester.cc \
  %reldir%/symbfact.cc \
  %reldir%/syminfo.cc \
  %reldir%/symrcm.cc \
  %reldir%/symrec.cc \
  %reldir%/symscope.cc \
  %reldir%/symtab.cc \
  %reldir%/syscalls.cc \
  %reldir%/sysdep.cc \
  %reldir%/text-engine.cc \
  %reldir%/text-renderer.cc \
  %reldir%/time.cc \
  %reldir%/toplev.cc \
  %reldir%/tril.cc \
  %reldir%/tsearch.cc \
  %reldir%/typecast.cc \
  %reldir%/url-handle-manager.cc \
  %reldir%/urlwrite.cc \
  %reldir%/utils.cc \
  %reldir%/variables.cc \
  %reldir%/xdiv.cc \
  %reldir%/xnorm.cc \
  %reldir%/xpow.cc \
  %reldir%/gzfstream.cc \
  $(NOINSTALL_COREFCN_INC)

## Special rules for sources which must be built before rest of compilation.

%reldir%/default-defs.h: %reldir%/default-defs.in.h build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

%reldir%/graphics.h: %reldir%/graphics.in.h %reldir%/genprops.awk | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) -f $(srcdir)/%reldir%/genprops.awk $< > $@-t && \
	mv $@-t $@

%reldir%/graphics-props.cc: %reldir%/graphics.in.h %reldir%/genprops.awk | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) -v emit_graphics_props=1 -f $(srcdir)/%reldir%/genprops.awk $< > $@-t && \
	mv $@-t $@

%reldir%/oct-errno.cc: %reldir%/oct-errno.in.cc %reldir%/mk-errno-list.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	if test -n "$(PERL)"; then \
	  $(SHELL) $(srcdir)/%reldir%/mk-errno-list.sh --perl "$(PERL)" < $< > $@-t; \
	elif test -n "$(PYTHON)"; then \
	  $(SHELL) $(srcdir)/%reldir%/mk-errno-list.sh --python "$(PYTHON)" < $< > $@-t; \
	else \
	  $(SHELL) $(srcdir)/%reldir%/mk-errno-list.sh --sed "$(SED)" < $< > $@-t; \
	fi && \
	mv $@-t $@

%reldir%/mxtypes.h: %reldir%/mxtypes.in.h %reldir%/mk-mxtypes-h.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,%reldir%/mk-mxtypes-h.sh)

%reldir%/oct-tex-lexer.ll: %reldir%/oct-tex-lexer.in.ll %reldir%/oct-tex-symbols.in | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) 'BEGIN { print "/* DO NOT EDIT. AUTOMATICALLY GENERATED FROM oct-tex-lexer.in.ll and oct-tex-symbols.in. */"; } /^@SYMBOL_RULES@$$/ { count = 0; while (getline < "$(srcdir)/%reldir%/oct-tex-symbols.in") { if ($$0 !~ /^#.*/ && NF == 3) { printf("\"\\\\%s\" { yylval->sym = %d; return SYM; }\n", $$1, count); count++; } } getline } ! /^@SYMBOL_RULES@$$/ { print }' $< > $@-t && \
	mv $@-t $@

%reldir%/oct-tex-symbols.cc: %reldir%/oct-tex-symbols.in | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) 'BEGIN { print "// DO NOT EDIT. AUTOMATICALLY GENERATED FROM oct-tex-symbols.in."; print "static uint32_t symbol_codes[][2] = {"; count = 0; } END { print "};"; printf("static int num_symbol_codes = %d;\n", count); } !/^#/ && (NF == 3) { printf("  { %s, %s },\n", $$2, $$3); count++; }' $< > $@-t && \
	mv $@-t $@

%reldir%/oct-tex-lexer.cc: LEX_OUTPUT_ROOT := lex.octave_tex_

noinst_LTLIBRARIES += \
  %reldir%/libcorefcn.la

%canon_reldir%_libcorefcn_la_SOURCES = $(COREFCN_SRC)

%canon_reldir%_libcorefcn_la_CPPFLAGS = \
  $(libinterp_liboctinterp_la_CPPFLAGS) \
  $(FFTW_XCPPFLAGS) \
  $(FONTCONFIG_CPPFLAGS) \
  $(FT2_CPPFLAGS) \
  $(HDF5_CPPFLAGS) \
  $(SPARSE_XCPPFLAGS) \
  $(Z_CPPFLAGS) \
  $(OCTAVE_TEX_PARSER_CPPFLAGS)

libinterp_EXTRA_DIST += \
  %reldir%/default-defs.in.h \
  %reldir%/genprops.awk \
  %reldir%/graphics.in.h \
  %reldir%/mk-errno-list.sh \
  %reldir%/mk-mxtypes-h.in.sh \
  %reldir%/mxtypes.in.h \
  %reldir%/oct-errno.in.cc \
  %reldir%/oct-tex-lexer.in.ll \
  %reldir%/oct-tex-symbols.in

GEN_CONFIG_SHELL += \
  %reldir%/mk-mxtypes-h.sh
