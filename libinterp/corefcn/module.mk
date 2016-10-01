## Options functions for Fortran packages like LSODE, DASPK.
## These are generated automagically by configure and Perl.
OPT_HANDLERS = \
  libinterp/corefcn/DASPK-opts.cc \
  libinterp/corefcn/DASRT-opts.cc \
  libinterp/corefcn/DASSL-opts.cc \
  libinterp/corefcn/LSODE-opts.cc \
  libinterp/corefcn/Quad-opts.cc

$(OPT_HANDLERS): libinterp/corefcn/%.cc : liboctave/numeric/%.in | libinterp/corefcn/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/build-aux/mk-opts.pl --opt-handler-fcns $< > $@-t && \
	mv $@-t $@

$(OPT_HANDLERS): $(srcdir)/build-aux/mk-opts.pl

DIRSTAMP_FILES += libinterp/corefcn/$(octave_dirstamp)

JIT_INC = \
  libinterp/corefcn/jit-util.h \
  libinterp/corefcn/jit-typeinfo.h \
  libinterp/corefcn/jit-ir.h \
  libinterp/corefcn/pt-jit.h

COREFCN_INC = \
  libinterp/corefcn/base-text-renderer.h \
  libinterp/corefcn/Cell.h \
  libinterp/corefcn/c-file-ptr-stream.h \
  libinterp/corefcn/call-stack.h \
  libinterp/corefcn/cdisplay.h \
  libinterp/corefcn/comment-list.h \
  libinterp/corefcn/data.h \
  libinterp/corefcn/debug.h \
  libinterp/corefcn/defun-dld.h \
  libinterp/corefcn/defun-int.h \
  libinterp/corefcn/defun.h \
  libinterp/corefcn/dirfns.h \
  libinterp/corefcn/display.h \
  libinterp/corefcn/dynamic-ld.h \
  libinterp/corefcn/error.h \
  libinterp/corefcn/errwarn.h \
  libinterp/corefcn/event-queue.h \
  libinterp/corefcn/file-io.h \
  libinterp/corefcn/ft-text-renderer.h \
  libinterp/corefcn/gl-render.h \
  libinterp/corefcn/gl2ps-print.h \
  libinterp/corefcn/gripes.h \
  libinterp/corefcn/help.h \
  libinterp/corefcn/hook-fcn.h \
  libinterp/corefcn/input.h \
  libinterp/corefcn/interpreter.h \
  libinterp/corefcn/load-path.h \
  libinterp/corefcn/load-save.h \
  libinterp/corefcn/ls-ascii-helper.h \
  libinterp/corefcn/ls-hdf5.h \
  libinterp/corefcn/ls-mat-ascii.h \
  libinterp/corefcn/ls-mat4.h \
  libinterp/corefcn/ls-mat5.h \
  libinterp/corefcn/ls-oct-text.h \
  libinterp/corefcn/ls-oct-binary.h \
  libinterp/corefcn/ls-utils.h \
  libinterp/corefcn/mex.h \
  libinterp/corefcn/mexproto.h \
  libinterp/corefcn/oct-errno.h \
  libinterp/corefcn/oct-fstrm.h \
  libinterp/corefcn/oct-handle.h \
  libinterp/corefcn/oct-hdf5-types.h \
  libinterp/corefcn/oct-hist.h \
  libinterp/corefcn/oct-iostrm.h \
  libinterp/corefcn/oct-lvalue.h \
  libinterp/corefcn/oct-map.h \
  libinterp/corefcn/oct-obj.h \
  libinterp/corefcn/oct-prcstrm.h \
  libinterp/corefcn/oct-procbuf.h \
  libinterp/corefcn/oct-stdstrm.h \
  libinterp/corefcn/oct-stream.h \
  libinterp/corefcn/oct-strstrm.h \
  libinterp/corefcn/oct.h \
  libinterp/corefcn/octave-default-image.h \
  libinterp/corefcn/octave-link.h \
  libinterp/corefcn/octave-preserve-stream-state.h \
  libinterp/corefcn/pager.h \
  libinterp/corefcn/pr-output.h \
  libinterp/corefcn/procstream.h \
  libinterp/corefcn/profiler.h \
  libinterp/corefcn/sighandlers.h \
  libinterp/corefcn/sparse-xdiv.h \
  libinterp/corefcn/sparse-xpow.h \
  libinterp/corefcn/symtab.h \
  libinterp/corefcn/sysdep.h \
  libinterp/corefcn/text-renderer.h \
  libinterp/corefcn/toplev.h \
  libinterp/corefcn/txt-eng.h \
  libinterp/corefcn/utils.h \
  libinterp/corefcn/variables.h \
  libinterp/corefcn/workspace-element.h \
  libinterp/corefcn/xdiv.h \
  libinterp/corefcn/xnorm.h \
  libinterp/corefcn/xpow.h \
  libinterp/corefcn/zfstream.h \
  $(JIT_INC)

JIT_SRC = \
  libinterp/corefcn/jit-util.cc \
  libinterp/corefcn/jit-typeinfo.cc \
  libinterp/corefcn/jit-ir.cc \
  libinterp/corefcn/pt-jit.cc

NOINSTALL_COREFCN_INC = \
  libinterp/corefcn/oct-hdf5.h \
  libinterp/corefcn/oct-opengl.h

## oct-tex-parser.h is in the SRC list so that it will be distributed
## but not installed.

COREFCN_SRC = \
  libinterp/corefcn/Cell.cc \
  libinterp/corefcn/__contourc__.cc \
  libinterp/corefcn/__dispatch__.cc \
  libinterp/corefcn/__dsearchn__.cc \
  libinterp/corefcn/__ichol__.cc \
  libinterp/corefcn/__ilu__.cc \
  libinterp/corefcn/__lin_interpn__.cc \
  libinterp/corefcn/__luinc__.cc \
  libinterp/corefcn/__magick_read__.cc \
  libinterp/corefcn/__pchip_deriv__.cc \
  libinterp/corefcn/__qp__.cc \
  libinterp/corefcn/balance.cc \
  libinterp/corefcn/besselj.cc \
  libinterp/corefcn/betainc.cc \
  libinterp/corefcn/bitfcns.cc \
  libinterp/corefcn/bsxfun.cc \
  libinterp/corefcn/c-file-ptr-stream.cc \
  libinterp/corefcn/call-stack.cc \
  libinterp/corefcn/cdisplay.c \
  libinterp/corefcn/cellfun.cc \
  libinterp/corefcn/colloc.cc \
  libinterp/corefcn/coct-hdf5-types.c \
  libinterp/corefcn/comment-list.cc \
  libinterp/corefcn/conv2.cc \
  libinterp/corefcn/daspk.cc \
  libinterp/corefcn/dasrt.cc \
  libinterp/corefcn/dassl.cc \
  libinterp/corefcn/data.cc \
  libinterp/corefcn/debug.cc \
  libinterp/corefcn/defaults.cc \
  libinterp/corefcn/defun.cc \
  libinterp/corefcn/det.cc \
  libinterp/corefcn/dirfns.cc \
  libinterp/corefcn/display.cc \
  libinterp/corefcn/dlmread.cc \
  libinterp/corefcn/dot.cc \
  libinterp/corefcn/dynamic-ld.cc \
  libinterp/corefcn/eig.cc \
  libinterp/corefcn/ellipj.cc \
  libinterp/corefcn/error.cc \
  libinterp/corefcn/errwarn.cc \
  libinterp/corefcn/event-queue.cc \
  libinterp/corefcn/fft.cc \
  libinterp/corefcn/fft2.cc \
  libinterp/corefcn/fftn.cc \
  libinterp/corefcn/file-io.cc \
  libinterp/corefcn/filter.cc \
  libinterp/corefcn/find.cc \
  libinterp/corefcn/ft-text-renderer.cc \
  libinterp/corefcn/gammainc.cc \
  libinterp/corefcn/gcd.cc \
  libinterp/corefcn/getgrent.cc \
  libinterp/corefcn/getpwent.cc \
  libinterp/corefcn/getrusage.cc \
  libinterp/corefcn/givens.cc \
  libinterp/corefcn/gl-render.cc \
  libinterp/corefcn/gl2ps-print.cc \
  libinterp/corefcn/graphics.cc \
  libinterp/corefcn/gripes.cc \
  libinterp/corefcn/hash.cc \
  libinterp/corefcn/help.cc \
  libinterp/corefcn/hess.cc \
  libinterp/corefcn/hex2num.cc \
  libinterp/corefcn/hook-fcn.cc \
  libinterp/corefcn/input.cc \
  libinterp/corefcn/inv.cc \
  libinterp/corefcn/interpreter.cc \
  libinterp/corefcn/kron.cc \
  libinterp/corefcn/load-path.cc \
  libinterp/corefcn/load-save.cc \
  libinterp/corefcn/lookup.cc \
  libinterp/corefcn/ls-ascii-helper.cc \
  libinterp/corefcn/ls-hdf5.cc \
  libinterp/corefcn/ls-mat-ascii.cc \
  libinterp/corefcn/ls-mat4.cc \
  libinterp/corefcn/ls-mat5.cc \
  libinterp/corefcn/ls-oct-text.cc \
  libinterp/corefcn/ls-oct-binary.cc \
  libinterp/corefcn/ls-utils.cc \
  libinterp/corefcn/lsode.cc \
  libinterp/corefcn/lu.cc \
  libinterp/corefcn/mappers.cc \
  libinterp/corefcn/matrix_type.cc \
  libinterp/corefcn/max.cc \
  libinterp/corefcn/mex.cc \
  libinterp/corefcn/mgorth.cc \
  libinterp/corefcn/nproc.cc \
  libinterp/corefcn/oct-fstrm.cc \
  libinterp/corefcn/oct-hdf5-types.cc \
  libinterp/corefcn/oct-hist.cc \
  libinterp/corefcn/oct-iostrm.cc \
  libinterp/corefcn/oct-lvalue.cc \
  libinterp/corefcn/oct-map.cc \
  libinterp/corefcn/oct-prcstrm.cc \
  libinterp/corefcn/oct-procbuf.cc \
  libinterp/corefcn/oct-stream.cc \
  libinterp/corefcn/oct-strstrm.cc \
  libinterp/corefcn/oct-tex-lexer.ll \
  libinterp/corefcn/oct-tex-parser.h \
  libinterp/corefcn/oct-tex-parser.yy \
  libinterp/corefcn/octave-link.cc \
  libinterp/corefcn/ordschur.cc \
  libinterp/corefcn/pager.cc \
  libinterp/corefcn/pinv.cc \
  libinterp/corefcn/pr-output.cc \
  libinterp/corefcn/procstream.cc \
  libinterp/corefcn/profiler.cc \
  libinterp/corefcn/psi.cc \
  libinterp/corefcn/quad.cc \
  libinterp/corefcn/quadcc.cc \
  libinterp/corefcn/qz.cc \
  libinterp/corefcn/rand.cc \
  libinterp/corefcn/rcond.cc \
  libinterp/corefcn/regexp.cc \
  libinterp/corefcn/schur.cc \
  libinterp/corefcn/sighandlers.cc \
  libinterp/corefcn/sparse-xdiv.cc \
  libinterp/corefcn/sparse-xpow.cc \
  libinterp/corefcn/sparse.cc \
  libinterp/corefcn/spparms.cc \
  libinterp/corefcn/sqrtm.cc \
  libinterp/corefcn/str2double.cc \
  libinterp/corefcn/strfind.cc \
  libinterp/corefcn/strfns.cc \
  libinterp/corefcn/sub2ind.cc \
  libinterp/corefcn/svd.cc \
  libinterp/corefcn/sylvester.cc \
  libinterp/corefcn/symtab.cc \
  libinterp/corefcn/syscalls.cc \
  libinterp/corefcn/sysdep.cc \
  libinterp/corefcn/time.cc \
  libinterp/corefcn/text-renderer.cc \
  libinterp/corefcn/toplev.cc \
  libinterp/corefcn/tril.cc \
  libinterp/corefcn/tsearch.cc \
  libinterp/corefcn/txt-eng.cc \
  libinterp/corefcn/typecast.cc \
  libinterp/corefcn/urlwrite.cc \
  libinterp/corefcn/utils.cc \
  libinterp/corefcn/variables.cc \
  libinterp/corefcn/xdiv.cc \
  libinterp/corefcn/xnorm.cc \
  libinterp/corefcn/xpow.cc \
  libinterp/corefcn/zfstream.cc \
  $(JIT_SRC) \
  $(NOINSTALL_COREFCN_INC)

## Special rules for sources which must be built before rest of compilation.

libinterp/corefcn/defaults.h: libinterp/corefcn/defaults.in.h build-aux/subst-default-vals.sh | libinterp/corefcn/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-default-vals.sh)

libinterp/corefcn/graphics.h: libinterp/corefcn/graphics.in.h libinterp/genprops.awk | libinterp/corefcn/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) -f $(srcdir)/libinterp/genprops.awk $< > $@-t && \
	mv $@-t $@

libinterp/corefcn/graphics-props.cc: libinterp/corefcn/graphics.in.h libinterp/genprops.awk | libinterp/corefcn/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) -v emit_graphics_props=1 -f $(srcdir)/libinterp/genprops.awk $< > $@-t && \
	mv $@-t $@

libinterp/corefcn/oct-errno.cc: libinterp/corefcn/oct-errno.in.cc | libinterp/corefcn/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	if test -n "$(PERL)"; then \
	  $(SHELL) $(srcdir)/libinterp/mk-errno-list --perl "$(PERL)" < $< > $@-t; \
	elif test -n "$(PYTHON)"; then \
	  $(SHELL) $(srcdir)/libinterp/mk-errno-list --python "$(PYTHON)" < $< > $@-t; \
	else \
	  $(SED) '/@SYSDEP_ERRNO_LIST@/D' $< > $@-t; \
	fi && \
	mv $@-t $@

libinterp/corefcn/mxarray.h: libinterp/corefcn/mxarray.in.h build-aux/mk-mxarray-h.sh | libinterp/corefcn/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/mk-mxarray-h.sh)

libinterp/corefcn/oct-tex-lexer.ll: libinterp/corefcn/oct-tex-lexer.in.ll libinterp/corefcn/oct-tex-symbols.in | libinterp/corefcn/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) 'BEGIN { print "/* DO NOT EDIT. AUTOMATICALLY GENERATED FROM oct-tex-lexer.in.ll and oct-tex-symbols.in. */"; } /^@SYMBOL_RULES@$$/ { count = 0; while (getline < "$(srcdir)/libinterp/corefcn/oct-tex-symbols.in") { if ($$0 !~ /^#.*/ && NF == 3) { printf("\"\\\\%s\" { yylval->sym = %d; return SYM; }\n", $$1, count); count++; } } getline } ! /^@SYMBOL_RULES@$$/ { print }' $< > $@-t && \
	mv $@-t $@

libinterp/corefcn/oct-tex-symbols.cc: libinterp/corefcn/oct-tex-symbols.in | libinterp/corefcn/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(AWK) 'BEGIN { print "// DO NOT EDIT. AUTOMATICALLY GENERATED FROM oct-tex-symbols.in."; print "static uint32_t symbol_codes[][2] = {"; count = 0; } END { print "};"; printf("static int num_symbol_codes = %d;\n", count); } !/^#/ && (NF == 3) { printf("  { %s, %s },\n", $$2, $$3); count++; }' $< > $@-t && \
	mv $@-t $@

libinterp/corefcn/oct-tex-lexer.cc: LEX_OUTPUT_ROOT := lex.octave_tex_

libinterp/corefcn/oct-tex-parser.yy: libinterp/corefcn/oct-tex-parser.in.yy
	$(AM_V_GEN)$(call subst-bison-api-decls,octave_tex_)

noinst_LTLIBRARIES += \
  libinterp/corefcn/libcorefcn.la

libinterp_corefcn_libcorefcn_la_SOURCES = $(COREFCN_SRC)

libinterp_corefcn_libcorefcn_la_CPPFLAGS = \
  $(libinterp_liboctinterp_la_CPPFLAGS) \
  $(FFTW_XCPPFLAGS) \
  $(FONTCONFIG_CPPFLAGS) \
  $(FT2_CPPFLAGS) \
  $(HDF5_CPPFLAGS) \
  $(LLVM_CPPFLAGS) \
  $(Z_CPPFLAGS)

libinterp_corefcn_libcorefcn_la_CFLAGS = $(AM_CFLAGS) $(WARN_CFLAGS)

libinterp_corefcn_libcorefcn_la_CXXFLAGS = $(AM_CXXFLAGS) $(WARN_CXXFLAGS) $(LLVM_CXXFLAGS)

libinterp_EXTRA_DIST += \
  libinterp/corefcn/defaults.in.h \
  libinterp/corefcn/graphics.in.h \
  libinterp/corefcn/mxarray.in.h \
  libinterp/corefcn/oct-errno.in.cc \
  libinterp/corefcn/oct-tex-lexer.in.ll \
  libinterp/corefcn/oct-tex-parser.in.yy \
  libinterp/corefcn/oct-tex-symbols.in

