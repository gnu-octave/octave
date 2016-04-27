if AMCOND_BUILD_DOCS

liboctave_TEXINFOS = \
  doc/liboctave/array.texi \
  doc/liboctave/bugs.texi \
  doc/liboctave/cp-idx.texi \
  doc/liboctave/dae.texi \
  doc/liboctave/diffeq.texi \
  doc/liboctave/error.texi \
  doc/liboctave/factor.texi \
  doc/liboctave/fn-idx.texi \
  doc/liboctave/gpl.texi \
  doc/liboctave/install.texi \
  doc/liboctave/intro.texi \
  doc/liboctave/matvec.texi \
  doc/liboctave/nleqn.texi \
  doc/liboctave/nlfunc.texi \
  doc/liboctave/ode.texi \
  doc/liboctave/optim.texi \
  doc/liboctave/preface.texi \
  doc/liboctave/quad.texi \
  doc/liboctave/range.texi

info_TEXINFOS += \
  doc/liboctave/liboctave.texi

INFO_DEPS += $(srcdir)/doc/liboctave/liboctave.info
DVIS += doc/liboctave/liboctave.dvi
PDFS += doc/liboctave/liboctave.pdf
PSS += doc/liboctave/liboctave.ps
HTMLS += doc/liboctave/liboctave.html

doc/liboctave/liboctave.dvi: doc/liboctave/liboctave.texi $(srcdir)/doc/liboctave/version-liboctave.texi | doc/liboctave/$(am__dirstamp)
	$(AM_V_TEXI2DVI)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/liboctave -I $(srcdir)/doc/liboctave' \
	$(TEXI2DVI) $(AM_V_texinfo) --build-dir=$(@:.dvi=.t2d) -o $@ $(AM_V_texidevnull) \
	`test -f 'doc/liboctave/liboctave.texi' || echo '$(srcdir)/'`doc/liboctave/liboctave.texi

doc/liboctave/liboctave.pdf: doc/liboctave/liboctave.texi $(srcdir)/doc/liboctave/version-liboctave.texi | doc/liboctave/$(am__dirstamp)
	$(AM_V_TEXI2PDF)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/liboctave -I $(srcdir)/doc/liboctave' \
	$(TEXI2PDF) $(AM_V_texinfo) --build-dir=$(@:.pdf=.t2p) -o $@ $(AM_V_texidevnull) \
	`test -f 'doc/liboctave/liboctave.texi' || echo '$(srcdir)/'`doc/liboctave/liboctave.texi

doc/liboctave/liboctave.html: doc/liboctave/liboctave.texi $(srcdir)/doc/liboctave/version-liboctave.texi | doc/liboctave/$(am__dirstamp)
	$(AM_V_MAKEINFO)rm -rf $(@:.html=.htp)
	$(AM_V_at)if $(MAKEINFOHTML) $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I doc/liboctave -I $(srcdir)/doc/liboctave \
	 -o $(@:.html=.htp) `test -f 'doc/liboctave/liboctave.texi' || echo '$(srcdir)/'`doc/liboctave/liboctave.texi; \
	then \
	  rm -rf $@ && mv $(@:.html=.htp) $@; \
	else \
	  rm -rf $(@:.html=.htp); exit 1; \
	fi

DOC_TARGETS += \
  $(srcdir)/doc/liboctave/liboctave.info \
  doc/liboctave/liboctave.ps \
  doc/liboctave/liboctave.pdf \
  doc/liboctave/liboctave.html

doc_EXTRA_DIST += \
  $(liboctave_TEXINFOS) \
  $(srcdir)/doc/liboctave/liboctave.info \
  doc/liboctave/liboctave.dvi \
  doc/liboctave/liboctave.ps \
  doc/liboctave/liboctave.pdf \
  doc/liboctave/liboctave.html

## The TeX software suite is used to create both PDF and PS output formats.
## In order to avoid race conditions between simultaneous TeX commands, the
## PDF and PS builds are forced to run serially through the following rule.
doc/liboctave/liboctave.pdf: doc/liboctave/liboctave.ps

DIRSTAMP_FILES += doc/liboctave/$(octave_dirstamp)

endif

doc-liboctave-clean:
	rm -rf doc/liboctave/liboctave.t2d
	rm -rf doc/liboctave/liboctave.t2p
