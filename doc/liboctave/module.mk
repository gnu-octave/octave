if AMCOND_BUILD_DOCS

liboctave_TEXINFOS = \
  %reldir%/array.texi \
  %reldir%/bugs.texi \
  %reldir%/cp-idx.texi \
  %reldir%/dae.texi \
  %reldir%/diffeq.texi \
  %reldir%/error.texi \
  %reldir%/factor.texi \
  %reldir%/fn-idx.texi \
  %reldir%/gpl.texi \
  %reldir%/install.texi \
  %reldir%/intro.texi \
  %reldir%/matvec.texi \
  %reldir%/nleqn.texi \
  %reldir%/nlfunc.texi \
  %reldir%/ode.texi \
  %reldir%/optim.texi \
  %reldir%/preface.texi \
  %reldir%/quad.texi \
  %reldir%/range.texi

info_TEXINFOS += \
  %reldir%/liboctave.texi

INFO_DEPS += $(srcdir)/%reldir%/liboctave.info
DVIS += %reldir%/liboctave.dvi
PDFS += %reldir%/liboctave.pdf
PSS += %reldir%/liboctave.ps
HTMLS += %reldir%/liboctave.html

%reldir%/liboctave.dvi: %reldir%/liboctave.texi $(srcdir)/%reldir%/version-liboctave.texi | %reldir%/$(am__dirstamp)
	$(AM_V_TEXI2DVI)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/liboctave -I $(srcdir)/doc/liboctave' \
	$(TEXI2DVI) $(AM_V_texinfo) --build-dir=$(@:.dvi=.t2d) -o $@ $(AM_V_texidevnull) \
	`test -f '%reldir%/liboctave.texi' || echo '$(srcdir)/'`%reldir%/liboctave.texi

%reldir%/liboctave.pdf: %reldir%/liboctave.texi $(srcdir)/%reldir%/version-liboctave.texi | %reldir%/$(am__dirstamp)
	$(AM_V_TEXI2PDF)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/liboctave -I $(srcdir)/doc/liboctave' \
	$(TEXI2PDF) $(AM_V_texinfo) --build-dir=$(@:.pdf=.t2p) -o $@ $(AM_V_texidevnull) \
	`test -f '%reldir%/liboctave.texi' || echo '$(srcdir)/'`%reldir%/liboctave.texi

%reldir%/liboctave.html: %reldir%/liboctave.texi $(srcdir)/%reldir%/version-liboctave.texi | %reldir%/$(am__dirstamp)
	$(AM_V_MAKEINFO)rm -rf $(@:.html=.htp)
	$(AM_V_at)if $(MAKEINFOHTML) $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I doc/liboctave -I $(srcdir)/doc/liboctave \
	 -o $(@:.html=.htp) `test -f '%reldir%/liboctave.texi' || echo '$(srcdir)/'`%reldir%/liboctave.texi; \
	then \
	  rm -rf $@ && mv $(@:.html=.htp) $@; \
	else \
	  rm -rf $(@:.html=.htp); exit 1; \
	fi

DOC_TARGETS += \
  $(srcdir)/%reldir%/liboctave.info \
  %reldir%/liboctave.ps \
  %reldir%/liboctave.pdf \
  %reldir%/liboctave.html

doc_EXTRA_DIST += \
  $(liboctave_TEXINFOS) \
  $(srcdir)/%reldir%/liboctave.info \
  %reldir%/liboctave.dvi \
  %reldir%/liboctave.ps \
  %reldir%/liboctave.pdf \
  %reldir%/liboctave.html

## The TeX software suite is used to create both PDF and PS output formats.
## In order to avoid race conditions between simultaneous TeX commands, the
## PDF and PS builds are forced to run serially through the following rule.
%reldir%/liboctave.pdf: %reldir%/liboctave.ps

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

endif

doc-liboctave-clean:
	rm -rf %reldir%/liboctave.t2d
	rm -rf %reldir%/liboctave.t2p
