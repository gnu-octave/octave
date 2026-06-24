if AMCOND_BUILD_DOCS

refcard_TEX_SRC = \
  %reldir%/refcard-a4.tex \
  %reldir%/refcard-legal.tex \
  %reldir%/refcard-letter.tex

refcard_DVI = $(refcard_TEX_SRC:.tex=.dvi)

refcard_PDF = $(refcard_TEX_SRC:.tex=.pdf)

refcard_PS  = $(refcard_TEX_SRC:.tex=.ps)

refcard_FORMATTED = \
  $(refcard_DVI) \
  $(refcard_PDF) \
  $(refcard_PS)

DOC_TARGETS += $(refcard_FORMATTED)

$(refcard_DVI) : %.dvi : %.tex %reldir%/refcard.tex | %reldir%/$(octave_dirstamp)
	-$(AM_V_TEX)cd $(@D) && \
	TEXINPUTS="$(abs_top_srcdir)/doc/refcard:$(TEXINPUTS):" \
	$(TEX) $(<F) $(AM_V_texidevnull)

$(refcard_PDF) : %.pdf : %.tex %reldir%/refcard.tex | %reldir%/$(octave_dirstamp)
	-$(AM_V_PDFTEX)cd $(@D) && \
	TEXINPUTS="$(abs_top_srcdir)/doc/refcard:$(TEXINPUTS):" \
	$(PDFTEX) $(<F) $(AM_V_texidevnull)

%reldir%/refcard-a4.ps: %reldir%/refcard-a4.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 297mm,210mm -o $@ $<

%reldir%/refcard-legal.ps: %reldir%/refcard-legal.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 14in,8.5in -o $@ $<

%reldir%/refcard-letter.ps: %reldir%/refcard-letter.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 11in,8.5in -o $@ $<

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

doc_EXTRA_DIST += \
  %reldir%/refcard.tex \
  $(refcard_FORMATTED) \
  $(refcard_TEX_SRC)

doc_CLEANFILES += \
  %reldir%/refcard-a4.log \
  %reldir%/refcard-legal.log \
  %reldir%/refcard-letter.log

doc_MAINTAINERCLEANFILES += \
  $(refcard_FORMATTED)

endif
