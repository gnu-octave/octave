if AMCOND_BUILD_DOCS

refcard_TEX_SRC = \
  %reldir%/refcard.tex \
  %reldir%/refcard-a4.tex \
  %reldir%/refcard-legal.tex \
  %reldir%/refcard-letter.tex

refcard_DVI = \
  %reldir%/refcard-a4.dvi \
  %reldir%/refcard-legal.dvi \
  %reldir%/refcard-letter.dvi

refcard_PDF = \
  %reldir%/refcard-a4.pdf \
  %reldir%/refcard-legal.pdf \
  %reldir%/refcard-letter.pdf

refcard_PS = \
  %reldir%/refcard-letter.ps \
  %reldir%/refcard-a4.ps \
  %reldir%/refcard-legal.ps

refcard_FORMATTED = \
  $(refcard_DVI) \
  $(refcard_PDF) \
  $(refcard_PS)

DOC_TARGETS += \
  $(refcard_FORMATTED)

%reldir%/refcard-a4.pdf: %reldir%/refcard.tex
%reldir%/refcard-a4.dvi: %reldir%/refcard.tex
%reldir%/refcard-a4.ps: %reldir%/refcard-a4.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 297mm,210mm -o $@ $<

%reldir%/refcard-legal.pdf: %reldir%/refcard.tex
%reldir%/refcard-legal.dvi: %reldir%/refcard.tex
%reldir%/refcard-legal.ps: %reldir%/refcard-legal.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 14in,8.5in -o $@ $<

%reldir%/refcard-letter.pdf: %reldir%/refcard.tex
%reldir%/refcard-letter.dvi: %reldir%/refcard.tex
%reldir%/refcard-letter.ps: %reldir%/refcard-letter.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 11in,8.5in -o $@ $<

$(refcard_DVI) : %.dvi : %.tex | %reldir%/$(octave_dirstamp)
	-$(AM_V_TEX)cd $(@D) && \
	TEXINPUTS="$(abs_top_srcdir)/doc/refcard:$(TEXINPUTS):" \
	$(TEX) $(<F) $(AM_V_texidevnull)

$(refcard_PDF) : %.pdf : %.tex | %reldir%/$(octave_dirstamp)
	-$(AM_V_PDFTEX)cd $(@D) && \
	TEXINPUTS="$(abs_top_srcdir)/doc/refcard:$(TEXINPUTS):" \
	$(PDFTEX) $(<F) $(AM_V_texidevnull)

doc_EXTRA_DIST += \
  $(refcard_FORMATTED) \
  $(refcard_TEX_SRC)

doc_CLEANFILES += \
  %reldir%/refcard-a4.log \
  %reldir%/refcard-legal.log \
  %reldir%/refcard-letter.log

doc_MAINTAINERCLEANFILES += \
  $(refcard_FORMATTED)

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

endif
