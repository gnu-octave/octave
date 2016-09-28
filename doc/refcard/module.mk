if AMCOND_BUILD_DOCS

refcard_TEX_SRC = \
  doc/refcard/refcard.tex \
  doc/refcard/refcard-a4.tex \
  doc/refcard/refcard-legal.tex \
  doc/refcard/refcard-letter.tex

refcard_DVI = \
  doc/refcard/refcard-a4.dvi \
  doc/refcard/refcard-legal.dvi \
  doc/refcard/refcard-letter.dvi

refcard_PDF = \
  doc/refcard/refcard-a4.pdf \
  doc/refcard/refcard-legal.pdf \
  doc/refcard/refcard-letter.pdf

refcard_PS = \
  doc/refcard/refcard-letter.ps \
  doc/refcard/refcard-a4.ps \
  doc/refcard/refcard-legal.ps

refcard_FORMATTED = \
  $(refcard_DVI) \
  $(refcard_PDF) \
  $(refcard_PS)

DOC_TARGETS += \
  $(refcard_FORMATTED)

doc/refcard/refcard-a4.pdf: doc/refcard/refcard.tex
doc/refcard/refcard-a4.dvi: doc/refcard/refcard.tex
doc/refcard/refcard-a4.ps: doc/refcard/refcard-a4.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 297mm,210mm -o $@ $<

doc/refcard/refcard-legal.pdf: doc/refcard/refcard.tex
doc/refcard/refcard-legal.dvi: doc/refcard/refcard.tex
doc/refcard/refcard-legal.ps: doc/refcard/refcard-legal.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 14in,8.5in -o $@ $<

doc/refcard/refcard-letter.pdf: doc/refcard/refcard.tex
doc/refcard/refcard-letter.dvi: doc/refcard/refcard.tex
doc/refcard/refcard-letter.ps: doc/refcard/refcard-letter.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 11in,8.5in -o $@ $<

$(srcdir)/doc/interpreter/images.mk: $(srcdir)/doc/interpreter/config-images.sh $(srcdir)/doc/interpreter/images.awk $(srcdir)/doc/interpreter/images
	$(AM_V_GEN)$(SHELL) $(srcdir)/doc/interpreter/config-images.sh $(top_srcdir)

$(refcard_DVI) : %.dvi : %.tex | doc/refcard/$(octave_dirstamp)
	-$(AM_V_TEX)cd $(@D) && \
	TEXINPUTS="$(abs_top_srcdir)/doc/refcard:$(TEXINPUTS):" \
	$(TEX) $(<F) $(AM_V_texidevnull)

$(refcard_PDF) : %.pdf : %.tex | doc/refcard/$(octave_dirstamp)
	-$(AM_V_PDFTEX)cd $(@D) && \
	TEXINPUTS="$(abs_top_srcdir)/doc/refcard:$(TEXINPUTS):" \
	$(PDFTEX) $(<F) $(AM_V_texidevnull)

doc_EXTRA_DIST += \
  $(refcard_FORMATTED) \
  $(refcard_TEX_SRC)

doc_CLEANFILES += \
  doc/refcard/refcard-a4.log \
  doc/refcard/refcard-legal.log \
  doc/refcard/refcard-letter.log

doc_MAINTAINERCLEANFILES += \
  $(refcard_FORMATTED)

DIRSTAMP_FILES += doc/refcard/$(octave_dirstamp)

endif
