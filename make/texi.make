# texi.make -- making .dvi and .info from .texi.

MAKEINFO = makeinfo
MAKEINFO_FLAGS = --paragraph-indent=2 -I$(srcdir)
TEXI2DVI = texi2dvi

.SUFFIXES: .info .dvi .texi
.texi.info:
	-$(MAKEINFO) $(MAKEINFO_FLAGS) $< -o $@
.texi.dvi:
	-$(TEXI2DVI) $(TEXI2DVI_FLAGS) $<
