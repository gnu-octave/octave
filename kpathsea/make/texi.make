# texi.make -- making .dvi and .info from .texi.
MAKEINFO = makeinfo
MAKEINFO_FLAGS = --paragraph-indent=2 -I$(srcdir)

TEXI2DVI = texi2dvi

TEXI2HTML = texi2html
TEXI2HTML_FLAGS = -expandinfo -number -menu -split_chapter
# If you prefer one big .html file instead of several, remove
# -split-node or replace it by -split_chapter.

# For making normal text files out of Texinfo source.
one_info = --no-headers --no-split --no-validate

.SUFFIXES: .info .dvi .html .texi
.texi.info:
	$(MAKEINFO) $(MAKEINFO_FLAGS) $< -o $@
.texi.dvi:
	$(TEXI2DVI) $(TEXI2DVI_FLAGS) $<
.texi.html:
	$(TEXI2HTML) $(TEXI2HTML_FLAGS) $< 
# End of texi.make.
