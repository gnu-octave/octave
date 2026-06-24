# Doxygen documentation is enormous and is not built by default (no all target)

# Generate README.md from README and replace first line by a Doxygen
# specific one.
%reldir%/pages/README.md: $(srcdir)/README | %reldir%/pages/$(octave_dirstamp)
	$(AM_V_GEN)cat $< | $(SED) '1s/.*/notitle {#mainpage}/; 2s/.*/=======/' > $@

DOXYGEN_PAGES = \
  %reldir%/pages/README.md \
  %reldir%/pages/macros.dox

doxyhtml: %reldir%/Doxyfile $(DOXYGEN_PAGES) | %reldir%/$(octave_dirstamp)
	doxygen %reldir%/Doxyfile

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
DIRSTAMP_FILES += %reldir%/pages/$(octave_dirstamp)

doc_EXTRA_DIST += \
  $(DOXYGEN_PAGES) \
  %reldir%/Doxyfile.in \
  %reldir%/README.md

# This target is important for builds in the source tree.
doxyhtml-maintainer-clean:
	rm -f %reldir%/$(octave_dirstamp)
	rm -f %reldir%/pages/$(octave_dirstamp)
	rm -f %reldir%/pages/README.md
	rm -f -r `ls -d %reldir%/* 2>/dev/null | $(GREP) -v 'module\.mk$$\|Doxyfile\.in$$\|README\.md$$\|pages$$'`
