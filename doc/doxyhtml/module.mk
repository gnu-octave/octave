# Generate README.md from README and replace first line by a Doxygen
# specific one.
%reldir%/pages/README.md: $(srcdir)/README
	$(MKDIR_P) $(@D)
	cat $< | $(SED) '1s/.*/notitle {#mainpage}/; 2s/.*/=======/' > $@

DOXYGEN_PAGES = \
  %reldir%/pages/macros.dox \
  %reldir%/pages/README.md

doxyhtml: %reldir%/Doxyfile $(DOXYGEN_PAGES) | %reldir%/$(octave_dirstamp)
	doxygen %reldir%/Doxyfile

# This target is important for in source tree builds.
doxyhtml-maintainer-clean:
	$(RM) %reldir%/pages/README.md
	$(RM) -r `ls -d %reldir%/* 2>/dev/null | $(GREP) -v 'module\.mk\|Doxyfile\.in\|README\|pages$$'`

doc_EXTRA_DIST += \
  $(DOXYGEN_PAGES) \
  %reldir%/Doxyfile.in \
  %reldir%/README.md

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
