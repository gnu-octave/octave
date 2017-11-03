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

doxyhtml-maintainer-clean:
	rm -f doc/doxygen_sqlite3.db
	rm -rf `ls -d %reldir%/* 2>/dev/null | $(GREP) -v 'Doxyfile\.in\|README\|pages$'`

doc_EXTRA_DIST += \
  $(DOXYGEN_PAGES) \
  %reldir%/Doxyfile.in \
  %reldir%/README

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
