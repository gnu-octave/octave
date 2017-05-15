doxyhtml: %reldir%/Doxyfile | %reldir%/$(octave_dirstamp)
	doxygen %reldir%/Doxyfile

doxyhtml-maintainer-clean:
	rm -f doc/doxygen_sqlite3.db
	rm -rf `ls -d %reldir%/* 2>/dev/null | $(GREP) -v 'Doxyfile\.in\|README'`

doc_EXTRA_DIST += \
  %reldir%/Doxyfile.in \
  %reldir%/README

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
