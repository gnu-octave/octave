doxyhtml: doc/doxyhtml/Doxyfile | doc/doxyhtml/$(octave_dirstamp)
	doxygen doc/doxyhtml/Doxyfile

doxyhtml-maintainer-clean:
	rm -f doc/doxygen_sqlite3.db
	rm -rf `ls -d doc/doxyhtml/* 2>/dev/null | $(GREP) -v 'Doxyfile\.in\|README'`

doc_EXTRA_DIST += \
  doc/doxyhtml/Doxyfile.in \
  doc/doxyhtml/README

DIRSTAMP_FILES += doc/doxyhtml/$(octave_dirstamp)
