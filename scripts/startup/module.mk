FCN_FILE_DIRS += %reldir%

SITE_STARTUP_FILE_SRC = %reldir%/site-rcfile

VERSION_STARTUP_FILE_SRC = %reldir%/version-rcfile

SYSTEM_INPUTRC_FILE_SRC = %reldir%/inputrc

SYSTEM_PKG_LIST_FILE_SRC = %reldir%/octave_packages

STARTUP_FILE_SRC := \
  $(SITE_STARTUP_FILE_SRC) \
  $(VERSION_STARTUP_FILE_SRC) \
  $(SYSTEM_INPUTRC_FILE_SRC) \
  $(SYSTEM_PKG_LIST_FILE_SRC)

%canon_reldir%dir = $(fcnfiledir)/startup

install-data-local: install-startup-files

uninstall-local: uninstall-startup-files

install-startup-files:
	$(MKDIR_P) $(DESTDIR)$(fcnfiledir)/startup
	if [ -f $(DESTDIR)$(fcnfiledir)/startup/octaverc ]; then true; \
	else \
	  $(INSTALL_DATA) $(srcdir)/$(VERSION_STARTUP_FILE_SRC) \
	    $(DESTDIR)$(fcnfiledir)/startup/octaverc; \
	fi
	if [ -f $(DESTDIR)$(fcnfiledir)/startup/inputrc ]; then true; \
	else \
	  $(INSTALL_DATA) $(srcdir)/$(SYSTEM_INPUTRC_FILE_SRC) \
	    $(DESTDIR)$(fcnfiledir)/startup/inputrc; \
	fi
	$(MKDIR_P) $(DESTDIR)$(localfcnfiledir)/startup
	if [ -f $(DESTDIR)$(localfcnfiledir)/startup/octaverc ]; then true; \
	else \
	  $(INSTALL_DATA) $(srcdir)/$(SITE_STARTUP_FILE_SRC) \
	    $(DESTDIR)$(localfcnfiledir)/startup/octaverc; \
	fi
	$(MKDIR_P) $(DESTDIR)$(localapipkgdir)
	if [ -f $(DESTDIR)$(localapipkgdir)/octave_packages ]; then true; \
	else \
	  $(INSTALL_DATA) $(srcdir)/$(SYSTEM_PKG_LIST_FILE_SRC) \
	    $(DESTDIR)$(localapipkgdir)/octave_packages; \
	fi
.PHONY: install-startup-files

uninstall-startup-files:
	rm -f $(DESTDIR)$(fcnfiledir)/startup/octaverc
	rm -f $(DESTDIR)$(fcnfiledir)/startup/inputrc
	rm -f $(DESTDIR)$(localfcnfiledir)/startup/octaverc
	rm -f $(DESTDIR)$(localapipkgdir)/octave_packages
.PHONY: uninstall-startup-files

scripts_EXTRA_DIST += $(STARTUP_FILE_SRC)
