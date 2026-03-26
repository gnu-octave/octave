DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

COREFCN_SYSTEM_SRC = \
  %reldir%/__ftp__.cc \
  %reldir%/dirfns.cc \
  %reldir%/dlmread.cc \
  %reldir%/getgrent.cc \
  %reldir%/getpwent.cc \
  %reldir%/getrusage.cc \
  %reldir%/nproc.cc \
  %reldir%/syscalls.cc \
  %reldir%/time.cc \
  %reldir%/urlwrite.cc

%reldir%/oct-errno.cc: %reldir%/oct-errno.in.cc %reldir%/mk-errno-list.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	if test -n "$(PERL)"; then \
	  $(SHELL) $(srcdir)/%reldir%/mk-errno-list.sh --perl "$(PERL)" < $< > $@-t; \
	elif test -n "$(PYTHON)"; then \
	  $(SHELL) $(srcdir)/%reldir%/mk-errno-list.sh --python "$(PYTHON)" < $< > $@-t; \
	else \
	  $(SHELL) $(srcdir)/%reldir%/mk-errno-list.sh --sed "$(SED)" < $< > $@-t; \
	fi && \
	mv $@-t $@

noinst_LTLIBRARIES += \
  %reldir%/libsystem.la

%canon_reldir%_libsystem_la_SOURCES = $(COREFCN_SYSTEM_SRC)

%canon_reldir%_libsystem_la_CPPFLAGS = \
  $(libinterp_liboctinterp_la_CPPFLAGS)

libinterp_EXTRA_DIST += \
  %reldir%/mk-errno-list.sh \
  %reldir%/oct-errno.in.cc

libinterp_liboctinterp_la_LIBADD += %reldir%/libsystem.la
