mex_TEST_FILES = \
  %reldir%/bug-54096.tst \
  %reldir%/bug-51725.tst \
  %reldir%/mexnumtst.tst \
  $(MEX_TEST_SRC)

MEX_TEST_SRC = \
  %reldir%/bug_54096.c \
  %reldir%/bug_51725.c \
  %reldir%/mexnumtst.c

MEX_TEST_FUNCTIONS := $(MEX_TEST_SRC:%.c=%.mex)

## Since these definitions for MKOCT and MKMEX are only used here, defining
## them in this file is probably OK.  If they are ever used elsewhere, then
## maybe they could be moved to build-aux/module.mk or to test/Makefile.am
## file.  The MKOCT variables are included for completeness, in case we someday
## want to test building .oct files as well.

AM_V_mkmex = $(am__v_mkmex_@AM_V@)
am__v_mkmex_ = $(am__v_mkmex_@AM_DEFAULT_V@)
am__v_mkmex_0 = @echo "  MKMEX   " $@;
am__v_mkmex_1 =

AM_VOPT_mkmex = $(am__vopt_mkmex_@AM_V@)
am__vopt_mkmex_ = $(am__vopt_mkmex_@AM_DEFAULT_V@)
am__vopt_mkmex_0 =
am__vopt_mkmex_1 = -v

## This list is probably incomplete.  It has never been tested.
MKOCT_CPPFLAGS = \
  -I$(top_builddir) \
  -I$(top_srcdir)/liboctinterp/corefcn

## NOTE: Must not be "_LDFLAGS" which is reserved by Automake.
MKOCT_LDFLAG = \
  -L$(top_builddir)/liboctinterp/.libs \
  -L$(top_builddir)/liboctave/.libs

MKOCT = \
  OCT_LINK_DEPS="$(DLDFCN_LINK_DEPS)" \
  DL_LDFLAGS="$(DL_LDFLAGS)" \
  $(top_builddir)/src/mkoctfile $(MKOCT_CPPFLAGS) $(MKOCT_LDFLAG)

MKMEX_CPPFLAGS = \
  -I$(top_builddir) \
  -I$(top_srcdir)/liboctmex \
  -I$(top_builddir)/liboctinterp/interp

## NOTE: Must not be "_LDFLAGS" which is reserved by Automake.
MKMEX_LDFLAG = \
  -L$(top_builddir)/liboctmex/.libs

MKMEX = \
  OCT_LINK_DEPS="" \
  DL_LDFLAGS="$(DL_LDFLAGS)" \
  $(top_builddir)/src/mkoctfile --mex $(MKMEX_CPPFLAGS) $(MKMEX_LDFLAG)

$(MEX_TEST_FUNCTIONS) : %.mex : %.c | %reldir%/$(octave_dirstamp)
	$(AM_V_mkmex)$(MKMEX) $(AM_VOPT_mkmex) $< -o $@ || rm -f $@

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

## Until we decide how to handle installing the executable MEX files,
## don't install them or the associated test files.
noinst_TEST_FILES += $(mex_TEST_FILES)

CLEANFILES += $(MEX_TEST_FUNCTIONS)
