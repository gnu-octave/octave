## NOTE: RANDLIB is self-contained body of code that is required to be built
## with standard 32-bit Fortran integers.
## The library is compiled as a libtool convenience library, without
## $F77_INTEGER_8_FLAG, and is included directly in liboctave rather than
## in libexternal to avoid any possibility of integer size mismatches.

RANDLIB_SRC = \
  %reldir%/advnst.f \
  %reldir%/genbet.f \
  %reldir%/genchi.f \
  %reldir%/genexp.f \
  %reldir%/genf.f \
  %reldir%/gengam.f \
  %reldir%/genmn.f \
  %reldir%/genmul.f \
  %reldir%/gennch.f \
  %reldir%/gennf.f \
  %reldir%/gennor.f \
  %reldir%/genprm.f \
  %reldir%/genunf.f \
  %reldir%/getcgn.f \
  %reldir%/getsd.f \
  %reldir%/ignbin.f \
  %reldir%/ignlgi.f \
  %reldir%/ignnbn.f \
  %reldir%/ignpoi.f \
  %reldir%/ignuin.f \
  %reldir%/initgn.f \
  %reldir%/inrgcm.f \
  %reldir%/lennob.f \
  %reldir%/mltmod.f \
  %reldir%/phrtsd.f \
  %reldir%/qrgnin.f \
  %reldir%/ranf.f \
  %reldir%/setall.f \
  %reldir%/setant.f \
  %reldir%/setgmn.f \
  %reldir%/setsd.f \
  %reldir%/sexpo.f \
  %reldir%/sgamma.f \
  %reldir%/snorm.f \
  %reldir%/wrap.f

## Start library specification
noinst_LTLIBRARIES += %reldir%/librandlib.la

%canon_reldir%_librandlib_la_SOURCES := $(RANDLIB_SRC)

## Note: Must be '=' because libexternal_la_FFLAGS has not been defined yet.
%canon_reldir%_librandlib_la_FFLAGS = \
	$(filter-out $(F77_INTEGER_8_FLAG), \
	             $(liboctave_external_libexternal_la_FFLAGS))

liboctave_liboctave_la_LIBADD += %reldir%/librandlib.la

liboctave_EXTRA_DIST += \
  %reldir%/Basegen.doc \
  %reldir%/HOWTOGET \
  %reldir%/README \
  %reldir%/randlib.chs \
  %reldir%/randlib.fdoc \
  %reldir%/tstbot.for \
  %reldir%/tstgmn.for \
  %reldir%/tstmid.for

