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

## Special rules for files which must be built before compilation

%canon_reldir%_librandlib_la_DEPENDENCIES = %reldir%/randlib.def

## randlib directory may not exist in VPATH build; create it if necessary.
%reldir%/randlib.def: $(RANDLIB_SRC) %reldir%/../mk-f77-def.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) %reldir%/../mk-f77-def.sh $(srcdir) $(RANDLIB_SRC) > $@-t && \
	mv $@-t $@

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

## Start library specification
noinst_LTLIBRARIES += %reldir%/librandlib.la

%canon_reldir%_librandlib_la_SOURCES := $(RANDLIB_SRC)

%canon_reldir%_librandlib_la_FFLAGS = $(liboctave_libexternal_la_FFLAGS)

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

liboctave_DISTCLEANFILES += %reldir%/randlib.def

