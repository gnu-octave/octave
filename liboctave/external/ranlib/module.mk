RANLIB_SRC = \
  liboctave/external/ranlib/advnst.f \
  liboctave/external/ranlib/genbet.f \
  liboctave/external/ranlib/genchi.f \
  liboctave/external/ranlib/genexp.f \
  liboctave/external/ranlib/genf.f \
  liboctave/external/ranlib/gengam.f \
  liboctave/external/ranlib/genmn.f \
  liboctave/external/ranlib/genmul.f \
  liboctave/external/ranlib/gennch.f \
  liboctave/external/ranlib/gennf.f \
  liboctave/external/ranlib/gennor.f \
  liboctave/external/ranlib/genprm.f \
  liboctave/external/ranlib/genunf.f \
  liboctave/external/ranlib/getcgn.f \
  liboctave/external/ranlib/getsd.f \
  liboctave/external/ranlib/ignbin.f \
  liboctave/external/ranlib/ignlgi.f \
  liboctave/external/ranlib/ignnbn.f \
  liboctave/external/ranlib/ignpoi.f \
  liboctave/external/ranlib/ignuin.f \
  liboctave/external/ranlib/initgn.f \
  liboctave/external/ranlib/inrgcm.f \
  liboctave/external/ranlib/lennob.f \
  liboctave/external/ranlib/mltmod.f \
  liboctave/external/ranlib/phrtsd.f \
  liboctave/external/ranlib/qrgnin.f \
  liboctave/external/ranlib/ranf.f \
  liboctave/external/ranlib/setall.f \
  liboctave/external/ranlib/setant.f \
  liboctave/external/ranlib/setgmn.f \
  liboctave/external/ranlib/setsd.f \
  liboctave/external/ranlib/sexpo.f \
  liboctave/external/ranlib/sgamma.f \
  liboctave/external/ranlib/snorm.f \
  liboctave/external/ranlib/wrap.f

noinst_LTLIBRARIES += liboctave/external/ranlib/libranlib.la

liboctave_external_ranlib_libranlib_la_SOURCES = $(RANLIB_SRC)

liboctave_external_ranlib_libranlib_la_DEPENDENCIES = liboctave/external/ranlib/ranlib.def

## Special rules for files which must be built before compilation
## ranlib directory may not exist in VPATH build; create it if necessary.
liboctave/external/ranlib/ranlib.def: $(RANLIB_SRC) build-aux/mk-f77-def.sh | liboctave/external/ranlib/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) build-aux/mk-f77-def.sh $(srcdir) $(RANLIB_SRC) > $@-t && \
	mv $@-t $@

liboctave_liboctave_la_LIBADD += liboctave/external/ranlib/libranlib.la

liboctave_EXTRA_DIST += \
  liboctave/external/ranlib/Basegen.doc \
  liboctave/external/ranlib/HOWTOGET \
  liboctave/external/ranlib/README \
  liboctave/external/ranlib/randlib.chs \
  liboctave/external/ranlib/randlib.fdoc \
  liboctave/external/ranlib/tstbot.for \
  liboctave/external/ranlib/tstgmn.for \
  liboctave/external/ranlib/tstmid.for

DIRSTAMP_FILES += liboctave/external/ranlib/$(octave_dirstamp)
