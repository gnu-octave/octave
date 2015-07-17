RANLIB_SRC = \
  liboctave/cruft/ranlib/advnst.f \
  liboctave/cruft/ranlib/genbet.f \
  liboctave/cruft/ranlib/genchi.f \
  liboctave/cruft/ranlib/genexp.f \
  liboctave/cruft/ranlib/genf.f \
  liboctave/cruft/ranlib/gengam.f \
  liboctave/cruft/ranlib/genmn.f \
  liboctave/cruft/ranlib/genmul.f \
  liboctave/cruft/ranlib/gennch.f \
  liboctave/cruft/ranlib/gennf.f \
  liboctave/cruft/ranlib/gennor.f \
  liboctave/cruft/ranlib/genprm.f \
  liboctave/cruft/ranlib/genunf.f \
  liboctave/cruft/ranlib/getcgn.f \
  liboctave/cruft/ranlib/getsd.f \
  liboctave/cruft/ranlib/ignbin.f \
  liboctave/cruft/ranlib/ignlgi.f \
  liboctave/cruft/ranlib/ignnbn.f \
  liboctave/cruft/ranlib/ignpoi.f \
  liboctave/cruft/ranlib/ignuin.f \
  liboctave/cruft/ranlib/initgn.f \
  liboctave/cruft/ranlib/inrgcm.f \
  liboctave/cruft/ranlib/lennob.f \
  liboctave/cruft/ranlib/mltmod.f \
  liboctave/cruft/ranlib/phrtsd.f \
  liboctave/cruft/ranlib/qrgnin.f \
  liboctave/cruft/ranlib/ranf.f \
  liboctave/cruft/ranlib/setall.f \
  liboctave/cruft/ranlib/setant.f \
  liboctave/cruft/ranlib/setgmn.f \
  liboctave/cruft/ranlib/setsd.f \
  liboctave/cruft/ranlib/sexpo.f \
  liboctave/cruft/ranlib/sgamma.f \
  liboctave/cruft/ranlib/snorm.f \
  liboctave/cruft/ranlib/wrap.f

noinst_LTLIBRARIES += liboctave/cruft/ranlib/libranlib.la

liboctave_cruft_ranlib_libranlib_la_SOURCES = $(RANLIB_SRC)

liboctave_cruft_ranlib_libranlib_la_DEPENDENCIES = liboctave/cruft/ranlib/ranlib.def

define gen-ranlib-def
  rm -f $@-t $@ && \
  $(MKDIR_P) liboctave/cruft/ranlib && \
  $(SHELL) liboctave/cruft/mkf77def $(top_srcdir) $(RANLIB_SRC) > $@-t && \
  mv $@-t $@
endef

## Special rules for files which must be built before compilation
## ranlib directory may not exist in VPATH build; create it if necessary.
liboctave/cruft/ranlib/ranlib.def: $(RANLIB_SRC) liboctave/cruft/mkf77def
	$(AM_V_GEN)$(gen-ranlib-def)

liboctave_liboctave_la_LIBADD += liboctave/cruft/ranlib/libranlib.la

liboctave_EXTRA_DIST += \
  liboctave/cruft/ranlib/Basegen.doc \
  liboctave/cruft/ranlib/HOWTOGET \
  liboctave/cruft/ranlib/README \
  liboctave/cruft/ranlib/randlib.chs \
  liboctave/cruft/ranlib/randlib.fdoc \
  liboctave/cruft/ranlib/tstbot.for \
  liboctave/cruft/ranlib/tstgmn.for \
  liboctave/cruft/ranlib/tstmid.for
