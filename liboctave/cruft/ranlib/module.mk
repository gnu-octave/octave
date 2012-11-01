EXTRA_DIST += \
  ranlib/module.mk \
  ranlib/Basegen.doc \
  ranlib/HOWTOGET \
  ranlib/README \
  ranlib/randlib.chs \
  ranlib/randlib.fdoc \
  ranlib/tstbot.for \
  ranlib/tstgmn.for \
  ranlib/tstmid.for

RANLIB_SRC = \
  ranlib/advnst.f \
  ranlib/genbet.f \
  ranlib/genchi.f \
  ranlib/genexp.f \
  ranlib/genf.f \
  ranlib/gengam.f \
  ranlib/genmn.f \
  ranlib/genmul.f \
  ranlib/gennch.f \
  ranlib/gennf.f \
  ranlib/gennor.f \
  ranlib/genprm.f \
  ranlib/genunf.f \
  ranlib/getcgn.f \
  ranlib/getsd.f \
  ranlib/ignbin.f \
  ranlib/ignlgi.f \
  ranlib/ignnbn.f \
  ranlib/ignpoi.f \
  ranlib/ignuin.f \
  ranlib/initgn.f \
  ranlib/inrgcm.f \
  ranlib/lennob.f \
  ranlib/mltmod.f \
  ranlib/phrtsd.f \
  ranlib/qrgnin.f \
  ranlib/ranf.f \
  ranlib/setall.f \
  ranlib/setant.f \
  ranlib/setgmn.f \
  ranlib/setsd.f \
  ranlib/sexpo.f \
  ranlib/sgamma.f \
  ranlib/snorm.f \
  ranlib/wrap.f

noinst_LTLIBRARIES += ranlib/libranlib.la

ranlib_libranlib_la_SOURCES = $(RANLIB_SRC)

ranlib_libranlib_la_DEPENDENCIES = ranlib/ranlib.def

## Special rules for files which must be built before compilation
ranlib/ranlib.def: $(RANLIB_SRC) mkf77def
	chmod a+rx mkf77def
	./mkf77def $(srcdir) $(RANLIB_SRC) > $@-t
	mv $@-t $@

