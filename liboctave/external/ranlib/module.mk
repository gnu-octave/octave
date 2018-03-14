RANLIB_SRC = \
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

noinst_LTLIBRARIES += %reldir%/libranlib.la

%canon_reldir%_libranlib_la_SOURCES = $(RANLIB_SRC)

%canon_reldir%_libranlib_la_DEPENDENCIES = %reldir%/ranlib.def

## Special rules for files which must be built before compilation
## ranlib directory may not exist in VPATH build; create it if necessary.
%reldir%/ranlib.def: $(RANLIB_SRC) %reldir%/../mk-f77-def.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) %reldir%/../mk-f77-def.sh $(srcdir) $(RANLIB_SRC) > $@-t && \
	mv $@-t $@

liboctave_liboctave_la_LIBADD += %reldir%/libranlib.la

liboctave_EXTRA_DIST += \
  %reldir%/Basegen.doc \
  %reldir%/HOWTOGET \
  %reldir%/README \
  %reldir%/randlib.chs \
  %reldir%/randlib.fdoc \
  %reldir%/tstbot.for \
  %reldir%/tstgmn.for \
  %reldir%/tstmid.for

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
