EXTERNAL_SOURCES += \
  liboctave/external/slatec-fn/albeta.f \
  liboctave/external/slatec-fn/alngam.f \
  liboctave/external/slatec-fn/alnrel.f \
  liboctave/external/slatec-fn/algams.f \
  liboctave/external/slatec-fn/acosh.f \
  liboctave/external/slatec-fn/asinh.f \
  liboctave/external/slatec-fn/atanh.f \
  liboctave/external/slatec-fn/betai.f \
  liboctave/external/slatec-fn/csevl.f \
  liboctave/external/slatec-fn/d9gmit.f \
  liboctave/external/slatec-fn/d9lgic.f \
  liboctave/external/slatec-fn/d9lgit.f \
  liboctave/external/slatec-fn/d9lgmc.f \
  liboctave/external/slatec-fn/dacosh.f \
  liboctave/external/slatec-fn/dasinh.f \
  liboctave/external/slatec-fn/datanh.f \
  liboctave/external/slatec-fn/dbetai.f \
  liboctave/external/slatec-fn/dcsevl.f \
  liboctave/external/slatec-fn/derf.f \
  liboctave/external/slatec-fn/dgami.f \
  liboctave/external/slatec-fn/dgamit.f \
  liboctave/external/slatec-fn/dgamlm.f \
  liboctave/external/slatec-fn/dgamma.f \
  liboctave/external/slatec-fn/dgamr.f \
  liboctave/external/slatec-fn/dlbeta.f \
  liboctave/external/slatec-fn/dlgams.f \
  liboctave/external/slatec-fn/dlngam.f \
  liboctave/external/slatec-fn/dlnrel.f \
  liboctave/external/slatec-fn/dpchim.f \
  liboctave/external/slatec-fn/dpchst.f \
  liboctave/external/slatec-fn/dpsifn.f \
  liboctave/external/slatec-fn/erf.f \
  liboctave/external/slatec-fn/gami.f \
  liboctave/external/slatec-fn/gamit.f \
  liboctave/external/slatec-fn/gamlim.f \
  liboctave/external/slatec-fn/gamma.f \
  liboctave/external/slatec-fn/gamr.f \
  liboctave/external/slatec-fn/initds.f \
  liboctave/external/slatec-fn/inits.f \
  liboctave/external/slatec-fn/pchim.f \
  liboctave/external/slatec-fn/pchst.f \
  liboctave/external/slatec-fn/psifn.f \
  liboctave/external/slatec-fn/r9lgmc.f \
  liboctave/external/slatec-fn/r9lgit.f \
  liboctave/external/slatec-fn/r9gmit.f \
  liboctave/external/slatec-fn/r9lgic.f \
  liboctave/external/slatec-fn/xdacosh.f \
  liboctave/external/slatec-fn/xdasinh.f \
  liboctave/external/slatec-fn/xdatanh.f \
  liboctave/external/slatec-fn/xdbetai.f \
  liboctave/external/slatec-fn/xderf.f \
  liboctave/external/slatec-fn/xderfc.f \
  liboctave/external/slatec-fn/xdgami.f \
  liboctave/external/slatec-fn/xdgamit.f \
  liboctave/external/slatec-fn/xdgamma.f \
  liboctave/external/slatec-fn/xgmainc.f \
  liboctave/external/slatec-fn/xacosh.f \
  liboctave/external/slatec-fn/xasinh.f \
  liboctave/external/slatec-fn/xatanh.f \
  liboctave/external/slatec-fn/xerf.f \
  liboctave/external/slatec-fn/xerfc.f \
  liboctave/external/slatec-fn/xsgmainc.f \
  liboctave/external/slatec-fn/xgamma.f \
  liboctave/external/slatec-fn/xbetai.f

nodist_liboctave_external_libexternal_la_SOURCES += \
  liboctave/external/slatec-fn/derfc.f \
  liboctave/external/slatec-fn/erfc.f

liboctave/external/slatec-fn/erfc.f: liboctave/external/slatec-fn/erfc.in.f build-aux/subst-f77-isnan-macro.sh | liboctave/external/slatec-fn/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-f77-isnan-macro.sh)

liboctave/external/slatec-fn/derfc.f: liboctave/external/slatec-fn/derfc.in.f build-aux/subst-f77-isnan-macro.sh | liboctave/external/slatec-fn/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-f77-isnan-macro.sh)

liboctave_EXTRA_DIST += \
  liboctave/external/slatec-fn/derfc.in.f \
  liboctave/external/slatec-fn/erfc.in.f

DIRSTAMP_FILES += liboctave/external/slatec-fn/$(octave_dirstamp)
