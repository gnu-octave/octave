EXTRA_DIST += \
  slatec-fn/module.mk \
  slatec-fn/derfc.in.f \
  slatec-fn/erfc.in.f

libcruft_la_SOURCES += \
  slatec-fn/albeta.f \
  slatec-fn/alngam.f \
  slatec-fn/alnrel.f \
  slatec-fn/algams.f \
  slatec-fn/acosh.f \
  slatec-fn/asinh.f \
  slatec-fn/atanh.f \
  slatec-fn/betai.f \
  slatec-fn/csevl.f \
  slatec-fn/d9gmit.f \
  slatec-fn/d9lgic.f \
  slatec-fn/d9lgit.f \
  slatec-fn/d9lgmc.f \
  slatec-fn/dacosh.f \
  slatec-fn/dasinh.f \
  slatec-fn/datanh.f \
  slatec-fn/dbetai.f \
  slatec-fn/dcsevl.f \
  slatec-fn/derf.f \
  slatec-fn/dgami.f \
  slatec-fn/dgamit.f \
  slatec-fn/dgamlm.f \
  slatec-fn/dgamma.f \
  slatec-fn/dgamr.f \
  slatec-fn/dlbeta.f \
  slatec-fn/dlgams.f \
  slatec-fn/dlngam.f \
  slatec-fn/dlnrel.f \
  slatec-fn/dpchim.f \
  slatec-fn/dpchst.f \
  slatec-fn/erf.f \
  slatec-fn/gami.f \
  slatec-fn/gamit.f \
  slatec-fn/gamlim.f \
  slatec-fn/gamma.f \
  slatec-fn/gamr.f \
  slatec-fn/initds.f \
  slatec-fn/inits.f \
  slatec-fn/pchim.f \
  slatec-fn/pchst.f \
  slatec-fn/r9lgmc.f \
  slatec-fn/r9lgit.f \
  slatec-fn/r9gmit.f \
  slatec-fn/r9lgic.f \
  slatec-fn/xdacosh.f \
  slatec-fn/xdasinh.f \
  slatec-fn/xdatanh.f \
  slatec-fn/xdbetai.f \
  slatec-fn/xderf.f \
  slatec-fn/xderfc.f \
  slatec-fn/xdgami.f \
  slatec-fn/xdgamit.f \
  slatec-fn/xdgamma.f \
  slatec-fn/xgmainc.f \
  slatec-fn/xacosh.f \
  slatec-fn/xasinh.f \
  slatec-fn/xatanh.f \
  slatec-fn/xerf.f \
  slatec-fn/xerfc.f \
  slatec-fn/xsgmainc.f \
  slatec-fn/xgamma.f \
  slatec-fn/xbetai.f

nodist_libcruft_la_SOURCES += \
  slatec-fn/derfc.f \
  slatec-fn/erfc.f

slatec-fn/erfc.f: slatec-fn/erfc.in.f Makefile
	$(SED) -e "${F77_ISNAN_MACRO}" < $< > $@-t
	mv $@-t $@

slatec-fn/derfc.f: slatec-fn/derfc.in.f Makefile
	$(SED) -e "${F77_ISNAN_MACRO}" < $< > $@-t
	mv $@-t $@

