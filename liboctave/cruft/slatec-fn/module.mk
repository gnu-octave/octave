CRUFT_SOURCES += \
  liboctave/cruft/slatec-fn/albeta.f \
  liboctave/cruft/slatec-fn/alngam.f \
  liboctave/cruft/slatec-fn/alnrel.f \
  liboctave/cruft/slatec-fn/algams.f \
  liboctave/cruft/slatec-fn/acosh.f \
  liboctave/cruft/slatec-fn/asinh.f \
  liboctave/cruft/slatec-fn/atanh.f \
  liboctave/cruft/slatec-fn/betai.f \
  liboctave/cruft/slatec-fn/csevl.f \
  liboctave/cruft/slatec-fn/d9gmit.f \
  liboctave/cruft/slatec-fn/d9lgic.f \
  liboctave/cruft/slatec-fn/d9lgit.f \
  liboctave/cruft/slatec-fn/d9lgmc.f \
  liboctave/cruft/slatec-fn/dacosh.f \
  liboctave/cruft/slatec-fn/dasinh.f \
  liboctave/cruft/slatec-fn/datanh.f \
  liboctave/cruft/slatec-fn/dbetai.f \
  liboctave/cruft/slatec-fn/dcsevl.f \
  liboctave/cruft/slatec-fn/derf.f \
  liboctave/cruft/slatec-fn/dgami.f \
  liboctave/cruft/slatec-fn/dgamit.f \
  liboctave/cruft/slatec-fn/dgamlm.f \
  liboctave/cruft/slatec-fn/dgamma.f \
  liboctave/cruft/slatec-fn/dgamr.f \
  liboctave/cruft/slatec-fn/dlbeta.f \
  liboctave/cruft/slatec-fn/dlgams.f \
  liboctave/cruft/slatec-fn/dlngam.f \
  liboctave/cruft/slatec-fn/dlnrel.f \
  liboctave/cruft/slatec-fn/dpchim.f \
  liboctave/cruft/slatec-fn/dpchst.f \
  liboctave/cruft/slatec-fn/dpsifn.f \
  liboctave/cruft/slatec-fn/erf.f \
  liboctave/cruft/slatec-fn/gami.f \
  liboctave/cruft/slatec-fn/gamit.f \
  liboctave/cruft/slatec-fn/gamlim.f \
  liboctave/cruft/slatec-fn/gamma.f \
  liboctave/cruft/slatec-fn/gamr.f \
  liboctave/cruft/slatec-fn/initds.f \
  liboctave/cruft/slatec-fn/inits.f \
  liboctave/cruft/slatec-fn/pchim.f \
  liboctave/cruft/slatec-fn/pchst.f \
  liboctave/cruft/slatec-fn/psifn.f \
  liboctave/cruft/slatec-fn/r9lgmc.f \
  liboctave/cruft/slatec-fn/r9lgit.f \
  liboctave/cruft/slatec-fn/r9gmit.f \
  liboctave/cruft/slatec-fn/r9lgic.f \
  liboctave/cruft/slatec-fn/xdacosh.f \
  liboctave/cruft/slatec-fn/xdasinh.f \
  liboctave/cruft/slatec-fn/xdatanh.f \
  liboctave/cruft/slatec-fn/xdbetai.f \
  liboctave/cruft/slatec-fn/xderf.f \
  liboctave/cruft/slatec-fn/xderfc.f \
  liboctave/cruft/slatec-fn/xdgami.f \
  liboctave/cruft/slatec-fn/xdgamit.f \
  liboctave/cruft/slatec-fn/xdgamma.f \
  liboctave/cruft/slatec-fn/xgmainc.f \
  liboctave/cruft/slatec-fn/xacosh.f \
  liboctave/cruft/slatec-fn/xasinh.f \
  liboctave/cruft/slatec-fn/xatanh.f \
  liboctave/cruft/slatec-fn/xerf.f \
  liboctave/cruft/slatec-fn/xerfc.f \
  liboctave/cruft/slatec-fn/xsgmainc.f \
  liboctave/cruft/slatec-fn/xgamma.f \
  liboctave/cruft/slatec-fn/xbetai.f

nodist_liboctave_cruft_libcruft_la_SOURCES += \
  liboctave/cruft/slatec-fn/derfc.f \
  liboctave/cruft/slatec-fn/erfc.f

## slatec-fn directory may not exist in VPATH build; create it if necessary.

define do-subst-isnan-macro
  rm -f $@-t $@ && \
  $(MKDIR_P) liboctave/cruft/slatec-fn && \
  $(SED) -e "${F77_ISNAN_MACRO}" < $< > $@-t && \
  mv $@-t $@
endef

liboctave/cruft/slatec-fn/erfc.f: liboctave/cruft/slatec-fn/erfc.in.f Makefile
	$(AM_V_GEN)$(do-subst-isnan-macro)

liboctave/cruft/slatec-fn/derfc.f: liboctave/cruft/slatec-fn/derfc.in.f Makefile
	$(AM_V_GEN)$(do-subst-isnan-macro)

liboctave_EXTRA_DIST += \
  liboctave/cruft/slatec-fn/derfc.in.f \
  liboctave/cruft/slatec-fn/erfc.in.f

