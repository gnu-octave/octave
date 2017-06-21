EXTERNAL_SOURCES += \
  %reldir%/albeta.f \
  %reldir%/alngam.f \
  %reldir%/alnrel.f \
  %reldir%/algams.f \
  %reldir%/betai.f \
  %reldir%/csevl.f \
  %reldir%/d9gmit.f \
  %reldir%/d9lgic.f \
  %reldir%/d9lgit.f \
  %reldir%/d9lgmc.f \
  %reldir%/dbetai.f \
  %reldir%/dcsevl.f \
  %reldir%/dgami.f \
  %reldir%/dgamit.f \
  %reldir%/dgamlm.f \
  %reldir%/dgamma.f \
  %reldir%/dgamr.f \
  %reldir%/dlbeta.f \
  %reldir%/dlgams.f \
  %reldir%/dlngam.f \
  %reldir%/dlnrel.f \
  %reldir%/dpchim.f \
  %reldir%/dpchst.f \
  %reldir%/dpsifn.f \
  %reldir%/gami.f \
  %reldir%/gamit.f \
  %reldir%/gamlim.f \
  %reldir%/gamma.f \
  %reldir%/gamr.f \
  %reldir%/initds.f \
  %reldir%/inits.f \
  %reldir%/pchim.f \
  %reldir%/pchst.f \
  %reldir%/psifn.f \
  %reldir%/r9lgmc.f \
  %reldir%/r9lgit.f \
  %reldir%/r9gmit.f \
  %reldir%/r9lgic.f \
  %reldir%/xdbetai.f \
  %reldir%/xdgami.f \
  %reldir%/xdgamit.f \
  %reldir%/xdgamma.f \
  %reldir%/xgmainc.f \
  %reldir%/xsgmainc.f \
  %reldir%/xgamma.f \
  %reldir%/xbetai.f

liboctave_EXTRA_DIST += \
  %reldir%/derfc.in.f \
  %reldir%/erfc.in.f

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
