EXTRA_DIST += \
  fftpack/module.mk \
  fftpack/fftpack.doc

FFTPACK_SRC = \
  fftpack/cfftb.f \
  fftpack/cfftb1.f \
  fftpack/cfftf.f \
  fftpack/cfftf1.f \
  fftpack/cffti.f \
  fftpack/cffti1.f \
  fftpack/passb.f \
  fftpack/passb2.f \
  fftpack/passb3.f \
  fftpack/passb4.f \
  fftpack/passb5.f \
  fftpack/passf.f \
  fftpack/passf2.f \
  fftpack/passf3.f \
  fftpack/passf4.f \
  fftpack/passf5.f \
  fftpack/zfftb.f \
  fftpack/zfftb1.f \
  fftpack/zfftf.f \
  fftpack/zfftf1.f \
  fftpack/zffti.f \
  fftpack/zffti1.f \
  fftpack/zpassb.f \
  fftpack/zpassb2.f \
  fftpack/zpassb3.f \
  fftpack/zpassb4.f \
  fftpack/zpassb5.f \
  fftpack/zpassf.f \
  fftpack/zpassf2.f \
  fftpack/zpassf3.f \
  fftpack/zpassf4.f \
  fftpack/zpassf5.f

if AMCOND_HAVE_FFTW
  EXTRA_DIST += $(FFTPACK_SRC)
else
  libcruft_la_SOURCES += $(FFTPACK_SRC)
endif
