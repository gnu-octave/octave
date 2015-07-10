EXTRA_DIST += \
  liboctave/cruft/fftpack/module.mk \
  liboctave/cruft/fftpack/fftpack.doc

FFTPACK_SRC = \
  liboctave/cruft/fftpack/cfftb.f \
  liboctave/cruft/fftpack/cfftb1.f \
  liboctave/cruft/fftpack/cfftf.f \
  liboctave/cruft/fftpack/cfftf1.f \
  liboctave/cruft/fftpack/cffti.f \
  liboctave/cruft/fftpack/cffti1.f \
  liboctave/cruft/fftpack/passb.f \
  liboctave/cruft/fftpack/passb2.f \
  liboctave/cruft/fftpack/passb3.f \
  liboctave/cruft/fftpack/passb4.f \
  liboctave/cruft/fftpack/passb5.f \
  liboctave/cruft/fftpack/passf.f \
  liboctave/cruft/fftpack/passf2.f \
  liboctave/cruft/fftpack/passf3.f \
  liboctave/cruft/fftpack/passf4.f \
  liboctave/cruft/fftpack/passf5.f \
  liboctave/cruft/fftpack/zfftb.f \
  liboctave/cruft/fftpack/zfftb1.f \
  liboctave/cruft/fftpack/zfftf.f \
  liboctave/cruft/fftpack/zfftf1.f \
  liboctave/cruft/fftpack/zffti.f \
  liboctave/cruft/fftpack/zffti1.f \
  liboctave/cruft/fftpack/zpassb.f \
  liboctave/cruft/fftpack/zpassb2.f \
  liboctave/cruft/fftpack/zpassb3.f \
  liboctave/cruft/fftpack/zpassb4.f \
  liboctave/cruft/fftpack/zpassb5.f \
  liboctave/cruft/fftpack/zpassf.f \
  liboctave/cruft/fftpack/zpassf2.f \
  liboctave/cruft/fftpack/zpassf3.f \
  liboctave/cruft/fftpack/zpassf4.f \
  liboctave/cruft/fftpack/zpassf5.f

if AMCOND_HAVE_FFTW
  EXTRA_DIST += $(FFTPACK_SRC)
else
  CRUFT_SOURCES += $(FFTPACK_SRC)
endif
