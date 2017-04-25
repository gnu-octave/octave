FFTPACK_SRC = \
  liboctave/external/fftpack/cfftb.f \
  liboctave/external/fftpack/cfftb1.f \
  liboctave/external/fftpack/cfftf.f \
  liboctave/external/fftpack/cfftf1.f \
  liboctave/external/fftpack/cffti.f \
  liboctave/external/fftpack/cffti1.f \
  liboctave/external/fftpack/passb.f \
  liboctave/external/fftpack/passb2.f \
  liboctave/external/fftpack/passb3.f \
  liboctave/external/fftpack/passb4.f \
  liboctave/external/fftpack/passb5.f \
  liboctave/external/fftpack/passf.f \
  liboctave/external/fftpack/passf2.f \
  liboctave/external/fftpack/passf3.f \
  liboctave/external/fftpack/passf4.f \
  liboctave/external/fftpack/passf5.f \
  liboctave/external/fftpack/zfftb.f \
  liboctave/external/fftpack/zfftb1.f \
  liboctave/external/fftpack/zfftf.f \
  liboctave/external/fftpack/zfftf1.f \
  liboctave/external/fftpack/zffti.f \
  liboctave/external/fftpack/zffti1.f \
  liboctave/external/fftpack/zpassb.f \
  liboctave/external/fftpack/zpassb2.f \
  liboctave/external/fftpack/zpassb3.f \
  liboctave/external/fftpack/zpassb4.f \
  liboctave/external/fftpack/zpassb5.f \
  liboctave/external/fftpack/zpassf.f \
  liboctave/external/fftpack/zpassf2.f \
  liboctave/external/fftpack/zpassf3.f \
  liboctave/external/fftpack/zpassf4.f \
  liboctave/external/fftpack/zpassf5.f

if AMCOND_HAVE_FFTW
  liboctave_EXTRA_DIST += $(FFTPACK_SRC)
else
  EXTERNAL_SOURCES += $(FFTPACK_SRC)
endif

liboctave_EXTRA_DIST += \
  liboctave/external/fftpack/fftpack.doc
