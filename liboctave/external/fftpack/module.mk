FFTPACK_SRC = \
  %reldir%/cfftb.f \
  %reldir%/cfftb1.f \
  %reldir%/cfftf.f \
  %reldir%/cfftf1.f \
  %reldir%/cffti.f \
  %reldir%/cffti1.f \
  %reldir%/passb.f \
  %reldir%/passb2.f \
  %reldir%/passb3.f \
  %reldir%/passb4.f \
  %reldir%/passb5.f \
  %reldir%/passf.f \
  %reldir%/passf2.f \
  %reldir%/passf3.f \
  %reldir%/passf4.f \
  %reldir%/passf5.f \
  %reldir%/zfftb.f \
  %reldir%/zfftb1.f \
  %reldir%/zfftf.f \
  %reldir%/zfftf1.f \
  %reldir%/zffti.f \
  %reldir%/zffti1.f \
  %reldir%/zpassb.f \
  %reldir%/zpassb2.f \
  %reldir%/zpassb3.f \
  %reldir%/zpassb4.f \
  %reldir%/zpassb5.f \
  %reldir%/zpassf.f \
  %reldir%/zpassf2.f \
  %reldir%/zpassf3.f \
  %reldir%/zpassf4.f \
  %reldir%/zpassf5.f

if AMCOND_HAVE_FFTW
  liboctave_EXTRA_DIST += $(FFTPACK_SRC)
else
  EXTERNAL_SOURCES += $(FFTPACK_SRC)
endif

liboctave_EXTRA_DIST += \
  %reldir%/fftpack.doc
