FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/rectangle_lw.m  \
  %reldir%/private/rectangle_sw.m  \
  %reldir%/private/triangle_lw.m  \
  %reldir%/private/triangle_sw.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/__parse_movargs__.m \
  %reldir%/arch_fit.m \
  %reldir%/arch_rnd.m \
  %reldir%/arch_test.m \
  %reldir%/arma_rnd.m \
  %reldir%/autoreg_matrix.m \
  %reldir%/bartlett.m \
  %reldir%/blackman.m \
  %reldir%/detrend.m \
  %reldir%/diffpara.m \
  %reldir%/durbinlevinson.m \
  %reldir%/fftconv.m \
  %reldir%/fftfilt.m \
  %reldir%/fftshift.m \
  %reldir%/filter2.m \
  %reldir%/fractdiff.m \
  %reldir%/freqz.m \
  %reldir%/freqz_plot.m \
  %reldir%/hamming.m \
  %reldir%/hanning.m \
  %reldir%/hurst.m \
  %reldir%/ifftshift.m \
  %reldir%/movfun.m \
  %reldir%/movslice.m \
  %reldir%/periodogram.m \
  %reldir%/sinc.m \
  %reldir%/sinetone.m \
  %reldir%/sinewave.m \
  %reldir%/spectral_adf.m \
  %reldir%/spectral_xdf.m \
  %reldir%/spencer.m \
  %reldir%/stft.m \
  %reldir%/synthesis.m \
  %reldir%/unwrap.m \
  %reldir%/yulewalker.m

%canon_reldir%dir = $(fcnfiledir)/signal

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/signal/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
