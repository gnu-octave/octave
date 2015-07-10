FCN_FILE_DIRS += scripts/signal

scripts_signal_PRIVATE_FCN_FILES = \
  scripts/signal/private/rectangle_lw.m  \
  scripts/signal/private/rectangle_sw.m  \
  scripts/signal/private/triangle_lw.m  \
  scripts/signal/private/triangle_sw.m

scripts_signal_FCN_FILES = \
  scripts/signal/arch_fit.m \
  scripts/signal/arch_rnd.m \
  scripts/signal/arch_test.m \
  scripts/signal/arma_rnd.m \
  scripts/signal/autoreg_matrix.m \
  scripts/signal/bartlett.m \
  scripts/signal/blackman.m \
  scripts/signal/detrend.m \
  scripts/signal/diffpara.m \
  scripts/signal/durbinlevinson.m \
  scripts/signal/fftconv.m \
  scripts/signal/fftfilt.m \
  scripts/signal/fftshift.m \
  scripts/signal/filter2.m \
  scripts/signal/fractdiff.m \
  scripts/signal/freqz.m \
  scripts/signal/freqz_plot.m \
  scripts/signal/hamming.m \
  scripts/signal/hanning.m \
  scripts/signal/hurst.m \
  scripts/signal/ifftshift.m \
  scripts/signal/periodogram.m \
  scripts/signal/sinc.m \
  scripts/signal/sinetone.m \
  scripts/signal/sinewave.m \
  scripts/signal/spectral_adf.m \
  scripts/signal/spectral_xdf.m \
  scripts/signal/spencer.m \
  scripts/signal/stft.m \
  scripts/signal/synthesis.m \
  scripts/signal/unwrap.m \
  scripts/signal/yulewalker.m \
  $(scripts_signal_PRIVATE_FCN_FILES)

FCN_FILES += $(scripts_signal_FCN_FILES)

PKG_ADD_FILES += scripts/signal/PKG_ADD

DIRSTAMP_FILES += scripts/signal/$(octave_dirstamp)
