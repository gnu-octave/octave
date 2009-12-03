FCN_FILE_DIRS += audio

audio_FCN_FILES = \
  audio/lin2mu.m \
  audio/loadaudio.m \
  audio/mu2lin.m \
  audio/playaudio.m \
  audio/record.m \
  audio/saveaudio.m \
  audio/setaudio.m \
  audio/wavread.m \
  audio/wavwrite.m

FCN_FILES += $(audio_FCN_FILES)

PKG_ADD_FILES += audio/PKG_ADD

DIRSTAMP_FILES += audio/$(octave_dirstamp)
