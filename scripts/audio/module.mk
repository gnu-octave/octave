FCN_FILE_DIRS += \
  scripts/audio \
  scripts/audio/@audioplayer \
  scripts/audio/@audiorecorder

scripts_audio_FCN_FILES = \
  scripts/audio/lin2mu.m \
  scripts/audio/mu2lin.m \
  scripts/audio/record.m \
  scripts/audio/sound.m \
  scripts/audio/soundsc.m

scripts_audio_@audioplayer_FCN_FILES = \
  scripts/audio/@audioplayer/__get_properties__.m \
  scripts/audio/@audioplayer/audioplayer.m \
  scripts/audio/@audioplayer/disp.m \
  scripts/audio/@audioplayer/display.m \
  scripts/audio/@audioplayer/get.m \
  scripts/audio/@audioplayer/isplaying.m \
  scripts/audio/@audioplayer/pause.m  \
  scripts/audio/@audioplayer/play.m \
  scripts/audio/@audioplayer/playblocking.m \
  scripts/audio/@audioplayer/resume.m \
  scripts/audio/@audioplayer/set.m \
  scripts/audio/@audioplayer/stop.m \
  scripts/audio/@audioplayer/subsasgn.m \
  scripts/audio/@audioplayer/subsref.m

scripts_audio_@audiorecorder_FCN_FILES = \
  scripts/audio/@audiorecorder/__get_properties__.m \
  scripts/audio/@audiorecorder/audiorecorder.m \
  scripts/audio/@audiorecorder/disp.m \
  scripts/audio/@audiorecorder/display.m \
  scripts/audio/@audiorecorder/get.m \
  scripts/audio/@audiorecorder/getaudiodata.m \
  scripts/audio/@audiorecorder/getplayer.m \
  scripts/audio/@audiorecorder/isrecording.m \
  scripts/audio/@audiorecorder/pause.m \
  scripts/audio/@audiorecorder/play.m \
  scripts/audio/@audiorecorder/record.m \
  scripts/audio/@audiorecorder/recordblocking.m \
  scripts/audio/@audiorecorder/resume.m \
  scripts/audio/@audiorecorder/set.m \
  scripts/audio/@audiorecorder/stop.m \
  scripts/audio/@audiorecorder/subsasgn.m \
  scripts/audio/@audiorecorder/subsref.m

scripts_audiodir = $(fcnfiledir)/audio

scripts_audio_DATA = $(scripts_audio_FCN_FILES)

scripts_audio_@audioplayerdir = $(fcnfiledir)/audio/@audioplayer

scripts_audio_@audioplayer_DATA = $(scripts_audio_@audioplayer_FCN_FILES)

scripts_audio_@audiorecorderdir = $(fcnfiledir)/audio/@audiorecorder

scripts_audio_@audiorecorder_DATA = $(scripts_audio_@audiorecorder_FCN_FILES)

FCN_FILES += \
  $(scripts_audio_FCN_FILES) \
  $(scripts_audio_@audioplayer_FCN_FILES) \
  $(scripts_audio_@audiorecorder_FCN_FILES)

PKG_ADD_FILES += scripts/audio/PKG_ADD

DIRSTAMP_FILES += scripts/audio/$(octave_dirstamp)
