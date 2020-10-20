FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/@audioplayer \
  %reldir%/@audiorecorder

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/lin2mu.m \
  %reldir%/mu2lin.m \
  %reldir%/record.m \
  %reldir%/sound.m \
  %reldir%/soundsc.m

%canon_reldir%_@audioplayer_FCN_FILES = \
  %reldir%/@audioplayer/__get_properties__.m \
  %reldir%/@audioplayer/audioplayer.m \
  %reldir%/@audioplayer/disp.m \
  %reldir%/@audioplayer/get.m \
  %reldir%/@audioplayer/isplaying.m \
  %reldir%/@audioplayer/pause.m  \
  %reldir%/@audioplayer/play.m \
  %reldir%/@audioplayer/playblocking.m \
  %reldir%/@audioplayer/resume.m \
  %reldir%/@audioplayer/set.m \
  %reldir%/@audioplayer/stop.m \
  %reldir%/@audioplayer/subsasgn.m \
  %reldir%/@audioplayer/subsref.m

%canon_reldir%_@audiorecorder_FCN_FILES = \
  %reldir%/@audiorecorder/__get_properties__.m \
  %reldir%/@audiorecorder/audiorecorder.m \
  %reldir%/@audiorecorder/disp.m \
  %reldir%/@audiorecorder/get.m \
  %reldir%/@audiorecorder/getaudiodata.m \
  %reldir%/@audiorecorder/getplayer.m \
  %reldir%/@audiorecorder/isrecording.m \
  %reldir%/@audiorecorder/pause.m \
  %reldir%/@audiorecorder/play.m \
  %reldir%/@audiorecorder/record.m \
  %reldir%/@audiorecorder/recordblocking.m \
  %reldir%/@audiorecorder/resume.m \
  %reldir%/@audiorecorder/set.m \
  %reldir%/@audiorecorder/stop.m \
  %reldir%/@audiorecorder/subsasgn.m \
  %reldir%/@audiorecorder/subsref.m

%canon_reldir%dir = $(fcnfiledir)/audio

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_@audioplayerdir = $(fcnfiledir)/audio/@audioplayer

%canon_reldir%_@audioplayer_DATA = $(%canon_reldir%_@audioplayer_FCN_FILES)

%canon_reldir%_@audiorecorderdir = $(fcnfiledir)/audio/@audiorecorder

%canon_reldir%_@audiorecorder_DATA = $(%canon_reldir%_@audiorecorder_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_@audioplayer_FCN_FILES) \
  $(%canon_reldir%_@audiorecorder_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
