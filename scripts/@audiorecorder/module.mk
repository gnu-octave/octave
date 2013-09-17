FCN_FILE_DIRS += @audiorecorder

@audiorecorder_FCN_FILES = \
  @audiorecorder/getproperties.m \
  @audiorecorder/audiorecorder.m \
  @audiorecorder/display.m \
  @audiorecorder/get.m \
  @audiorecorder/getaudiodata.m \
  @audiorecorder/getplayer.m \
  @audiorecorder/isrecording.m \
  @audiorecorder/pause.m \
  @audiorecorder/play.m \
  @audiorecorder/record.m \
  @audiorecorder/recordblocking.m \
  @audiorecorder/resume.m \
  @audiorecorder/set.m \
  @audiorecorder/stop.m  \
  @audiorecorder/subsasgn.m  \
  @audiorecorder/susbref.m

FCN_FILES += $(@audiorecorder_FCN_FILES)

PKG_ADD_FILES += @audiorecorder/PKG_ADD

DIRSTAMP_FILES += @audiorecorder/$(octave_dirstamp)
