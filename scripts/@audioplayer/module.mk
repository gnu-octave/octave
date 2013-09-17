FCN_FILE_DIRS += @audioplayer

@audioplayer_FCN_FILES = \
  @audioplayer/getproperties.m \
  @audioplayer/audioplayer.m \
  @audioplayer/display.m  \
  @audioplayer/get.m  \
  @audioplayer/isplaying.m  \
  @audioplayer/pause.m  \
  @audioplayer/play.m  \
  @audioplayer/playblocking.m  \
  @audioplayer/resume.m  \
  @audioplayer/set.m  \
  @audioplayer/stop.m  \
  @audioplayer/subsasgn.m  \
  @audioplayer/susbref.m

FCN_FILES += $(@audioplayer_FCN_FILES)

PKG_ADD_FILES += @audioplayer/PKG_ADD

DIRSTAMP_FILES += @audioplayer/$(octave_dirstamp)
