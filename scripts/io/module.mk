FCN_FILE_DIRS += io

io_FCN_FILES = \
  io/beep.m \
  io/csvread.m \
  io/csvwrite.m \
  io/dlmwrite.m \
  io/strread.m \
  io/textread.m

FCN_FILES += $(io_FCN_FILES)

PKG_ADD_FILES += io/PKG_ADD

DIRSTAMP_FILES += io/$(octave_dirstamp)
