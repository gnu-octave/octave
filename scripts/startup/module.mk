FCN_FILE_DIRS += startup

startup_FCN_FILES = \
  startup/__finish__.m

SYSTEM_STARTUP_FILE_SRC = startup/main-rcfile

SYSTEM_INPUTRC_FILE_SRC = startup/inputrc

LOCAL_STARTUP_FILE_SRC = startup/local-rcfile

STARTUP_FILE_SRC = \
  $(SYSTEM_STARTUP_FILE_SRC) \
  $(SYSTEM_INPUTRC_FILE_SRC) \
  $(LOCAL_STARTUP_FILE_SRC)

FCN_FILES += \
  $(startup_FCN_FILES) \
  $(SYSTEM_STARTUP_FILE_SRC) \
  $(LOCAL_STARTUP_FILE_SRC)

EXTRA_DIST += \
  $(SYSTEM_INPUTRC_FILE_SRC)

PKG_ADD_FILES += startup/PKG_ADD

DIRSTAMP_FILES += startup/$(octave_dirstamp)
