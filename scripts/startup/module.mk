FCN_FILE_DIRS += scripts/startup

scripts_startup_FCN_FILES = \
  scripts/startup/__finish__.m

LOCAL_STARTUP_FILE_SRC  = scripts/startup/local-rcfile

SYSTEM_STARTUP_FILE_SRC = scripts/startup/main-rcfile

SYSTEM_INPUTRC_FILE_SRC = scripts/startup/inputrc

STARTUP_FILE_SRC = \
  $(LOCAL_STARTUP_FILE_SRC) \
  $(SYSTEM_STARTUP_FILE_SRC) \
  $(SYSTEM_INPUTRC_FILE_SRC)

scripts_startupdir = $(fcnfiledir)/startup

scripts_startup_DATA = $(scripts_startup_FCN_FILES)

FCN_FILES += $(scripts_startup_FCN_FILES)

PKG_ADD_FILES += scripts/startup/PKG_ADD

DIRSTAMP_FILES += scripts/startup/$(octave_dirstamp)

scripts_EXTRA_DIST += $(STARTUP_FILE_SRC)
