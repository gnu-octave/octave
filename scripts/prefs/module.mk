FCN_FILE_DIRS += \
  scripts/prefs \
  scripts/prefs/private

scripts_prefs_PRIVATE_FCN_FILES = \
  scripts/prefs/private/loadprefs.m \
  scripts/prefs/private/prefsfile.m \
  scripts/prefs/private/saveprefs.m

scripts_prefs_FCN_FILES = \
  scripts/prefs/addpref.m \
  scripts/prefs/getpref.m \
  scripts/prefs/ispref.m \
  scripts/prefs/prefdir.m \
  scripts/prefs/preferences.m \
  scripts/prefs/rmpref.m \
  scripts/prefs/setpref.m

scripts_prefsdir = $(fcnfiledir)/prefs

scripts_prefs_DATA = $(scripts_prefs_FCN_FILES)

scripts_prefs_privatedir = $(fcnfiledir)/prefs/private

scripts_prefs_private_DATA = $(scripts_prefs_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_prefs_FCN_FILES) \
  $(scripts_prefs_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/prefs/PKG_ADD

DIRSTAMP_FILES += scripts/prefs/$(octave_dirstamp)
