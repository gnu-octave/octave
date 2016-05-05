FCN_FILE_DIRS += \
  scripts/profiler

scripts_profiler_FCN_FILES = \
  scripts/profiler/profexplore.m \
  scripts/profiler/profile.m \
  scripts/profiler/profshow.m

scripts_profilerdir = $(fcnfiledir)/profiler

scripts_profiler_DATA = $(scripts_profiler_FCN_FILES)

FCN_FILES += \
  $(scripts_profiler_FCN_FILES)

PKG_ADD_FILES += scripts/profiler/PKG_ADD

DIRSTAMP_FILES += scripts/profiler/$(octave_dirstamp)
