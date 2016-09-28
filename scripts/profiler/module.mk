FCN_FILE_DIRS += \
  scripts/profiler

scripts_profiler_FCN_FILES = \
  scripts/profiler/profexplore.m \
  scripts/profiler/profexport.m \
  scripts/profiler/profile.m \
  scripts/profiler/profshow.m

scripts_profilerdir = $(fcnfiledir)/profiler
scripts_profiler_DATA = $(scripts_profiler_FCN_FILES)

scripts_profiler_htmldir = $(octetcdir)/profiler
scripts_profiler_html_DATA = \
  scripts/profiler/html/flat.html \
  scripts/profiler/html/flat_entry.html \
  scripts/profiler/html/function.html \
  scripts/profiler/html/hierarchical.html \
  scripts/profiler/html/hierarchical_entry.html \
  scripts/profiler/html/style.css

FCN_FILES += \
  $(scripts_profiler_FCN_FILES)

PKG_ADD_FILES += scripts/profiler/PKG_ADD

DIRSTAMP_FILES += scripts/profiler/$(octave_dirstamp)

scripts_EXTRA_DIST += \
  $(scripts_profiler_html_DATA)
