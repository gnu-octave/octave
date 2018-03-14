FCN_FILE_DIRS += \
  scripts/profiler

%canon_reldir%_FCN_FILES = \
  %reldir%/profexplore.m \
  %reldir%/profexport.m \
  %reldir%/profile.m \
  %reldir%/profshow.m

%canon_reldir%dir = $(fcnfiledir)/profiler

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_htmldir = $(octetcdir)/profiler

%canon_reldir%_html_DATA = \
  %reldir%/html/flat.html \
  %reldir%/html/flat_entry.html \
  %reldir%/html/function.html \
  %reldir%/html/hierarchical.html \
  %reldir%/html/hierarchical_entry.html \
  %reldir%/html/style.css

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

scripts_EXTRA_DIST += \
  $(%canon_reldir%_html_DATA)
