FCN_FILE_DIRS += %reldir%

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/convhull.m \
  %reldir%/delaunay.m \
  %reldir%/delaunayn.m \
  %reldir%/dsearch.m \
  %reldir%/dsearchn.m \
  %reldir%/griddata.m \
  %reldir%/griddata3.m \
  %reldir%/griddatan.m \
  %reldir%/inpolygon.m \
  %reldir%/rectint.m \
  %reldir%/rotx.m \
  %reldir%/roty.m \
  %reldir%/rotz.m \
  %reldir%/tsearchn.m \
  %reldir%/voronoi.m \
  %reldir%/voronoin.m

%canon_reldir%dir = $(fcnfiledir)/geometry

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
