FCN_FILE_DIRS += scripts/geometry

scripts_geometry_FCN_FILES = \
  scripts/geometry/convhull.m \
  scripts/geometry/delaunayn.m \
  scripts/geometry/delaunay.m \
  scripts/geometry/dsearch.m \
  scripts/geometry/dsearchn.m \
  scripts/geometry/griddata.m \
  scripts/geometry/griddata3.m \
  scripts/geometry/griddatan.m \
  scripts/geometry/inpolygon.m \
  scripts/geometry/rectint.m \
  scripts/geometry/tsearchn.m \
  scripts/geometry/voronoi.m \
  scripts/geometry/voronoin.m

FCN_FILES += $(scripts_geometry_FCN_FILES)

PKG_ADD_FILES += scripts/geometry/PKG_ADD

DIRSTAMP_FILES += scripts/geometry/$(octave_dirstamp)
