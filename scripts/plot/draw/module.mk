FCN_FILE_DIRS += \
  scripts/plot/draw \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__add_datasource__.m \
  %reldir%/private/__bar__.m \
  %reldir%/private/__calc_isovalue_from_data__.m \
  %reldir%/private/__contour__.m \
  %reldir%/private/__errplot__.m \
  %reldir%/private/__ezplot__.m \
  %reldir%/private/__interp_cube__.m \
  %reldir%/private/__line__.m \
  %reldir%/private/__marching_cube__.m \
  %reldir%/private/__patch__.m \
  %reldir%/private/__pie__.m \
  %reldir%/private/__plt__.m \
  %reldir%/private/__quiver__.m \
  %reldir%/private/__rotate_around_axis__.m \
  %reldir%/private/__scatter__.m \
  %reldir%/private/__stem__.m \
  %reldir%/private/__unite_shared_vertices__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/area.m \
  %reldir%/barh.m \
  %reldir%/bar.m \
  %reldir%/camlight.m \
  %reldir%/colorbar.m \
  %reldir%/comet3.m \
  %reldir%/comet.m \
  %reldir%/compass.m \
  %reldir%/contour3.m \
  %reldir%/contourc.m \
  %reldir%/contourf.m \
  %reldir%/contour.m \
  %reldir%/cylinder.m \
  %reldir%/ellipsoid.m \
  %reldir%/errorbar.m \
  %reldir%/ezcontourf.m \
  %reldir%/ezcontour.m \
  %reldir%/ezmeshc.m \
  %reldir%/ezmesh.m \
  %reldir%/ezplot3.m \
  %reldir%/ezplot.m \
  %reldir%/ezpolar.m \
  %reldir%/ezsurfc.m \
  %reldir%/ezsurf.m \
  %reldir%/feather.m \
  %reldir%/fill.m \
  %reldir%/fplot.m \
  %reldir%/hist.m \
  %reldir%/isocaps.m \
  %reldir%/isocolors.m \
  %reldir%/isonormals.m \
  %reldir%/isosurface.m \
  %reldir%/light.m \
  %reldir%/line.m \
  %reldir%/loglogerr.m \
  %reldir%/loglog.m \
  %reldir%/meshc.m \
  %reldir%/mesh.m \
  %reldir%/meshz.m \
  %reldir%/pareto.m \
  %reldir%/patch.m \
  %reldir%/pcolor.m \
  %reldir%/peaks.m \
  %reldir%/pie3.m \
  %reldir%/pie.m \
  %reldir%/plot3.m \
  %reldir%/plot.m \
  %reldir%/plotmatrix.m \
  %reldir%/plotyy.m \
  %reldir%/polar.m \
  %reldir%/quiver3.m \
  %reldir%/quiver.m \
  %reldir%/rectangle.m \
  %reldir%/reducepatch.m \
  %reldir%/reducevolume.m \
  %reldir%/ribbon.m \
  %reldir%/rose.m \
  %reldir%/scatter3.m \
  %reldir%/scatter.m \
  %reldir%/semilogxerr.m \
  %reldir%/semilogx.m \
  %reldir%/semilogyerr.m \
  %reldir%/semilogy.m \
  %reldir%/shrinkfaces.m \
  %reldir%/slice.m \
  %reldir%/smooth3.m \
  %reldir%/sombrero.m \
  %reldir%/sphere.m \
  %reldir%/stairs.m \
  %reldir%/stem3.m \
  %reldir%/stemleaf.m \
  %reldir%/stem.m \
  %reldir%/surface.m \
  %reldir%/surfc.m \
  %reldir%/surfl.m \
  %reldir%/surf.m \
  %reldir%/surfnorm.m \
  %reldir%/tetramesh.m \
  %reldir%/trimesh.m \
  %reldir%/triplot.m \
  %reldir%/trisurf.m \
  %reldir%/waterfall.m

%canon_reldir%dir = $(fcnfiledir)/plot/draw

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/plot/draw/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
