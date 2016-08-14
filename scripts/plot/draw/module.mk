FCN_FILE_DIRS += \
  scripts/plot/draw \
  scripts/plot/draw/private

scripts_plot_draw_PRIVATE_FCN_FILES = \
  scripts/plot/draw/private/__add_datasource__.m \
  scripts/plot/draw/private/__bar__.m \
  scripts/plot/draw/private/__calc_isovalue_from_data__.m \
  scripts/plot/draw/private/__contour__.m \
  scripts/plot/draw/private/__errplot__.m \
  scripts/plot/draw/private/__ezplot__.m \
  scripts/plot/draw/private/__interp_cube__.m \
  scripts/plot/draw/private/__line__.m \
  scripts/plot/draw/private/__marching_cube__.m \
  scripts/plot/draw/private/__patch__.m \
  scripts/plot/draw/private/__pie__.m \
  scripts/plot/draw/private/__plt__.m \
  scripts/plot/draw/private/__quiver__.m \
  scripts/plot/draw/private/__rotate_around_axis__.m \
  scripts/plot/draw/private/__scatter__.m \
  scripts/plot/draw/private/__stem__.m \
  scripts/plot/draw/private/__unite_shared_vertices__.m

scripts_plot_draw_FCN_FILES = \
  scripts/plot/draw/area.m \
  scripts/plot/draw/barh.m \
  scripts/plot/draw/bar.m \
  scripts/plot/draw/camlight.m \
  scripts/plot/draw/colorbar.m \
  scripts/plot/draw/comet3.m \
  scripts/plot/draw/comet.m \
  scripts/plot/draw/compass.m \
  scripts/plot/draw/contour3.m \
  scripts/plot/draw/contourc.m \
  scripts/plot/draw/contourf.m \
  scripts/plot/draw/contour.m \
  scripts/plot/draw/cylinder.m \
  scripts/plot/draw/ellipsoid.m \
  scripts/plot/draw/errorbar.m \
  scripts/plot/draw/ezcontourf.m \
  scripts/plot/draw/ezcontour.m \
  scripts/plot/draw/ezmeshc.m \
  scripts/plot/draw/ezmesh.m \
  scripts/plot/draw/ezplot3.m \
  scripts/plot/draw/ezplot.m \
  scripts/plot/draw/ezpolar.m \
  scripts/plot/draw/ezsurfc.m \
  scripts/plot/draw/ezsurf.m \
  scripts/plot/draw/feather.m \
  scripts/plot/draw/fill.m \
  scripts/plot/draw/fplot.m \
  scripts/plot/draw/hist.m \
  scripts/plot/draw/isocaps.m \
  scripts/plot/draw/isocolors.m \
  scripts/plot/draw/isonormals.m \
  scripts/plot/draw/isosurface.m \
  scripts/plot/draw/light.m \
  scripts/plot/draw/line.m \
  scripts/plot/draw/loglogerr.m \
  scripts/plot/draw/loglog.m \
  scripts/plot/draw/meshc.m \
  scripts/plot/draw/mesh.m \
  scripts/plot/draw/meshz.m \
  scripts/plot/draw/pareto.m \
  scripts/plot/draw/patch.m \
  scripts/plot/draw/pcolor.m \
  scripts/plot/draw/peaks.m \
  scripts/plot/draw/pie3.m \
  scripts/plot/draw/pie.m \
  scripts/plot/draw/plot3.m \
  scripts/plot/draw/plot.m \
  scripts/plot/draw/plotmatrix.m \
  scripts/plot/draw/plotyy.m \
  scripts/plot/draw/polar.m \
  scripts/plot/draw/quiver3.m \
  scripts/plot/draw/quiver.m \
  scripts/plot/draw/rectangle.m \
  scripts/plot/draw/reducepatch.m \
  scripts/plot/draw/reducevolume.m \
  scripts/plot/draw/ribbon.m \
  scripts/plot/draw/rose.m \
  scripts/plot/draw/scatter3.m \
  scripts/plot/draw/scatter.m \
  scripts/plot/draw/semilogxerr.m \
  scripts/plot/draw/semilogx.m \
  scripts/plot/draw/semilogyerr.m \
  scripts/plot/draw/semilogy.m \
  scripts/plot/draw/shrinkfaces.m \
  scripts/plot/draw/slice.m \
  scripts/plot/draw/smooth3.m \
  scripts/plot/draw/sombrero.m \
  scripts/plot/draw/sphere.m \
  scripts/plot/draw/stairs.m \
  scripts/plot/draw/stem3.m \
  scripts/plot/draw/stemleaf.m \
  scripts/plot/draw/stem.m \
  scripts/plot/draw/surface.m \
  scripts/plot/draw/surfc.m \
  scripts/plot/draw/surfl.m \
  scripts/plot/draw/surf.m \
  scripts/plot/draw/surfnorm.m \
  scripts/plot/draw/tetramesh.m \
  scripts/plot/draw/trimesh.m \
  scripts/plot/draw/triplot.m \
  scripts/plot/draw/trisurf.m \
  scripts/plot/draw/waterfall.m

scripts_plot_drawdir = $(fcnfiledir)/plot/draw

scripts_plot_draw_DATA = $(scripts_plot_draw_FCN_FILES)

scripts_plot_draw_privatedir = $(fcnfiledir)/plot/draw/private

scripts_plot_draw_private_DATA = $(scripts_plot_draw_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_plot_draw_FCN_FILES) \
  $(scripts_plot_draw_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/plot/draw/PKG_ADD

DIRSTAMP_FILES += scripts/plot/draw/$(octave_dirstamp)

