FCN_FILE_DIRS += plot

plot_GEN_FCN_FILES = \
  plot/gnuplot_binary.m

GEN_FCN_FILES += $(plot_GEN_FCN_FILES)

plot_PRIVATE_FCN_FILES = \
  plot/private/__actual_axis_position__.m \
  plot/private/__add_datasource__.m \
  plot/private/__axes_limits__.m \
  plot/private/__axis_label__.m \
  plot/private/__bar__.m \
  plot/private/__clabel__.m \
  plot/private/__color_str_rgb__.m \
  plot/private/__contour__.m \
  plot/private/__default_plot_options__.m \
  plot/private/__errcomm__.m \
  plot/private/__errplot__.m \
  plot/private/__ezplot__.m \
  plot/private/__interp_cube__.m \
  plot/private/__line__.m \
  plot/private/__patch__.m \
  plot/private/__plt__.m \
  plot/private/__pltopt__.m \
  plot/private/__quiver__.m \
  plot/private/__scatter__.m \
  plot/private/__stem__.m

plot_FCN_FILES = \
  plot/__gnuplot_get_var__.m \
  plot/__gnuplot_ginput__.m \
  plot/__gnuplot_has_feature__.m \
  plot/__gnuplot_open_stream__.m \
  plot/__gnuplot_version__.m \
  plot/__go_close_all__.m \
  plot/__go_draw_axes__.m \
  plot/__go_draw_figure__.m \
  plot/__marching_cube__.m \
  plot/__next_line_color__.m \
  plot/__next_line_style__.m \
  plot/__plt_get_axis_arg__.m \
  plot/allchild.m \
  plot/ancestor.m \
  plot/area.m \
  plot/axes.m \
  plot/axis.m \
  plot/backend.m \
  plot/bar.m \
  plot/barh.m \
  plot/box.m \
  plot/caxis.m \
  plot/cla.m \
  plot/clabel.m \
  plot/clf.m \
  plot/close.m \
  plot/closereq.m \
  plot/colorbar.m \
  plot/comet.m \
  plot/compass.m \
  plot/contour.m \
  plot/contour3.m \
  plot/contourc.m \
  plot/contourf.m \
  plot/cylinder.m \
  plot/daspect.m \
  plot/diffuse.m \
  plot/ellipsoid.m \
  plot/errorbar.m \
  plot/ezcontour.m \
  plot/ezcontourf.m \
  plot/ezmesh.m \
  plot/ezmeshc.m \
  plot/ezplot.m \
  plot/ezplot3.m \
  plot/ezpolar.m \
  plot/ezsurf.m \
  plot/ezsurfc.m \
  plot/feather.m \
  plot/figure.m \
  plot/fill.m \
  plot/findall.m \
  plot/findobj.m \
  plot/fplot.m \
  plot/gca.m \
  plot/gcbf.m \
  plot/gcbo.m \
  plot/gcf.m \
  plot/ginput.m \
  plot/gnuplot_drawnow.m \
  plot/grid.m \
  plot/gtext.m \
  plot/hggroup.m \
  plot/hidden.m \
  plot/hist.m \
  plot/hold.m \
  plot/isfigure.m \
  plot/ishghandle.m \
  plot/ishold.m \
  plot/isocolors.m \
  plot/isonormals.m \
  plot/isosurface.m \
  plot/legend.m \
  plot/line.m \
  plot/linkprop.m \
  plot/loglog.m \
  plot/loglogerr.m \
  plot/mesh.m \
  plot/meshc.m \
  plot/meshgrid.m \
  plot/meshz.m \
  plot/ndgrid.m \
  plot/newplot.m \
  plot/orient.m \
  plot/pareto.m \
  plot/patch.m \
  plot/pbaspect.m \
  plot/pcolor.m \
  plot/peaks.m \
  plot/pie.m \
  plot/plot.m \
  plot/plot3.m \
  plot/plotmatrix.m \
  plot/plotyy.m \
  plot/polar.m \
  plot/print.m \
  plot/quiver.m \
  plot/quiver3.m \
  plot/refresh.m \
  plot/refreshdata.m \
  plot/replot.m \
  plot/ribbon.m \
  plot/rose.m \
  plot/scatter.m \
  plot/scatter3.m \
  plot/semilogx.m \
  plot/semilogxerr.m \
  plot/semilogy.m \
  plot/semilogyerr.m \
  plot/shading.m \
  plot/shg.m \
  plot/slice.m \
  plot/sombrero.m \
  plot/specular.m \
  plot/sphere.m \
  plot/spinmap.m \
  plot/stairs.m \
  plot/stem.m \
  plot/stem3.m \
  plot/subplot.m \
  plot/surf.m \
  plot/surface.m \
  plot/surfc.m \
  plot/surfl.m \
  plot/surfnorm.m \
  plot/text.m \
  plot/title.m \
  plot/view.m \
  plot/waitforbuttonpress.m \
  plot/xlabel.m \
  plot/xlim.m \
  plot/ylabel.m \
  plot/ylim.m \
  plot/zlabel.m \
  plot/zlim.m \
  $(plot_PRIVATE_FCN_FILES)

FCN_FILES += $(plot_FCN_FILES)

PKG_ADD_FILES += plot/PKG_ADD

DIRSTAMP_FILES += plot/$(octave_dirstamp)
