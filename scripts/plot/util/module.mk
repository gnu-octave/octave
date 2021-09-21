FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_GEN_FCN_FILES = \
  %reldir%/gnuplot_binary.m

GEN_FCN_FILES += $(%canon_reldir%_GEN_FCN_FILES)

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__add_default_menu__.m \
  %reldir%/private/__ghostscript__.m \
  %reldir%/private/__gnuplot_draw_axes__.m \
  %reldir%/private/__gnuplot_draw_figure__.m \
  %reldir%/private/__gnuplot_get_var__.m \
  %reldir%/private/__gnuplot_ginput__.m \
  %reldir%/private/__gnuplot_has_feature__.m \
  %reldir%/private/__gnuplot_has_terminal__.m \
  %reldir%/private/__gnuplot_open_stream__.m \
  %reldir%/private/__gnuplot_print__.m \
  %reldir%/private/__gnuplot_version__.m \
  %reldir%/private/__opengl_print__.m \
  %reldir%/private/__print_parse_opts__.m \
  %reldir%/private/__set_default_mouse_modes__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/__actual_axis_position__.m \
  %reldir%/__check_rendering_capability__.m \
  %reldir%/__default_plot_options__.m \
  %reldir%/__gnuplot_drawnow__.m \
  %reldir%/__next_line_color__.m \
  %reldir%/__next_line_style__.m \
  %reldir%/__opengl_info__.m \
  %reldir%/__plt_get_axis_arg__.m \
  %reldir%/__pltopt__.m \
  %reldir%/allchild.m \
  %reldir%/ancestor.m \
  %reldir%/axes.m \
  %reldir%/cla.m \
  %reldir%/clf.m \
  %reldir%/close.m \
  %reldir%/closereq.m \
  %reldir%/colstyle.m \
  %reldir%/copyobj.m \
  %reldir%/figure.m \
  %reldir%/findall.m \
  %reldir%/findfigs.m \
  %reldir%/findobj.m \
  %reldir%/gca.m \
  %reldir%/gcbf.m \
  %reldir%/gcbo.m \
  %reldir%/gcf.m \
  %reldir%/gco.m \
  %reldir%/ginput.m \
  %reldir%/graphics_toolkit.m \
  %reldir%/groot.m \
  %reldir%/gui_mainfcn.m \
  %reldir%/hdl2struct.m \
  %reldir%/hggroup.m \
  %reldir%/hgload.m \
  %reldir%/hgsave.m \
  %reldir%/hgtransform.m \
  %reldir%/hold.m \
  %reldir%/isaxes.m \
  %reldir%/isfigure.m \
  %reldir%/isgraphics.m \
  %reldir%/ishandle.m \
  %reldir%/ishold.m \
  %reldir%/isprop.m \
  %reldir%/linkaxes.m \
  %reldir%/linkprop.m \
  %reldir%/meshgrid.m \
  %reldir%/ndgrid.m \
  %reldir%/newplot.m \
  %reldir%/openfig.m \
  %reldir%/pan.m \
  %reldir%/print.m \
  %reldir%/printd.m \
  %reldir%/refresh.m \
  %reldir%/refreshdata.m \
  %reldir%/rotate.m \
  %reldir%/rotate3d.m \
  %reldir%/saveas.m \
  %reldir%/savefig.m \
  %reldir%/shg.m \
  %reldir%/struct2hdl.m \
  %reldir%/subplot.m \
  %reldir%/zoom.m

%canon_reldir%dir = $(fcnfiledir)/plot/util

%canon_reldir%_DATA = \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_GEN_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/plot/util/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
