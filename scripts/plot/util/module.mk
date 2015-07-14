FCN_FILE_DIRS += \
  scripts/plot/util \
  scripts/plot/util/private

scripts_plot_util_GEN_FCN_FILES = \
  scripts/plot/util/gnuplot_binary.m

GEN_FCN_FILES += $(scripts_plot_util_GEN_FCN_FILES)

scripts_plot_util_PRIVATE_FCN_FILES = \
  scripts/plot/util/private/__add_default_menu__.m \
  scripts/plot/util/private/__ghostscript__.m \
  scripts/plot/util/private/__gnuplot_get_var__.m \
  scripts/plot/util/private/__gnuplot_ginput__.m \
  scripts/plot/util/private/__gnuplot_has_feature__.m \
  scripts/plot/util/private/__gnuplot_has_terminal__.m \
  scripts/plot/util/private/__gnuplot_open_stream__.m \
  scripts/plot/util/private/__gnuplot_print__.m \
  scripts/plot/util/private/__gnuplot_version__.m \
  scripts/plot/util/private/__go_draw_axes__.m \
  scripts/plot/util/private/__go_draw_figure__.m \
  scripts/plot/util/private/__opengl_print__.m \
  scripts/plot/util/private/__print_parse_opts__.m \
  scripts/plot/util/private/__tight_eps_bbox__.m

scripts_plot_util_FCN_FILES = \
  scripts/plot/util/__actual_axis_position__.m \
  scripts/plot/util/allchild.m \
  scripts/plot/util/ancestor.m \
  scripts/plot/util/axes.m \
  scripts/plot/util/cla.m \
  scripts/plot/util/clf.m \
  scripts/plot/util/close.m \
  scripts/plot/util/closereq.m \
  scripts/plot/util/colstyle.m \
  scripts/plot/util/copyobj.m \
  scripts/plot/util/__default_plot_options__.m \
  scripts/plot/util/figure.m \
  scripts/plot/util/findall.m \
  scripts/plot/util/findfigs.m \
  scripts/plot/util/findobj.m \
  scripts/plot/util/frame2im.m \
  scripts/plot/util/gca.m \
  scripts/plot/util/gcbf.m \
  scripts/plot/util/gcbo.m \
  scripts/plot/util/gcf.m \
  scripts/plot/util/gco.m \
  scripts/plot/util/ginput.m \
  scripts/plot/util/__gnuplot_drawnow__.m \
  scripts/plot/util/graphics_toolkit.m \
  scripts/plot/util/hdl2struct.m \
  scripts/plot/util/hggroup.m \
  scripts/plot/util/hgload.m \
  scripts/plot/util/hgsave.m \
  scripts/plot/util/hold.m \
  scripts/plot/util/im2frame.m \
  scripts/plot/util/isaxes.m \
  scripts/plot/util/isfigure.m \
  scripts/plot/util/ishghandle.m \
  scripts/plot/util/ishold.m \
  scripts/plot/util/isprop.m \
  scripts/plot/util/linkaxes.m \
  scripts/plot/util/linkprop.m \
  scripts/plot/util/meshgrid.m \
  scripts/plot/util/ndgrid.m \
  scripts/plot/util/newplot.m \
  scripts/plot/util/__next_line_color__.m \
  scripts/plot/util/__next_line_style__.m \
  scripts/plot/util/pan.m \
  scripts/plot/util/__plt_get_axis_arg__.m \
  scripts/plot/util/__pltopt__.m \
  scripts/plot/util/printd.m \
  scripts/plot/util/print.m \
  scripts/plot/util/refreshdata.m \
  scripts/plot/util/refresh.m \
  scripts/plot/util/rotate.m \
  scripts/plot/util/rotate3d.m \
  scripts/plot/util/saveas.m \
  scripts/plot/util/shg.m \
  scripts/plot/util/struct2hdl.m \
  scripts/plot/util/subplot.m \
  scripts/plot/util/zoom.m

scripts_plot_utildir = $(fcnfiledir)/plot/util

scripts_plot_util_DATA = \
  $(scripts_plot_util_FCN_FILES) \
  $(scripts_plot_util_GEN_FCN_FILES)

scripts_plot_util_privatedir = $(fcnfiledir)/plot/util/private

scripts_plot_util_private_DATA = $(scripts_plot_util_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_plot_util_FCN_FILES) \
  $(scripts_plot_util_GEN_FCN_FILES) \
  $(scripts_plot_util_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/plot/util/PKG_ADD

DIRSTAMP_FILES += scripts/plot/util/$(octave_dirstamp)

