FCN_FILE_DIRS += \
  scripts/plot/appearance \
  scripts/plot/appearance/private

scripts_plot_appearance_PRIVATE_FCN_FILES = \
  scripts/plot/appearance/private/__axis_limits__.m \
  scripts/plot/appearance/private/__axis_label__.m

scripts_plot_appearance_FCN_FILES = \
  scripts/plot/appearance/__clabel__.m \
  scripts/plot/appearance/__getlegenddata__.m \
  scripts/plot/appearance/annotation.m \
  scripts/plot/appearance/axis.m \
  scripts/plot/appearance/box.m \
  scripts/plot/appearance/caxis.m \
  scripts/plot/appearance/clabel.m \
  scripts/plot/appearance/daspect.m \
  scripts/plot/appearance/datetick.m \
  scripts/plot/appearance/diffuse.m \
  scripts/plot/appearance/grid.m \
  scripts/plot/appearance/gtext.m \
  scripts/plot/appearance/hidden.m \
  scripts/plot/appearance/legend.m \
  scripts/plot/appearance/lighting.m \
  scripts/plot/appearance/material.m \
  scripts/plot/appearance/orient.m \
  scripts/plot/appearance/pbaspect.m \
  scripts/plot/appearance/shading.m \
  scripts/plot/appearance/specular.m \
  scripts/plot/appearance/text.m \
  scripts/plot/appearance/title.m \
  scripts/plot/appearance/view.m \
  scripts/plot/appearance/whitebg.m \
  scripts/plot/appearance/xlabel.m \
  scripts/plot/appearance/xlim.m \
  scripts/plot/appearance/ylabel.m \
  scripts/plot/appearance/ylim.m \
  scripts/plot/appearance/zlabel.m \
  scripts/plot/appearance/zlim.m

scripts_plot_appearancedir = $(fcnfiledir)/plot/appearance

scripts_plot_appearance_DATA = $(scripts_plot_appearance_FCN_FILES)

scripts_plot_appearance_privatedir = $(fcnfiledir)/plot/appearance/private

scripts_plot_appearance_private_DATA = $(scripts_plot_appearance_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_plot_appearance_FCN_FILES) \
  $(scripts_plot_appearance_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/plot/appearance/PKG_ADD

DIRSTAMP_FILES += scripts/plot/appearance/$(octave_dirstamp)

