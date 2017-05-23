FCN_FILE_DIRS += \
  scripts/plot/appearance \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__axis_label__.m \
  %reldir%/private/__axis_limits__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/__clabel__.m \
  %reldir%/__getlegenddata__.m \
  %reldir%/annotation.m \
  %reldir%/axis.m \
  %reldir%/box.m \
  %reldir%/caxis.m \
  %reldir%/clabel.m \
  %reldir%/daspect.m \
  %reldir%/datetick.m \
  %reldir%/diffuse.m \
  %reldir%/grid.m \
  %reldir%/gtext.m \
  %reldir%/hidden.m \
  %reldir%/legend.m \
  %reldir%/lighting.m \
  %reldir%/material.m \
  %reldir%/orient.m \
  %reldir%/pbaspect.m \
  %reldir%/shading.m \
  %reldir%/specular.m \
  %reldir%/text.m \
  %reldir%/title.m \
  %reldir%/view.m \
  %reldir%/whitebg.m \
  %reldir%/xlabel.m \
  %reldir%/xlim.m \
  %reldir%/ylabel.m \
  %reldir%/ylim.m \
  %reldir%/zlabel.m \
  %reldir%/zlim.m

%canon_reldir%dir = $(fcnfiledir)/plot/appearance

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/plot/appearance/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
