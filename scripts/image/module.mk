FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__imfinfo__.m \
  %reldir%/private/__imread__.m \
  %reldir%/private/__imwrite__.m \
  %reldir%/private/colorspace_conversion_input_check.m \
  %reldir%/private/colorspace_conversion_revert.m \
  %reldir%/private/imageIO.m \
  %reldir%/private/imwrite_filename.m \
  %reldir%/private/ind2x.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/autumn.m \
  %reldir%/bone.m \
  %reldir%/brighten.m \
  %reldir%/cmpermute.m \
  %reldir%/cmunique.m \
  %reldir%/colorcube.m \
  %reldir%/colormap.m \
  %reldir%/contrast.m \
  %reldir%/cool.m \
  %reldir%/copper.m \
  %reldir%/cubehelix.m \
  %reldir%/flag.m \
  %reldir%/frame2im.m \
  %reldir%/getframe.m \
  %reldir%/gray.m \
  %reldir%/gray2ind.m \
  %reldir%/hot.m \
  %reldir%/hsv.m \
  %reldir%/hsv2rgb.m \
  %reldir%/im2double.m \
  %reldir%/im2frame.m \
  %reldir%/image.m \
  %reldir%/imagesc.m \
  %reldir%/imfinfo.m \
  %reldir%/imformats.m \
  %reldir%/imread.m \
  %reldir%/imshow.m \
  %reldir%/imwrite.m \
  %reldir%/ind2gray.m \
  %reldir%/ind2rgb.m \
  %reldir%/iscolormap.m \
  %reldir%/jet.m \
  %reldir%/lines.m \
  %reldir%/movie.m \
  %reldir%/ocean.m \
  %reldir%/pink.m \
  %reldir%/prism.m \
  %reldir%/rainbow.m \
  %reldir%/rgb2gray.m \
  %reldir%/rgb2hsv.m \
  %reldir%/rgb2ind.m \
  %reldir%/rgbplot.m \
  %reldir%/spinmap.m \
  %reldir%/spring.m \
  %reldir%/summer.m \
  %reldir%/turbo.m \
  %reldir%/viridis.m \
  %reldir%/white.m \
  %reldir%/winter.m

SCRIPTS_IMAGES += \
  %reldir%/default.img

%canon_reldir%dir = $(fcnfiledir)/image

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/image/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
