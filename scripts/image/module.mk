FCN_FILE_DIRS += \
  scripts/image \
  scripts/image/private

scripts_image_PRIVATE_FCN_FILES = \
  scripts/image/private/__imfinfo__.m \
  scripts/image/private/__imread__.m \
  scripts/image/private/__imwrite__.m \
  scripts/image/private/colorspace_conversion_input_check.m \
  scripts/image/private/colorspace_conversion_revert.m \
  scripts/image/private/imageIO.m \
  scripts/image/private/imwrite_filename.m \
  scripts/image/private/ind2x.m

scripts_image_FCN_FILES = \
  scripts/image/autumn.m \
  scripts/image/bone.m \
  scripts/image/brighten.m \
  scripts/image/cmpermute.m \
  scripts/image/cmunique.m \
  scripts/image/colorcube.m \
  scripts/image/colormap.m \
  scripts/image/contrast.m \
  scripts/image/cool.m \
  scripts/image/copper.m \
  scripts/image/cubehelix.m \
  scripts/image/flag.m \
  scripts/image/gray.m \
  scripts/image/gray2ind.m \
  scripts/image/hot.m \
  scripts/image/hsv.m \
  scripts/image/hsv2rgb.m \
  scripts/image/iscolormap.m \
  scripts/image/im2double.m \
  scripts/image/image.m \
  scripts/image/imagesc.m \
  scripts/image/imfinfo.m \
  scripts/image/imformats.m \
  scripts/image/imread.m \
  scripts/image/imshow.m \
  scripts/image/imwrite.m \
  scripts/image/ind2gray.m \
  scripts/image/ind2rgb.m \
  scripts/image/jet.m \
  scripts/image/lines.m \
  scripts/image/ntsc2rgb.m \
  scripts/image/ocean.m \
  scripts/image/pink.m \
  scripts/image/prism.m \
  scripts/image/rainbow.m \
  scripts/image/rgb2hsv.m \
  scripts/image/rgb2ind.m \
  scripts/image/rgb2ntsc.m \
  scripts/image/rgbplot.m \
  scripts/image/spinmap.m \
  scripts/image/spring.m \
  scripts/image/summer.m \
  scripts/image/viridis.m \
  scripts/image/white.m \
  scripts/image/winter.m

SCRIPTS_IMAGES += \
  scripts/image/default.img

scripts_imagedir = $(fcnfiledir)/image

scripts_image_DATA = $(scripts_image_FCN_FILES)

scripts_image_privatedir = $(fcnfiledir)/image/private

scripts_image_private_DATA = $(scripts_image_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_image_FCN_FILES) \
  $(scripts_image_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/image/PKG_ADD

DIRSTAMP_FILES += scripts/image/$(octave_dirstamp)
