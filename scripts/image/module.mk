FCN_FILE_DIRS += image

image_FCN_FILES = \
  image/autumn.m \
  image/bone.m \
  image/brighten.m \
  image/colormap.m \
  image/contrast.m \
  image/cool.m \
  image/copper.m \
  image/flag.m \
  image/gmap40.m \
  image/gray.m \
  image/gray2ind.m \
  image/hot.m \
  image/hsv.m \
  image/hsv2rgb.m \
  image/image.m \
  image/imagesc.m \
  image/imfinfo.m \
  image/imread.m \
  image/imshow.m \
  image/imwrite.m \
  image/ind2gray.m \
  image/ind2rgb.m \
  image/jet.m \
  image/ntsc2rgb.m \
  image/ocean.m \
  image/pink.m \
  image/prism.m \
  image/rainbow.m \
  image/rgb2hsv.m \
  image/rgb2ind.m \
  image/rgb2ntsc.m \
  image/spring.m \
  image/summer.m \
  image/white.m \
  image/winter.m

IMAGES += \
  image/default.img

FCN_FILES += $(image_FCN_FILES)

PKG_ADD_FILES += image/PKG_ADD

DIRSTAMP_FILES += image/$(octave_dirstamp)
