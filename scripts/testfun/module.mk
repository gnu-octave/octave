FCN_FILE_DIRS += testfun

testfun_FCN_FILES = \
  testfun/assert.m \
  testfun/demo.m \
  testfun/example.m \
  testfun/fail.m \
  testfun/rundemos.m \
  testfun/speed.m \
  testfun/test.m

FCN_FILES += $(testfun_FCN_FILES)

PKG_ADD_FILES += testfun/PKG_ADD

DIRSTAMP_FILES += testfun/$(octave_dirstamp)
