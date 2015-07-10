FCN_FILE_DIRS += scripts/testfun

scripts_testfun_FCN_FILES = \
  scripts/testfun/__have_feature__.m \
  scripts/testfun/__printf_assert__.m \
  scripts/testfun/__prog_output_assert__.m \
  scripts/testfun/__run_test_suite__.m \
  scripts/testfun/assert.m \
  scripts/testfun/demo.m \
  scripts/testfun/example.m \
  scripts/testfun/fail.m \
  scripts/testfun/rundemos.m \
  scripts/testfun/runtests.m \
  scripts/testfun/speed.m \
  scripts/testfun/test.m

FCN_FILES += $(scripts_testfun_FCN_FILES)

PKG_ADD_FILES += scripts/testfun/PKG_ADD

DIRSTAMP_FILES += scripts/testfun/$(octave_dirstamp)
