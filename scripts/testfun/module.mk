FCN_FILE_DIRS += scripts/testfun

scripts_testfun_PRIVATE_FCN_FILES = \
  scripts/testfun/private/compare_plot_demos.m \
  scripts/testfun/private/dump_demos.m \
  scripts/testfun/private/html_compare_plot_demos.m \
  scripts/testfun/private/html_plot_demos_template.html 

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

scripts_testfundir = $(fcnfiledir)/testfun

scripts_testfun_DATA = $(scripts_testfun_FCN_FILES)

scripts_testfun_privatedir = $(fcnfiledir)/testfun/private

scripts_testfun_private_DATA = $(scripts_testfun_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_testfun_FCN_FILES) \
  $(scripts_testfun_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/testfun/PKG_ADD

DIRSTAMP_FILES += scripts/testfun/$(octave_dirstamp)
