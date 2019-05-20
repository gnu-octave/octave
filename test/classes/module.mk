class_Blork_TEST_FILES = \
  %reldir%/@Blork/Blork.m \
  %reldir%/@Blork/bleek.m \
  %reldir%/@Blork/display.m \
  %reldir%/@Blork/get.m \
  %reldir%/@Blork/set.m

class_Cork_TEST_FILES = \
  %reldir%/@Cork/Cork.m \
  %reldir%/@Cork/click.m \
  %reldir%/@Cork/display.m \
  %reldir%/@Cork/get.m \
  %reldir%/@Cork/set.m

class_Dork_TEST_FILES = \
  %reldir%/@Dork/Dork.m \
  %reldir%/@Dork/bling.m \
  %reldir%/@Dork/display.m \
  %reldir%/@Dork/gack.m \
  %reldir%/@Dork/get.m \
  %reldir%/@Dork/getStash.m \
  %reldir%/@Dork/private/myStash.m \
  %reldir%/@Dork/set.m

class_Gork_TEST_FILES = \
  %reldir%/@Gork/Gork.m \
  %reldir%/@Gork/cork.m \
  %reldir%/@Gork/display.m \
  %reldir%/@Gork/gark.m \
  %reldir%/@Gork/get.m \
  %reldir%/@Gork/set.m \
  %reldir%/@Gork/subsasgn.m \
  %reldir%/@Gork/subsref.m

class_Pork_TEST_FILES = \
  %reldir%/@Pork/Pork.m \
  %reldir%/@Pork/bling.m \
  %reldir%/@Pork/display.m \
  %reldir%/@Pork/get.m \
  %reldir%/@Pork/gurk.m \
  %reldir%/@Pork/private/myStash.m \
  %reldir%/@Pork/set.m

class_Sneetch_TEST_FILES = \
  %reldir%/@Sneetch/Sneetch.m \
  %reldir%/@Sneetch/display.m

class_Snork_TEST_FILES = \
  %reldir%/@Snork/Snork.m \
  %reldir%/@Snork/cack.m \
  %reldir%/@Snork/ctranspose.m \
  %reldir%/@Snork/display.m \
  %reldir%/@Snork/double.m \
  %reldir%/@Snork/end.m \
  %reldir%/@Snork/eq.m \
  %reldir%/@Snork/ge.m \
  %reldir%/@Snork/get.m \
  %reldir%/@Snork/getStash.m \
  %reldir%/@Snork/gick.m \
  %reldir%/@Snork/gt.m \
  %reldir%/@Snork/horzcat.m \
  %reldir%/@Snork/ldivide.m \
  %reldir%/@Snork/le.m \
  %reldir%/@Snork/loadobj.m \
  %reldir%/@Snork/lt.m \
  %reldir%/@Snork/minus.m \
  %reldir%/@Snork/mldivide.m \
  %reldir%/@Snork/mpower.m \
  %reldir%/@Snork/mrdivide.m \
  %reldir%/@Snork/mtimes.m \
  %reldir%/@Snork/ne.m \
  %reldir%/@Snork/plus.m \
  %reldir%/@Snork/power.m \
  %reldir%/@Snork/private/myStash.m \
  %reldir%/@Snork/rdivide.m \
  %reldir%/@Snork/saveobj.m \
  %reldir%/@Snork/set.m \
  %reldir%/@Snork/subsasgn.m \
  %reldir%/@Snork/subsindex.m \
  %reldir%/@Snork/subsref.m \
  %reldir%/@Snork/tattack.m \
  %reldir%/@Snork/times.m \
  %reldir%/@Snork/transpose.m \
  %reldir%/@Snork/uminus.m \
  %reldir%/@Snork/uplus.m \
  %reldir%/@Snork/vertcat.m

class_Spork_TEST_FILES = \
  %reldir%/@Spork/Spork.m \
  %reldir%/@Spork/cack.m \
  %reldir%/@Spork/display.m \
  %reldir%/@Spork/geek.m \
  %reldir%/@Spork/get.m \
  %reldir%/@Spork/getStash.m \
  %reldir%/@Spork/loadobj.m \
  %reldir%/@Spork/private/myStash.m \
  %reldir%/@Spork/saveobj.m \
  %reldir%/@Spork/set.m

class_CPrecedenceTester1_TEST_FILES = \
  %reldir%/@CPrecedenceTester1/CPrecedenceTester1.m \
  %reldir%/@CPrecedenceTester1/double.m \
  %reldir%/@CPrecedenceTester1/plus.m \
  %reldir%/@CPrecedenceTester1/tattack.m

class_CPrecedenceTester2_TEST_FILES = \
  %reldir%/@CPrecedenceTester2/CPrecedenceTester2.m \
  %reldir%/@CPrecedenceTester2/double.m \
  %reldir%/@CPrecedenceTester2/plus.m \
  %reldir%/@CPrecedenceTester2/tattack.m

class_CPrecedenceTester3_TEST_FILES = \
  %reldir%/@CPrecedenceTester3/CPrecedenceTester3.m \
  %reldir%/@CPrecedenceTester3/double.m \
  %reldir%/@CPrecedenceTester3/plus.m \
  %reldir%/@CPrecedenceTester3/tattack.m

class_SizeTester_TEST_FILES = \
  %reldir%/@SizeTester/SizeTester.m \
  %reldir%/@SizeTester/numel.m \
  %reldir%/@SizeTester/size.m

classes_TEST_FILES = \
  $(class_Blork_TEST_FILES) \
  $(class_Cork_TEST_FILES) \
  $(class_Dork_TEST_FILES) \
  $(class_Gork_TEST_FILES) \
  $(class_Pork_TEST_FILES) \
  $(class_Sneetch_TEST_FILES) \
  $(class_Snork_TEST_FILES) \
  $(class_Spork_TEST_FILES) \
  $(class_CPrecedenceTester1_TEST_FILES) \
  $(class_CPrecedenceTester2_TEST_FILES) \
  $(class_CPrecedenceTester3_TEST_FILES) \
  $(class_SizeTester_TEST_FILES) \
  %reldir%/classes.tst

TEST_FILES += $(classes_TEST_FILES)
