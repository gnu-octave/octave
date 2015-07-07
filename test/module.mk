TEST_FILES += \
  test/fntests.m \
  test/args.tst \
  test/bug-31371.tst \
  test/bug-38576.tst \
  test/colormaps.tst \
  test/complex.tst \
  test/diag-perm.tst \
  test/error.tst \
  test/eval-catch.tst \
  test/for.tst \
  test/func.tst \
  test/global.tst \
  test/if.tst \
  test/index.tst \
  test/io.tst \
  test/jit.tst \
  test/line-continue.tst \
  test/logical-index.tst \
  test/null-assign.tst \
  test/parser.tst \
  test/prefer.tst \
  test/range.tst \
  test/recursion.tst \
  test/return.tst \
  test/slice.tst \
  test/struct.tst \
  test/switch.tst \
  test/system.tst \
  test/transpose.tst \
  test/try.tst \
  test/unwind.tst \
  test/while.tst

DIRSTAMP_FILES += test/$(octave_dirstamp)

## Keep these grouped by directory.

bug_35448_TEST_FILES = \
  test/bug-35448/fA.m \
  test/bug-35448/fB.m \
  test/bug-35448/fC.m \
  test/bug-35448/bug-35448.tst

TEST_FILES += $(bug_35448_TEST_FILES)

bug_36025_TEST_FILES = \
  test/bug-36025/@testclass/one.m \
  test/bug-36025/@testclass/testclass.m \
  test/bug-36025/@testclass/two.m \
  test/bug-36025/bug-36025.tst

TEST_FILES += $(bug_36025_TEST_FILES)

bug_38236_TEST_FILES = \
  test/bug-38236/df_vr.m \
  test/bug-38236/u_vr.m \
  test/bug-38236/bug-38236.tst

TEST_FILES += $(bug_38236_TEST_FILES)

bug_38691_TEST_FILES = \
  test/bug-38691/dir1/func1.m \
  test/bug-38691/dir2/func1.m \
  test/bug-38691/dir2/func2.m \
  test/bug-38691/dir2/func3.m \
  test/bug-38691/bug-38691.tst

TEST_FILES += $(bug_38691_TEST_FILES)

bug_44940_TEST_FILES = \
  test/bug-44940/bug-44940.tst \
  test/bug-44940/class_bug44940.m

TEST_FILES += $(bug_44940_TEST_FILES)

classdef_TEST_FILES = \
  test/classdef/foo_method_changes_property_size.m \
  test/classdef/foo_static_method_constant_property.m \
  test/classdef/foo_value_class.m \
  test/classdef/classdef.tst

TEST_FILES += $(classdef_TEST_FILES)

class_Blork_TEST_FILES = \
  test/classes/@Blork/Blork.m \
  test/classes/@Blork/bleek.m \
  test/classes/@Blork/display.m \
  test/classes/@Blork/get.m \
  test/classes/@Blork/set.m

class_Cork_TEST_FILES = \
  test/classes/@Cork/Cork.m \
  test/classes/@Cork/click.m \
  test/classes/@Cork/display.m \
  test/classes/@Cork/get.m \
  test/classes/@Cork/set.m

class_Dork_TEST_FILES = \
  test/classes/@Dork/Dork.m \
  test/classes/@Dork/bling.m \
  test/classes/@Dork/display.m \
  test/classes/@Dork/gack.m \
  test/classes/@Dork/get.m \
  test/classes/@Dork/getStash.m \
  test/classes/@Dork/private/myStash.m \
  test/classes/@Dork/set.m

class_Gork_TEST_FILES = \
  test/classes/@Gork/Gork.m \
  test/classes/@Gork/cork.m \
  test/classes/@Gork/display.m \
  test/classes/@Gork/gark.m \
  test/classes/@Gork/get.m \
  test/classes/@Gork/set.m \
  test/classes/@Gork/subsasgn.m \
  test/classes/@Gork/subsref.m

class_Pork_TEST_FILES = \
  test/classes/@Pork/Pork.m \
  test/classes/@Pork/bling.m \
  test/classes/@Pork/display.m \
  test/classes/@Pork/get.m \
  test/classes/@Pork/gurk.m \
  test/classes/@Pork/private/myStash.m \
  test/classes/@Pork/set.m

class_Sneetch_TEST_FILES = \
  test/classes/@Sneetch/Sneetch.m \
  test/classes/@Sneetch/display.m

class_Snork_TEST_FILES = \
  test/classes/@Snork/Snork.m \
  test/classes/@Snork/cack.m \
  test/classes/@Snork/display.m \
  test/classes/@Snork/double.m \
  test/classes/@Snork/end.m \
  test/classes/@Snork/eq.m \
  test/classes/@Snork/ge.m \
  test/classes/@Snork/get.m \
  test/classes/@Snork/getStash.m \
  test/classes/@Snork/gick.m \
  test/classes/@Snork/gt.m \
  test/classes/@Snork/horzcat.m \
  test/classes/@Snork/ldivide.m \
  test/classes/@Snork/le.m \
  test/classes/@Snork/loadobj.m \
  test/classes/@Snork/lt.m \
  test/classes/@Snork/minus.m \
  test/classes/@Snork/mldivide.m \
  test/classes/@Snork/mpower.m \
  test/classes/@Snork/mrdivide.m \
  test/classes/@Snork/mtimes.m \
  test/classes/@Snork/ne.m \
  test/classes/@Snork/plus.m \
  test/classes/@Snork/power.m \
  test/classes/@Snork/private/myStash.m \
  test/classes/@Snork/rdivide.m \
  test/classes/@Snork/saveobj.m \
  test/classes/@Snork/set.m \
  test/classes/@Snork/subsasgn.m \
  test/classes/@Snork/subsindex.m \
  test/classes/@Snork/subsref.m \
  test/classes/@Snork/tattack.m \
  test/classes/@Snork/times.m \
  test/classes/@Snork/uminus.m \
  test/classes/@Snork/uplus.m \
  test/classes/@Snork/vertcat.m

class_Spork_TEST_FILES = \
  test/classes/@Spork/Spork.m \
  test/classes/@Spork/cack.m \
  test/classes/@Spork/display.m \
  test/classes/@Spork/geek.m \
  test/classes/@Spork/get.m \
  test/classes/@Spork/getStash.m \
  test/classes/@Spork/loadobj.m \
  test/classes/@Spork/private/myStash.m \
  test/classes/@Spork/saveobj.m \
  test/classes/@Spork/set.m

class_CPrecedenceTester1_TEST_FILES = \
  test/classes/@CPrecedenceTester1/CPrecedenceTester1.m \
  test/classes/@CPrecedenceTester1/tattack.m

class_CPrecedenceTester2_TEST_FILES = \
  test/classes/@CPrecedenceTester2/CPrecedenceTester2.m \
  test/classes/@CPrecedenceTester2/tattack.m

class_CPrecedenceTester3_TEST_FILES = \
  test/classes/@CPrecedenceTester3/CPrecedenceTester3.m \
  test/classes/@CPrecedenceTester3/tattack.m

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
  test/classes/classes.tst

TEST_FILES += $(classes_TEST_FILES)

class_concat_TEST_FILES = \
  test/class-concat/@foo/foo.m \
  test/class-concat/class-concat.tst

TEST_FILES += $(class_concat_TEST_FILES)

ctor_vs_method_TEST_FILES = \
  test/ctor-vs-method/@derived/derived.m \
  test/ctor-vs-method/@derived/parent.m \
  test/ctor-vs-method/@other/other.m \
  test/ctor-vs-method/@other/parent.m \
  test/ctor-vs-method/@parent/method.m \
  test/ctor-vs-method/@parent/parent.m \
  test/ctor-vs-method/__trace__.m \
  test/ctor-vs-method/ctor-vs-method.tst

TEST_FILES += $(ctor_vs_method_TEST_FILES)

fcn_handle_derived_resolution_TEST_FILES = \
  test/fcn-handle-derived-resolution/@fhdr_derived/fhdr_derived.m \
  test/fcn-handle-derived-resolution/@fhdr_other/getsize_arrayfun.m \
  test/fcn-handle-derived-resolution/@fhdr_other/getsize_cellfun.m \
  test/fcn-handle-derived-resolution/@fhdr_other/getsize_loop.m \
  test/fcn-handle-derived-resolution/@fhdr_other/fhdr_other.m \
  test/fcn-handle-derived-resolution/@fhdr_parent/numel.m \
  test/fcn-handle-derived-resolution/@fhdr_parent/fhdr_parent.m \
  test/fcn-handle-derived-resolution/fcn-handle-derived-resolution.tst

TEST_FILES += $(fcn_handle_derived_resolution_TEST_FILES)

nest_fcn_files = \
  test/nest/arg_nest.m \
  test/nest/arg_ret.m \
  test/nest/nest_eval.m \
  test/nest/no_closure.m \
  test/nest/persistent_nest.m \
  test/nest/recursive_nest.m \
  test/nest/recursive_nest2.m \
  test/nest/recursive_nest3.m \
  test/nest/scope0.m \
  test/nest/scope1.m \
  test/nest/scope2.m \
  test/nest/scope3.m \
  test/nest/script_nest.m \
  test/nest/script_nest_script.m \
  test/nest/nest.tst \
  test/nest/varg_nest.m \
  test/nest/varg_nest2.m

TEST_FILES += $(nest_TEST_FILES)

all-local: test/.gdbinit

test/.gdbinit: etc/gdbinit
	@$(gdbinit_install_rule)

check-local: 
	cd test && ../run-octave $(RUN_OCTAVE_OPTIONS) --norc --silent --no-history $(srcdir)/test/fntests.m $(srcdir)/test

if AMCOND_HAVE_LLVM
check-jit: test/sparse.tst test/bc-overload-tests.stamp test/$(octave_dirstamp)
	cd test && ../run-octave $(RUN_OCTAVE_OPTIONS) --jit-compiler --norc --silent --no-history $(srcdir)/test/fntests.m $(srcdir)/test
endif

test/sparse.tst: test/build-sparse-tests.sh test/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/test/build-sparse-tests.sh > $@-t && \
	mv $@-t $@

GENERATED_BC_OVERLOADS_DIRS := \
  $(shell $(srcdir)/test/build-bc-overload-tests.sh test --list-dirs)

GENERATED_BC_OVERLOADS_FILES := \
  $(shell $(srcdir)/test/build-bc-overload-tests.sh test --list-files)

$(GENERATED_BC_OVERLOADS_FILES): test/bc-overload-tests.stamp

test/bc-overload-tests.stamp: test/build-bc-overload-tests.sh test/bc-overloads-expected
	$(AM_V_GEN)rm -f $@ && \
	$(srcdir)/test/build-bc-overload-tests.sh test $(srcdir)/test/bc-overloads-expected && \
	touch $@

BUILT_SOURCES += \
  test/sparse.tst \
  test/bc-overload-tests.stamp

EXTRA_DIST += \
  test/build-sparse-tests.sh \
  test/build-bc-overload-tests.sh \
  test/bc-overloads-expected \
  test/build_bc_overloads_expected.m \
  $(TEST_FILES)

CLEANFILES += \
  $(GENERATED_BC_OVERLOADS_FILES) \
  test/sparse.tst \
  test/bc-overload-tests.stamp

DISTCLEANFILES += \
  test/.gdbinit \
  test/fntests.log

fixedtestsdir := $(octtestsdir)/fixed

nobase_fixedtests_DATA = \
  test/sparse.tst \
  $(GENERATED_BC_OVERLOADS_FILES) \
  $(filter-out test/fntests.m, $(TEST_FILES))
