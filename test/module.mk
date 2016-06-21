test_EXTRA_DIST =

test_CLEANFILES =
test_DISTCLEANFILES =
test_MAINTAINERCLEANFILES =

TEST_FILES += \
  test/fntests.m \
  test/args.tst \
  test/bug-31371.tst \
  test/bug-38565.tst \
  test/bug-38576.tst \
  test/colormaps.tst \
  test/command.tst \
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

include test/bug-35448/module.mk
include test/bug-36025/module.mk
include test/bug-38236/module.mk
include test/bug-38691/module.mk
include test/bug-44940/module.mk
include test/class-concat/module.mk
include test/classdef/module.mk
include test/classes/module.mk
include test/ctor-vs-method/module.mk
include test/fcn-handle-derived-resolution/module.mk
include test/nest/module.mk

ALL_LOCAL_TARGETS += test/.gdbinit

test/.gdbinit: etc/gdbinit
	@$(gdbinit_install_rule)

check-local: $(GENERATED_TEST_FILES) | $(OCTAVE_INTERPRETER_TARGETS) test/$(octave_dirstamp)
	cd test && $(SHELL) ../run-octave $(RUN_OCTAVE_OPTIONS) --norc --silent --no-history $(abs_top_srcdir)/test/fntests.m $(abs_top_srcdir)/test

if AMCOND_HAVE_LLVM
check-jit: $(GENERATED_TEST_FILES) | $(OCTAVE_INTERPRETER_TARGETS) test/$(octave_dirstamp)
	cd test && $(SHELL) ../run-octave $(RUN_OCTAVE_OPTIONS) --jit-compiler --norc --silent --no-history $(abs_top_srcdir)/test/fntests.m $(abs_top_srcdir)/test
endif

test/sparse.tst: test/build-sparse-tests.sh | test/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) $(srcdir)/test/build-sparse-tests.sh > $@-t && \
	mv $@-t $@

GENERATED_BC_OVERLOADS_DIRS := \
  $(shell $(SHELL) $(srcdir)/test/build-bc-overload-tests.sh test --list-dirs)

GENERATED_BC_OVERLOADS_FILES := \
  $(shell $(SHELL) $(srcdir)/test/build-bc-overload-tests.sh test --list-files)

$(GENERATED_BC_OVERLOADS_FILES): test/.bc-overload-tests-stamp

test/.bc-overload-tests-stamp: test/build-bc-overload-tests.sh test/bc-overloads-expected | test/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@ && \
	$(SHELL) $(srcdir)/test/build-bc-overload-tests.sh test $(srcdir)/test/bc-overloads-expected && \
	touch $@

GENERATED_TEST_FILES = \
  test/sparse.tst \
  test/.bc-overload-tests-stamp

fixedtestsdir := $(octtestsdir)/fixed

TEST_INST_FILES = \
  test/sparse.tst \
  $(GENERATED_BC_OVERLOADS_FILES) \
  $(filter-out test/fntests.m, $(TEST_FILES))

install-data-local: install-test-files

uninstall-local: uninstall-test-files

install-test-files:
	for f in $(TEST_INST_FILES); do \
	  if test -f "$$f"; then d=; else d="$(srcdir)/"; fi; \
	  base=`echo $$f | $(SED) 's,^test/,,'`; \
	  $(MKDIR_P) $(DESTDIR)$(fixedtestsdir)/`echo $$base | $(SED) 's,/*[^/]*$$,,'`; \
	  $(INSTALL_DATA) $$d$$f $(DESTDIR)$(fixedtestsdir)/$$base; \
	done
.PHONY: install-test-files

uninstall-test-files:
	for f in $(TEST_INST_FILES); do \
	  base=`echo $$f | $(SED) 's,^test/,,'`; \
	  rm -f $(DESTDIR)$(fixedtestsdir)/$$base; \
	done
.PHONY: uninstall-test-files

BUILT_SOURCES += $(GENERATED_TEST_FILES)

test_EXTRA_DIST += \
  test/build-sparse-tests.sh \
  test/build-bc-overload-tests.sh \
  test/bc-overloads-expected \
  test/build_bc_overloads_expected.m \
  $(TEST_FILES)

EXTRA_DIST += $(test_EXTRA_DIST)

test_CLEANFILES += \
  $(GENERATED_BC_OVERLOADS_FILES) \
  $(GENERATED_TEST_FILES)

test_DISTCLEANFILES += \
  test/.gdbinit \
  test/fntests.log

CLEANFILES += $(test_CLEANFILES)
DISTCLEANFILES += $(test_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(test_MAINTAINERCLEANFILES)

test-clean:
	rm -f $(test_CLEANFILES)

test-distclean: test-clean
	rm -f $(test_DISTCLEANFILES)

test-maintainer-clean: test-distclean
	rm -f $(test_MAINTAINERCLEANFILES)
