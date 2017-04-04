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
  test/bug-46330.tst \
  test/bug-49904.tst \
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
include test/bug-41723/module.mk
include test/bug-44940/module.mk
include test/bug-46660/module.mk
include test/bug-50014/module.mk
include test/bug-50035/module.mk
include test/bug-50716/module.mk
include test/class-concat/module.mk
include test/classdef/module.mk
include test/classdef-multiple-inheritance/module.mk
include test/classes/module.mk
include test/ctor-vs-method/module.mk
include test/fcn-handle-derived-resolution/module.mk
include test/nest/module.mk
include test/publish/module.mk

ALL_LOCAL_TARGETS += test/.gdbinit

test/.gdbinit: etc/gdbinit
	@$(gdbinit_install_rule)

define run-octave-tests
  cd test && $(SHELL) ../run-octave $(RUN_OCTAVE_OPTIONS) $(1) --norc --silent --no-history $(abs_top_srcdir)/test/fntests.m $(abs_top_srcdir)/test
  if $(AM_V_P); then \
    echo ""; \
    if [ -f test/fntests.log ]; then \
      echo "Contents of test/fntests.log:"; \
      echo ""; \
      $(AWK) -f $(srcdir)/test/show-failures.awk test/fntests.log; \
    else \
      echo "test/fntests.log is missing!"; \
    fi; \
  fi
endef

check-local: $(GENERATED_TEST_FILES) | $(OCTAVE_INTERPRETER_TARGETS) test/$(octave_dirstamp)
	$(call run-octave-tests)

if AMCOND_HAVE_LLVM
check-jit: $(GENERATED_TEST_FILES) | $(OCTAVE_INTERPRETER_TARGETS) test/$(octave_dirstamp)
	$(call run-octave-tests,--jit-compiler)
endif

COVERAGE_DIR = test/coverage
COVERAGE_INFO = $(COVERAGE_DIR)/$(PACKAGE).info

## FIXME: To get something useful out of 'make coverage', you should use gcc
## and configure with compiler flags set to '-g --coverage'.  Adding the
## --coverage option to either WARN_CXXFLAGS or XTRA_CXXFLAGS resulted in
## link errors, so some work still needed to get a '--enable-coverage-flags'
## option working.

coverage: all
	lcov --directory . --zerocounters
	$(MAKE) $(AM_MAKEFLAGS) check
	$(MKDIR_P) $(COVERAGE_DIR)
	lcov --directory . --capture --output-file $(COVERAGE_INFO)
	genhtml --output-directory $(COVERAGE_DIR) $(COVERAGE_INFO)
	@echo ""
	@echo "Code coverage report successfully built.  Open the file"
	@echo ""
	@echo "   $(abs_top_builddir)/$(COVERAGE_DIR)/index.html"
	@echo ""
	@echo "in a web browser to view the results."
	@echo ""
.PHONY: coverage

test/sparse.tst: test/build-sparse-tests.sh | test/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) $(srcdir)/test/build-sparse-tests.sh > $@-t && \
	mv $@-t $@

test/conv.tst: test/build-conv-tests.sh | test/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) $(srcdir)/test/build-conv-tests.sh > $@-t && \
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
  test/conv.tst \
  test/sparse.tst \
  test/.bc-overload-tests-stamp

fixedtestsdir := $(octtestsdir)/fixed

TEST_INST_FILES = \
  test/conv.tst \
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
  test/build-conv-tests.sh \
  test/build-sparse-tests.sh \
  test/build-bc-overload-tests.sh \
  test/bc-overloads-expected \
  test/build_bc_overloads_expected.m \
  test/show-failures.awk \
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

clean-local: test-clean

test-clean:
	rm -f $(test_CLEANFILES)
	rm -rf $(GENERATED_BC_OVERLOADS_DIRS)
	rm -rf $(COVERAGE_DIR)

test-distclean: test-clean
	rm -f $(test_DISTCLEANFILES)

test-maintainer-clean: test-distclean
	rm -f $(test_MAINTAINERCLEANFILES)
