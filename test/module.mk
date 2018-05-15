%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

TEST_FILES += \
  %reldir%/fntests.m \
  %reldir%/args.tst \
  %reldir%/bug-31371.tst \
  %reldir%/bug-38565.tst \
  %reldir%/bug-38576.tst \
  %reldir%/bug-46330.tst \
  %reldir%/bug-49904.tst \
  %reldir%/bug-53579.tst \
  %reldir%/bug-53599.tst \
  %reldir%/colormaps.tst \
  %reldir%/command.tst \
  %reldir%/complex.tst \
  %reldir%/deprecate-props.tst \
  %reldir%/diag-perm.tst \
  %reldir%/error.tst \
  %reldir%/eval-catch.tst \
  %reldir%/for.tst \
  %reldir%/func.tst \
  %reldir%/global.tst \
  %reldir%/if.tst \
  %reldir%/index.tst \
  %reldir%/io.tst \
  %reldir%/jit.tst \
  %reldir%/leftdiv.tst \
  %reldir%/line-continue.tst \
  %reldir%/logical-index.tst \
  %reldir%/null-assign.tst \
  %reldir%/parser.tst \
  %reldir%/prefer.tst \
  %reldir%/range.tst \
  %reldir%/recursion.tst \
  %reldir%/return.tst \
  %reldir%/single-index.tst \
  %reldir%/slice.tst \
  %reldir%/struct.tst \
  %reldir%/switch.tst \
  %reldir%/system.tst \
  %reldir%/transpose.tst \
  %reldir%/try.tst \
  %reldir%/unwind.tst \
  %reldir%/while.tst

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

include %reldir%/bug-35448/module.mk
include %reldir%/bug-35881/module.mk
include %reldir%/bug-36025/module.mk
include %reldir%/bug-38236/module.mk
include %reldir%/bug-38691/module.mk
include %reldir%/bug-41723/module.mk
include %reldir%/bug-44940/module.mk
include %reldir%/bug-46660/module.mk
include %reldir%/bug-49379/module.mk
include %reldir%/bug-50014/module.mk
include %reldir%/bug-50035/module.mk
include %reldir%/bug-50716/module.mk
include %reldir%/bug-51192/module.mk
include %reldir%/bug-51532/module.mk
include %reldir%/bug-51534/module.mk
include %reldir%/bug-51599/module.mk
include %reldir%/bug-52075/module.mk
include %reldir%/bug-52722/module.mk
include %reldir%/bug-53027/module.mk
include %reldir%/class-concat/module.mk
include %reldir%/classdef/module.mk
include %reldir%/classdef-multiple-inheritance/module.mk
include %reldir%/classes/module.mk
include %reldir%/ctor-vs-method/module.mk
include %reldir%/fcn-handle-derived-resolution/module.mk
include %reldir%/local-functions/module.mk
include %reldir%/nest/module.mk
include %reldir%/publish/module.mk
include %reldir%/pkg/module.mk

define run-octave-tests
  ( cd %reldir% && $(SHELL) ../run-octave $(RUN_OCTAVE_OPTIONS) $(1) --norc --silent --no-history $(abs_top_srcdir)/%reldir%/fntests.m $(abs_top_srcdir)/%reldir% ); \
  if $(AM_V_P); then \
    echo ""; \
    if [ -f %reldir%/fntests.log ]; then \
      echo "Contents of %reldir%/fntests.log:"; \
      echo ""; \
      $(AWK) -f $(srcdir)/%reldir%/show-failures.awk %reldir%/fntests.log; \
    else \
      echo "%reldir%/fntests.log is missing!"; \
    fi; \
  fi
endef

check-local: $(GENERATED_TEST_FILES) | $(OCTAVE_INTERPRETER_TARGETS) %reldir%/$(octave_dirstamp)
	$(AM_V_at)$(call run-octave-tests)

if AMCOND_HAVE_LLVM
check-jit: $(GENERATED_TEST_FILES) | $(OCTAVE_INTERPRETER_TARGETS) %reldir%/$(octave_dirstamp)
	$(AM_V_at)$(call run-octave-tests,--jit-compiler)
endif

COVERAGE_DIR = %reldir%/coverage
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

%reldir%/conv.tst: %reldir%/mk-conv-tst.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) $(srcdir)/%reldir%/mk-conv-tst.sh > $@-t && \
	mv $@-t $@

%reldir%/sparse.tst: %reldir%/mk-sparse-tst.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) $(srcdir)/%reldir%/mk-sparse-tst.sh > $@-t && \
	mv $@-t $@

GENERATED_BC_OVERLOADS_DIRS := \
  $(shell $(SHELL) $(srcdir)/%reldir%/mk-bc-overloads-tst.sh test --list-dirs)

GENERATED_BC_OVERLOADS_FILES := \
  $(shell $(SHELL) $(srcdir)/%reldir%/mk-bc-overloads-tst.sh test --list-files)

$(GENERATED_BC_OVERLOADS_FILES): %reldir%/mk-bc-overloads-tst-stamp

%reldir%/.bc-overload-tests-stamp: %reldir%/mk-bc-overloads-tst.sh %reldir%/bc-overloads-expected | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@ && \
	$(SHELL) $(srcdir)/%reldir%/mk-bc-overloads-tst.sh test $(srcdir)/%reldir%/bc-overloads-expected && \
	touch $@

GENERATED_TEST_FILES = \
  %reldir%/conv.tst \
  %reldir%/sparse.tst \
  %reldir%/.bc-overload-tests-stamp

fixedtestsdir := $(octtestsdir)/fixed

TEST_INST_FILES = \
  %reldir%/conv.tst \
  %reldir%/sparse.tst \
  $(GENERATED_BC_OVERLOADS_FILES) \
  $(filter-out %reldir%/fntests.m, $(TEST_FILES))

install-data-local: install-test-files

uninstall-local: uninstall-test-files

install-test-files:
	for f in $(TEST_INST_FILES); do \
	  if test -f "$$f"; then d=; else d="$(srcdir)/"; fi; \
	  base=`echo $$f | $(SED) 's,^%reldir%/,,'`; \
	  $(MKDIR_P) $(DESTDIR)$(fixedtestsdir)/`echo $$base | $(SED) 's,/*[^/]*$$,,'`; \
	  $(INSTALL_DATA) $$d$$f $(DESTDIR)$(fixedtestsdir)/$$base; \
	done
.PHONY: install-test-files

uninstall-test-files:
	for f in $(TEST_INST_FILES); do \
	  base=`echo $$f | $(SED) 's,^%reldir%/,,'`; \
	  rm -f $(DESTDIR)$(fixedtestsdir)/$$base; \
	done
.PHONY: uninstall-test-files

BUILT_SOURCES += $(GENERATED_TEST_FILES)

%canon_reldir%_EXTRA_DIST += \
  %reldir%/bc-overloads-expected \
  %reldir%/mk-bc-overloads-tst.sh \
  %reldir%/mk-conv-tst.sh \
  %reldir%/mk-sparse-tst.sh \
  %reldir%/mk_bc_overloads_expected.m \
  %reldir%/show-failures.awk \
  $(TEST_FILES)

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_CLEANFILES += \
  $(GENERATED_BC_OVERLOADS_FILES) \
  $(GENERATED_TEST_FILES)

%canon_reldir%_DISTCLEANFILES += \
  %reldir%/fntests.log

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

clean-local: test-clean

test-clean:
	rm -f $(%canon_reldir%_CLEANFILES)
	rm -rf $(GENERATED_BC_OVERLOADS_DIRS)
	rm -rf $(COVERAGE_DIR)

test-distclean: test-clean
	rm -f $(%canon_reldir%_DISTCLEANFILES)

test-maintainer-clean: test-distclean
	rm -f $(%canon_reldir%_MAINTAINERCLEANFILES)
