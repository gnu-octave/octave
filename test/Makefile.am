# Makefile for test directory

## This directory uses it's own Makefile.am, and the top-level Makefile employs
## a recursive build strategy ("make xxx" at top-level leads to changing into
## subdirectory test/ and executing "make xxx").
## This is required because the Octave project ran into internal limits on the
## number of string objects (e.g., filenames) that could be used in one shell
## command line.  An independent Makefile has its own namespace and this
## strategy reduces the number of strings that are present in the top-level
## Makefile namespace.

EXTRA_DIST =
CLEANFILES =
DISTCLEANFILES =
MAINTAINERCLEANFILES =
# Octave-created automake primitive for test files
TEST_FILES =
noinst_TEST_FILES =

# Stamp directories which have built sources to enable VPATH builds
octave_dirstamp := $(am__leading_dot)octave-dirstamp

DIRSTAMP_FILES = $(octave_dirstamp)

$(DIRSTAMP_FILES):
	$(AM_V_GEN)$(MKDIR_P) $(@D) && \
	: > $@

TEST_FILES += \
  anonymous-function.tst \
  args.tst \
  bug-31371.tst \
  bug-40117.tst \
  bug-45969.tst \
  bug-45972.tst \
  bug-46330.tst \
  bug-49904.tst \
  bug-53579.tst \
  bug-53599.tst \
  bug-54490.tst \
  bug-55308.tst \
  bug-55321.tst \
  bug-55322.tst \
  bug-59950.tst \
  bug-61201.tst \
  bug-65153.tst \
  colormaps.tst \
  command.tst \
  complex.tst \
  deprecate-props.tst \
  diag-perm.tst \
  error.tst \
  eval-catch.tst \
  eval-command.tst \
  for.tst \
  func.tst \
  global.tst \
  if.tst \
  index.tst \
  inline-fcn.tst \
  integer.tst \
  io.tst \
  leftdiv.tst \
  line-continue.tst \
  logical-index.tst \
  null-assign.tst \
  parser.tst \
  prefer.tst \
  range.tst \
  recursion.tst \
  return.tst \
  single-index.tst \
  slice.tst \
  sparse-assign.tst \
  sparse-broadcasting.tst \
  struct.tst \
  switch.tst \
  system.tst \
  transpose.tst \
  try.tst \
  unwind.tst \
  while.tst

noinst_TEST_FILES += \
  test_suite.m

include bug-35448/module.mk
include bug-35881/module.mk
include bug-36025/module.mk
include bug-38236/module.mk
include bug-38691/module.mk
include bug-41723/module.mk
include bug-44643/module.mk
include bug-44940/module.mk
include bug-45351/module.mk
include bug-46660/module.mk
include bug-47680/module.mk
include bug-48693/module.mk
include bug-49379/module.mk
include bug-50014/module.mk
include bug-50035/module.mk
include bug-50395/module.mk
include bug-50716/module.mk
include bug-50831/module.mk
include bug-51192/module.mk
include bug-51532/module.mk
include bug-51534/module.mk
include bug-51599/module.mk
include bug-52075/module.mk
include bug-52722/module.mk
include bug-52851/module.mk
include bug-53027/module.mk
include bug-53468/module.mk
include bug-53956/module.mk
include bug-54995/module.mk
include bug-55758/module.mk
include bug-56068/module.mk
include bug-58572/module.mk
include bug-58593/module.mk
include bug-59451/module.mk
include bug-59617/module.mk
include bug-59661/module.mk
include bug-59704/module.mk
include bug-59937/module.mk
include bug-60237/module.mk
include bug-60726/module.mk
include bug-60882/module.mk
include bug-61105/module.mk
include bug-61191/module.mk
include bug-63841/module.mk
include bug-64620/module.mk
include bug-65037/module.mk
include bug-67096/module.mk
include bug-67403/module.mk
include bug-67964/module.mk
include class-concat/module.mk
include classdef-breakpoints/module.mk
include classdef-load-save/module.mk
include classdef-multiple-inheritance/module.mk
include classdef/module.mk
include classes/module.mk
include colon-op/module.mk
include ctor-vs-method/module.mk
include fcn-handle/module.mk
include file-encoding/module.mk
include help/module.mk
include json/module.mk
include jupyter-notebook/module.mk
include load-path/module.mk
include local-functions/module.mk
include mex/module.mk
include nest/module.mk
include pkg/module.mk
include private-functions/module.mk
include publish/module.mk

BUILT_TEST_FILES = \
  conv.tst \
  sparse.tst \
  $(BUILT_BC_OVERLOADS_FILES)

## Special rules:

conv.tst: mk-conv-tst.sh | $(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) $(srcdir)/mk-conv-tst.sh > $@-t && \
	mv $@-t $@

sparse.tst: mk-sparse-tst.sh | $(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) $(srcdir)/mk-sparse-tst.sh > $@-t && \
	mv $@-t $@

BUILT_BC_OVERLOADS_DIRS := \
  $(shell $(SHELL) $(srcdir)/mk-bc-overloads-tst.sh $(abs_top_builddir)/test --list-dirs)

BUILT_BC_OVERLOADS_FILES := \
  $(shell $(SHELL) $(srcdir)/mk-bc-overloads-tst.sh $(abs_top_builddir)/test --list-files)

# Special syntax '&:' informs Make that all targets will be built by running
# the associated rule once.
$(BUILT_BC_OVERLOADS_FILES) &: mk-bc-overloads-tst.sh bc-overloads-expected | $(octave_dirstamp)
	$(AM_V_GEN)rm -f $@ && \
	$(SHELL) $(srcdir)/mk-bc-overloads-tst.sh $(abs_top_builddir)/test $(srcdir)/bc-overloads-expected

check-local: $(BUILT_TEST_FILES) $(MEX_TEST_FUNCTIONS) | $(octave_dirstamp)
	$(AM_V_at) ( $(SHELL) $(abs_top_builddir)/run-octave $(RUN_OCTAVE_OPTIONS) \
	  --no-init-file --quiet --no-history --path $(abs_top_builddir)/test/mex \
	  $(abs_top_srcdir)/test/test_suite.m $(abs_top_srcdir)/test "$(1)" ) && \
	if $(AM_V_P); then \
	  echo ""; \
	  if [ -f test_suite.log ]; then \
	    echo "Contents of test/test_suite.log:"; \
	    echo ""; \
	    $(AWK) -f $(srcdir)/show-failures.awk test_suite.log; \
	  else \
	    echo "test_suite.log is missing!"; \
	  fi; \
	fi

## FIXME: Code coverage target does not work at this time (2026-05-18).
## FIXME: To get something useful out of 'make coverage', you should use g++
## and configure with compiler flags set include to '-g --coverage'.  Per g++
## documentation, option '--coverage' should also be added to link flags.
## Option '-fprofile-abs-path' may be necessary for projects with different
## working directories.  Coverage data will match the source files more closely
## if optimization is not used.
## Adding the --coverage option to either WARN_CXXFLAGS or XTRA_CXXFLAGS
## resulted in link errors, so work is still needed to get an
## '--enable-coverage-flags' option working.

COVERAGE_DIR = coverage
COVERAGE_INFO = $(COVERAGE_DIR)/$(PACKAGE).info

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

## Utility rule to aid development

ALL_TEST_FILES := $(addprefix $(srcdir)/, $(TEST_FILES))

## Tag bug IDs in tests as fixed
update-bug-status:
	$(SHELL) $(srcdir)/%reldir%/update-bug-status.sh $(ALL_TEST_FILES)
.PHONY: update-bug-status

## Installation rules

fixedtestsdir := $(octtestsdir)/fixed

inst_TEST_FILES := \
  $(BUILT_TEST_FILES) \
  $(TEST_FILES)

install-data-local: install-test-files

uninstall-local: uninstall-test-files

install-test-files: $(inst_TEST_FILES)
	for f in $(inst_TEST_FILES); do \
	  if test -f "$$f"; then d=; else d="$(srcdir)/"; fi; \
	  $(MKDIR_P) $(DESTDIR)$(fixedtestsdir)/`echo $$f | $(SED) 's,/*[^/]*$$,,'`; \
	  $(INSTALL_DATA) $$d$$f $(DESTDIR)$(fixedtestsdir)/$$f; \
	done
.PHONY: install-test-files

uninstall-test-files:
	for f in $(inst_TEST_FILES); do \
	  rm -f $(DESTDIR)$(fixedtestsdir)/$$f; \
	done
.PHONY: uninstall-test-files

dist_TEST_FILES := \
  $(addprefix $(srcdir)/, $(TEST_FILES)) \
  $(addprefix $(srcdir)/, $(noinst_TEST_FILES))

EXTRA_DIST += \
  bc-overloads-expected \
  mk-bc-overloads-tst.sh \
  mk-conv-tst.sh \
  mk-sparse-tst.sh \
  mk_bc_overloads_expected.m \
  show-failures.awk \
	$(dist_TEST_FILES)

CLEANFILES += \
  test_suite.log \
  $(BUILT_TEST_FILES)

DISTCLEANFILES += \
  $(DIRSTAMP_FILES)

clean-local:
	rm -rf $(BUILT_BC_OVERLOADS_DIRS)
	rm -rf $(COVERAGE_DIR)
