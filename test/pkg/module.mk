pkg_TEST_FILES = \
    %reldir%/pkg.tst \
    %reldir%/mfile_basic_test/INDEX \
    %reldir%/mfile_basic_test/NEWS \
    %reldir%/mfile_basic_test/DESCRIPTION \
    %reldir%/mfile_basic_test/doc/macros.texi \
    %reldir%/mfile_basic_test/doc/example-package.txi \
    %reldir%/mfile_basic_test/COPYING \
    %reldir%/mfile_basic_test/inst/example_mfile.m \
    %reldir%/mfile_minimal_test/DESCRIPTION \
    %reldir%/mfile_minimal_test/COPYING \
    %reldir%/mfile_minimal_test/inst/example_mfile.m

TEST_FILES += $(pkg_TEST_FILES)
