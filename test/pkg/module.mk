pkg_TEST_FILES = \
    %reldir%/pkg.tst \
    %reldir%/mfile-basic-test/INDEX \
    %reldir%/mfile-basic-test/NEWS \
    %reldir%/mfile-basic-test/DESCRIPTION \
    %reldir%/mfile-basic-test/doc/macros.texi \
    %reldir%/mfile-basic-test/doc/example-package.txi \
    %reldir%/mfile-basic-test/COPYING \
    %reldir%/mfile-basic-test/inst/example_mfile.m \
    %reldir%/mfile-minimal-test/DESCRIPTION \
    %reldir%/mfile-minimal-test/COPYING \
    %reldir%/mfile-minimal-test/inst/example_mfile.m

TEST_FILES += $(pkg_TEST_FILES)
