load_path_TEST_FILES = \
    %reldir%/refresh-load-path.tst \
    %reldir%/in-load-path/load_path_fcn.m \
    %reldir%/namespace-builtin/+namespace_test/quad.m \
    %reldir%/namespace-corelib/+namespace_test/plot.m \
    %reldir%/not-in-load-path/load_path_fcn.m \
    %reldir%/shadowed-builtin/quad.m \
    %reldir%/shadowed-corelib/plot.m

TEST_FILES += $(load_path_TEST_FILES)
