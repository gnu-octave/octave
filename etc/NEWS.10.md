Summary of important user-visible changes for version 10 (yyyy-mm-dd):
---------------------------------------------------------------------

### General improvements

- Three short form aliases have been added for long form options when starting
`octave`.
  * `-e CODE` for `--eval CODE`
  * `-g` for `--gui`
  * `-G` for `--no-gui`

- Three long form options have been introduced for clarity.
  * `--no-init-user` : Don't read user configuration files `~/.octaverc` or
    `.octaverc` files at startup.
  * `--no-init-site` : Don't read site-wide configuration files at startup.
  * `--no-init-all` : Don't read any configuration files at startup.

- `nchoosek` algorithm is now ~2x faster and provides greater precision.

- `nextpow2` algorithm is now more accurate for inputs very close to a power
  of 2.  The output class now matches the input class for Matlab compatibility.
  The function no longer accepts complex inputs and emits an error for these
  inputs.

- `jsonencode` now outputs integers and floating point integers without an
  unnecessary ".0" suffix.

- `hist` now accepts N-dimensional array inputs for input `Y`, which is
  processed in columns as if the array was flattened to a 2-dimensional array.

- The third output for `unique` is now correct when the `stable` sort option is
  used.

- Support setting breakpoints in `set` or `get` methods of `classdef`
  properties (bug #65610).

- `.mex` files now link to the new library `liboctmex` (instead of to
  `liboctinterp` and `liboctave`).  The `SOVERSION` of this new library is
  expected to be stable across multiple major versions of Octave.  The benefit
  is that `.mex` files will not necessarily require rebuilding for every
  major version release of Octave.

- `pkg describe` command now supports a new `Tracker` tag in the `DESCRIPTION`
  file and returns package-provided repository and bug tracker URLs.  Package
  maintainers are encouraged to utilize the new feature to redirect users to
  package-specific bug trackers instead of Savannah.

- `pkg install` now mentions package-provided URL and package-provided bug
  tracker if they exist.

### Graphical User Interface

### Graphics backend

- `polar` plots now include the center tick mark value, typically 0, in the
  `'rtick'` parameter when the plot is created.  Subsequent modifications to
  `'rtick'` by the function `rticks` will only include the center tick mark
  value if it is specified.

- `view` correctly interprets Cartesian viewpoints on main axes (bug #65641).

- `plot3` now draws a single marker if only one data point is given.
  Previously the plot was blank (`marker` = `"none"`) which was confusing.

### Matlab compatibility

- `height` and `width` are now aliases for the `rows` and `columns` functions.

- All colormaps now default to a size of 256 colors. (The previous default size
  was 64.)

- The first argument to `colormap` may now be a figure or axes object.  Calling
  `colormap` on a figure object will clear any `"colormap"` properties set at
  the axes level.

- `griddata` output size more consistently matches the input interpolation
  points when the inputs are vectors.  If the inputs are vectors with the
  same-orientation, then the outputs will be the same size as those vectors.
  When either input is a row vector and the other is a column vector, the
  interpolating points are processed through `meshgrid` and the output is a
  matrix the same size as the meshgrid.

- `iqr` now provides compatible output for empty inputs.

- `cross` now produces row vector outputs when the inputs are a mix of row and
  column vectors (bug #61295).

- `rat` now accepts complex inputs.

- The optional second input argument of `system`, denoting whether to return
  the output as a variable, is now required to be a boolean value if used.

- Octave functions whose Matlab equivalents give errors when passed non-integer
  values as sizes or dimensions now also give similar errors.  For example,
  `cell (e, pi)` now gives an error in Octave about requiring integer sizes for
  the cell array, matching Matlab behavior.  Previously, Octave's conversion
  from non-integers to integers was more lenient.

- `issorted` now accepts the `MODE` option `"monotonic"`, which has the same
  behavior as the option `"either"`.

- `movfun` and `movslice`: Functions now accept `wlen` equal to 1 or [0,0],
  non-integer values of `wlen`, and values of `wlen` that create window lengths
  exceeding the size of the target array.  `movfun` also accepts values of
  `dim` larger than the number of non-singleton dimensions in the target array.
  The `SamplePoints` option has been implemented for both functions.
  Non-numeric input array handling has been improved.  These changes affect all
  moving window functions (`movmad`, `movmax`, `movmean`, `movmedian`,
  `movmin`, `movprod`, `movstd`, `movsum`, and `movvar`) (bug #65928,
  bug #66025).

- `movfun`: The `nancond` property has been fully implemented and made
  Matlab-compatible.  The `omitnan` option will ignore `NaN` and `NA` values
  when calculating the function return, and, if all elements in a window slice
  are `NaN` or `NA`, it will return the value contained in a new property
  `nanval` (default `NaN`) for that element.  The `includenan` property (the
  default) has been updated for compatibility such that any window containing
  `NaN` or `NA` will return `NaN` rather than passing those values to the
  calculating function.  `omitmissing` and `includemissing` are now accepted as
  aliases for `omitnan` and `includenan`.  These changes affect all moving
  window functions (`movmad`, `movmax`, `movmean`, `movmedian`, `movmin`,
  `movprod`, `movstd`, `movsum`, and `movvar`) (bug #66156).

- `movmin` and `movmax`: These functions now have their default behavior set to
  `omitnan`.  `NaN` and `NA` values will be ignored unless a moving window
  contains only `NaN` or `NA` values, in which case the function will return
  `NaN` for that element (bug #66156).

- `movsum`: When called with option `omitnan`, any windows containing only
  `NaN` and `NA` values will return 0 (bug #66156).

- `movprod`: When called with option `omitnan`, any windows containing only
  `NaN` and `NA` values will return 1 (bug #66156).

- `movmad`: The function now defaults to calculating median absolute deviation.
  Before Octave 10, the function calculated mean absolute deviation.  A new
  `mode` property has been provided that takes values of either `"mean"` or
  `"median"` to allow the user to select which option to use.  This property
  should not be expected to function in code used outside of Octave.

- `symbfact`: outputs `count`, `parent`, and `post` are now row vectors rather
  than column vectors.

### Alphabetical list of new functions added in Octave 10

* `clim`
* `rticklabels`
* `thetaticklabels`

### Deprecated functions, properties, and operators

The following functions and properties have been deprecated in Octave 10
and will be removed from Octave 12 (or whatever version is the second
major release after 10):

- Functions

        Function    | Replacement
        ------------|------------
        dsearch     | dsearchn

- Core

        Function                                     | Replacement
        ---------------------------------------------|-----------------------------------
        symbol_record_rep::mark_as_variable          | symbol_record_rep::mark_variable
        symbol_record_rep::unmark_as_variable        | symbol_record_rep::unmark_variable
        symbol_record::mark_as_variable              | symbol_record::mark_variable
        symbol_record::unmark_as_variable            | symbol_record::unmark_variable
        interpreter::verbose                         | interpreter::init_trace
        cmdline_options::verbose                     | cmdline_options::init_trace
        interpreter::read_init_files                 | interpreter::read_user_files
        cmdline_options::read_init_files             | cmdline_options::read_user_files
        __lo_ieee_isnan,    __lo_ieee_float_isnan    | std::isnan     or  isnan
        __lo_ieee_isfinite, __lo_ieee_float_isfinite | std::isfinite  or  isfinite
        __lo_ieee_isinf,    __lo_ieee_float_isinf    | std::isinf     or  isinf
        __lo_ieee_signbit,  __lo_ieee_float_signbit  | std::signbit   or  signbit

  - A new method `rwdata ()` provides direct read/write access (a pointer) to
    the data in a liboctave `Array` object (or its derived classes such as
    `Matrix`).  Historically, the method `fortran_vec ()` provided this access,
    but the name is unclear, and it is recommended to use `rwdata ()` in any
    new code.  There are no plans to deprecate `fortran_vec`.

  - The `--verbose`,`-V` command-line option has been deprecated.  Use
    `--init-trace` to print the configuration files executed during
    initialization.

The following features were deprecated in Octave 8 and have been removed
from Octave 10.

- Functions

        Function             | Replacement
        ---------------------|---------------------
        shift                | circshift
        sparse_auto_mutate   | none (see NEWS.8.md)

- `fminsearch` parameter passing: A legacy, undocumented, and only partially
  supported syntax for passing parameters to the minimized function `fcn`
  called by `fminsearch` by appending them to the input argument list was
  partly implemented since Octave 4.4.0.  Due to conflicts with other input
  methods, the documentation of this syntax was removed in Octave 5.1.0.  The
  remaining functionality has been completely removed in Octave 10.  Attempts
  to call `fminsearch` with that syntax will result in an error.  The preferred
  method of passing parameters to any of the minimization functions (including
  `fminsearch`, `fminbnd`, and `fminunc`) is through the use of anonymous
  functions.  Specific examples of this can be found in the "Minimizers"
  section of the Octave manual.

- The unsupported and unnecessary `load` options of `-force` and `-import` have
  been removed.  If legacy m-files are still invoking `load` with these options
  update the code and remove them.

### Build system

- Octave now requires a C++ compiler that is compliant with C++17 (preferably
  with GNU extensions).

- The location of the list of packages installed site-wide for all users
  (`pkg global_list`) has changed (bug #66831).  The file `octave_packages` is
  now installed at `octave-config -p LOCALAPIPKGDIR`.  There will be no
  disruption if you are upgrading to version 10.1 of Octave and then installing
  packages.  If you want to use previously installed packages that you *know*
  will run without re-compilation (because the package uses m-files only and no
  oct-files or mex-files), then you can execute the command
  `pkg global_list CURRENT_GLOBAL_LIST_FILE` (e.g., from within one of your
  startup files) to point Octave to the existing list.  Alternatively, run
  `pkg rebuild -global` once (potentially requires superuser rights), or copy
  `CURRENT_GLOBAL_LIST_FILE` to the path and filename returned by
  `pkg global_list`.


Summary of bugs fixed for version 10.1.0 (yyyy-mm-dd):
----------------------------------------------------

- bug #66753: `hist.m`: Test for equal bin spacing using a numeric tolerance.
- bug #66753: `hist.m`: Fix regression in determining equal bin spacing.
- bug #66753: Fix thinko in cset be1d0c816788
- bug #66647: Silence unexpected `Octave:mixed-string-concat` warning in
  `num2str.m`.
- bug #66642: Code beautification for cset 2c2301104caf
- bug #66642: Check for undefined outputs in `cellfun` and `arrayfun`.
- bug #66642: Add `cellfun` BIST for function failing to return requested
  output.
- bug #66642: Add `arrayfun` BIST for function failing to return requested
  output.
- bug #66617: Avoid error for `struct2cell` with function without output
  arguments.
- bug #66617: Add self-tests for `structfun` with function without output
  arguments.
- bug #66558: Translate shortcuts and descriptions in settings dialog.
- bug #66521: `symbfact`: Return row vectors for Matlab compatibility.
- bug #66511: `sparseqr`: Support permutation output with CXSparse.
- bug #66511: Emit error when `qr()` is called with permutation output and
  CXSparse library is used.
- bug #66488: `qr (sparse, 0)` isn't equal to `qr (sparse, 'econ', 'vector')`.
- bug #66477: Avoid ASAN error with `path (_)` manipulations.
- bug #66466: `legend` and `bar` cause ASAN to crash Octave.
- bug #66451: Ctrl-C kills Octave 10.0.0.
- bug #66448: File encoding conversion errors on macOS 14.
- bug #66399: `fseek` and `ftell` functions don't always work correctly on
  Windows with filesizes >2GB.
- bug #66315: `menu()` throws an error when Qt dialogs are not available.
- bug #66256: `movmad` uses 'mean absolute deviation' while Matlab uses 'median
  absolute deviation'.
- bug #66156: Implement `nanflag` option for moving window functions.
- bug #66025: `movfun`: Implement `SamplePoints` option.
- bug #66010: Function `glpk` produces incorrect output.
- bug #65964: `nthargout` does not propagate error ID.
- bug #65928: `movfun` dimension constraints: Shouldn't error for
  `dim > ndims(x)`, or `wlen > size(x, dim)`, or `wlen=1`.
- bug #65876: Error retrieving data from `struct` values in `containers.Map`.
- bug #65768: Segmentation fault on default branch.
- bug #65753: Strip leading/trailing whitespace from "Function Index" search
  expression.
- bug #65753: Documentation window: Function Index: Search box respects
  trailing spaces.
- bug #65730: Some remaining cases of silent conversion of fractional inputs.
- bug #65683: `issorted`: enable `'monotonic'` and `'strict...'` sort modes.
- bug #65674: `axes` `'colormap'` property being set over `figure` `'colormap'`
  property.
- bug #65665: Input validation for `system()`.
- bug #65645: Execute FIXME of `perms.cc`: Use `constexpr` instead of template
  specialisation.
- bug #65641: `view` produces incorrect viewpoint when given a vector aligned
  with the primary axes.
- bug #65637: `short_disp` doesn't show ellipsis at end of long arrays.
- bug #65617: <F9> clears editor setting 'Always show debug breakpoints and
  pointers...'.
- bug #65577: Feature request: Editor right-click to run test without needing
  to clear `%!` first.
- bug #65538: `xint_value` does not work the way it is intended.
- bug #65531: `iqr` handling of empty inputs is not compatible.
- bug #65499: Use separate GUI settings for Octave releases.
- bug #65495: `nchoosek` error 'gcd: all values must be integers'
- bug #65459: `uifigure ('visible', 'off')` temporarily creates a visible
  figure window.
- bug #65447: `jsonencode` does not accept integer values larger than 999999.
- bug #65441: `nextpow2` incorrect for some inputs slightly larger than powers
  of two.
- bug #65238: Improve `nchoosek.m` algorithm to prevent numerical issues.
- bug #65221: `movfun`: Create `inputParser` only once.
- bug #65176: `unique.m`: Enable third output with option `'stable'`.
- bug #65134: `griddata`: Output size inconsistent for vector input
  interpolation points.
- bug #65030: Column width in browser pane not retained between restarts of
  Octave.
- bug #65010: Compatibility: Colormaps now default to 256 colors in Matlab.
- bug #64995: Implement `height` and `width` as aliases for `rows` and
  `columns` for Matlab compatibility.
- bug #64991: `polar` doesn't populate `'rtick'` with center tick value.
- bug #62928: Error sourcing file message when script with embedded '.' in
  filename has syntax error.
- bug #61295: `cross()`: Dimensions inconsistent with Matlab when using
  mismatched input vector dimensions.
- bug #60797: `sqrtm`: Returns `NaN` for matrix of ones with rows and columns
  >=4.
- bug #60726: Fix `nargout` for `subsref` when returned value may be a cs-list.
- bug #56690: Support displaying lazy index objects in variable editor.
- bug #55961: `properties` function does not preserve order.
- bug #55198: `rat()` should support complex numbers.
- bug #41028: `lastwarn`: Save warning info for disabled warnings.
- bug #41028: Modify built-in self-tests to pass with Matlab-compatible
  `lastwarn()` behavior.

### Old release news

- [Octave 9.x](etc/NEWS.9.md)
- [Octave 8.x](etc/NEWS.8.md)
- [Octave 7.x](etc/NEWS.7.md)
- [Octave 6.x](etc/NEWS.6.md)
- [Octave 5.x](etc/NEWS.5.md)
- [Octave 4.x](etc/NEWS.4)
- [Octave 3.x](etc/NEWS.3)
- [Octave 2.x](etc/NEWS.2)
- [Octave 1.x](etc/NEWS.1)
