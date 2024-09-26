Summary of important user-visible changes for version 10 (yyyy-mm-dd):
---------------------------------------------------------------------

### General improvements

- Three short form aliases have been added for long form options when starting
`octave`.
  * `-e CODE` for `--eval CODE`
  * `-g` for `--gui`
  * `-G` for `--no-gui`

- Three long form options have been introduced for clarity.
  * `--no-init-user` : Don't read user configuration files ~/.octaverc or
    .octaverc files at startup.
  * `--no-init-site` : Don't read site-wide configuration files at startup.
  * `--no-init-all` : Don't read any configuration files at startup.

- `nchoosek` algorithm is now ~2x faster and provides greater precision.

- `nextpow2` algorithm is now more accurate for inputs very close to a power
  of 2.  The output class now matches the input class for Matlab compatibility.
  The function no longer accepts complex inputs and will emit an error for
  these inputs.

- `jsonencode` now outputs integers and floating point integers without ".0"
  suffix.

- `hist` now accepts N-dimensional array inputs for input `Y` which is
  processed in columns as if the array was flattened to a 2-dimensional array.

- The third output for `unique` is now correct when `stable` sort option is
  used.

- Support setting breakpoints in `set` or `get` methods of `classdef`
  properties (bug #65610).

### Graphical User Interface

### Graphics backend

- `polar` plots now include the center tick mark value, typically 0, in the
  `'rtick'` parameter when the plot is created.  Subsequent modifications to
  `'rtick'` by the function `rticks` will only include the center tick mark
  value if it is specified.

- `view` correctly interprets cartesian viewpoints on main axes (bug #65641).

### Matlab compatibility

- `height` and `width` are now aliases for the `rows` and `columns` functions.

- All colormaps now default to a size of 256 colors. (The previous default size
  was 64.)

- The first argument to `colormap` may now be a figure or axes object.  Calling
  `colormap` on a figure object will clear any "colormap" properties set at the
  axes level.

- `griddata` output size more consistently matches the input interpolation
  points when they are input as vectors.  When they are same-orientation
  vectors, the outputs will be the same size as those vectors.  When either is
  a row vector and the other is a column vector, the interpolating points are
  processed through meshgrid and the output is a matrix the same size as the
  meshgrid.

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

- `issorted` now accepts the MODE option "monotonic", which has the same
  behavior as the option "either".
  
- `movfun` and `movslice`:  Functions now accept `wlen` equal to 1 or [0,0],
  non-integer values of `wlen`, and values of `wlen` that create window lengths
  exceeding the size of the target array. `movfun` also accepts values of `dim`
  larger than the number of non-singleton dimensions in the target array.  The
  `SamplePoints` option has been implemented for both functions.  Non-numeric
  input array handling has been improved.  These changes affect all moving
  window functions (`movmad`, `movmax`, `movmean`, `movmedian`, `movmin`,
  `movprod`, `movstd`, `movsum`, and `movvar`) (bug #65928 and bug #66025).

- `movfun`: The `nancond` property has been fully implemented and made MATLAB
  compatible.  The `omitnan` option will ignore NaN and NA values when
  calculating the function return, and, if all elements in a window slice are
  NaN or NA, it will return the value contained in a new property `nanval`
  (default NaN) for that element.  The `includenan` property (the default) has
  been updated for compatibility such that any window containing NaN or NA will
  return NaN rather than passing those values to the calculating function.
  `omitmissing` and `includemissing` are now accepted as aliases for `omitnan`
  and `includenan`.  These changes affect all moving window functions (`movmad`,
  `movmax`, `movmean`, `movmedian`, `movmin`, `movprod`, `movstd`, `movsum`, and
  `movvar`) (bug #66156).
  
- `movmin` and `movmax`: These functions now have their default behavior set to
  `omitnan`.  NaN and NA values will be ignored unless a moving window contains
  only NaN or NA values, in which case the function will return NaN for that
  element (bug #66156).

- `movsum`: When called with option `omitnan`, any windows containing only NaN
  and NA values will return 0 (bug #66156).

- `movprod`: When called with option `omitnan`, any windows containing only NaN
  and NA values will return 1 (bug #66156).
  
- `movmad`: The function now defaults to calculating median absolute deviation.
  Before Octave 10, the function calculated mean absolute deviation.  A new
  `mode` option has been provided that takes a string of either "mean" or
  "median" to allow the user to select which option to use.  This option should
  not be expected to function in code used outside of Octave.

### Alphabetical list of new functions added in Octave 10

* `clim`
* `rticklabels`
* `tticklabels`

### Deprecated functions, properties, and operators

The following functions and properties have been deprecated in Octave 10
and will be removed from Octave 12 (or whatever version is the second
major release after 10):

- Functions

        Function               | Replacement
        -----------------------|------------------

- Core

        Function                              | Replacement
        --------------------------------------|------------------
        symbol_record_rep::mark_as_variable   | symbol_record_rep::mark_variable
        symbol_record_rep::unmark_as_variable | symbol_record_rep::unmark_variable
        symbol_record::mark_as_variable       | symbol_record::mark_variable
        symbol_record::unmark_as_variable     | symbol_record::unmark_variable
        interpreter::verbose                  | interpreter::init_trace
        cmdline_options::verbose              | cmdline_options::init_trace

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

        Function               | Replacement
        -----------------------|------------------
        shift                  | circshift
        sparse_auto_mutate     | none (see below)

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

### Build system

- Octave now requires a C++ compiler that is compliant with C++17 (preferably
  with GNU extensions).


Summary of bugs fixed for version 10.1.0 (yyyy-mm-dd):
----------------------------------------------------

- Improve nchoosek.m algorithm to prevent numerical issues (bug #65238)
- nchoosek error "gcd: all values must be integers" (bug #65495)
- nextpow2 incorrect for some inputs slightly larger than powers of two (bug #65441)
- jsonencode does not accept integer values larger than 999999 (bug #65447)
- hist produces nonconformtant arguments error when input has ndims > 2 (bug #65478)
- compatibility: colormaps now default to 256 colors in matlab (bug #65010)
- implement height and width as aliases for rows and columns for matlab compatibility (bug #64995)
- 'view' produces incorrect viewpoint when given a vector aligned with the primary axes (bug #65641)
- polar doesn't populate rtick with center tick value (bug #64991)
- iqr handling of empty inputs is not compatible (bug #65531)
- griddata: output size inconsistent for vector input interpolation points (bug #65134)
- unique.m - Enable third output with option 'stable' (bug #65176)
- Input validation for system() (bug #65665)
- rat() should support complex numbers (bug #55198)
- cross() dimensions inconsistent with Matlab when using mismatched input vector dimensions (bug #61295)
- xint_value does not work the way it is intended (bug #65538)

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
