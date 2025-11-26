Summary of important user-visible changes for version 11 (yyyy-mm-dd):
---------------------------------------------------------------------

### General improvements

- The internal interface to Java has been updated to be more memory-efficient
  (faster culling of unused objects).  Building Octave from source code with
  Java support now requires JDK 1.9 or newer.  Distributed, precompiled
  versions of Octave will run with any JVM.

- The `pkg` command has these user-visible changes:
  * The package installation command `pkg install foo` now automatically
    determines whether `foo` is a local file, a URL, or an unqualified package
    name in that order, and installs it as appropriate.  The `-forge` option is
    no longer required but is still accepted.  Octave will issue a warning
    which can be silenced with the warning ID `"Octave:pkg:install-forge"`.
  * There is a new `pkg search` functionality.  Running `pkg search foo bar
    baz` searches Octave Packages online for packages having all those keywords
    in their descriptions.  Search terms can also be regular expressions.
  * The old command `pkg list -forge`, which returned a list of packages found
    online, has been replaced by `pkg search -all`.  Calling `pkg list -forge`
    now gives a warning and then executes `pkg search -all`.  The warning can
    be silenced with the ID `"Octave:pkg:list-forge"`.
  * Package metadata is now cached on the local computer for speed and
    reliability.  Previously, it used to be downloaded afresh for operations
    such as `install` or `update` or `search`.  Metadata updates can also be
    done on demand with the new option `-refresh` for those three actions.

- Convolution of short and wide arrays (especially row vectors) is now faster
  by anywhere from 10% to 150X, depending on the array size and shape.
  Previously, convolution of tall and skinny arrays (especially column vectors)
  was much faster than convolving short and wide ones, requiring transposition
  or permutation of the inputs for performance.  Now Octave automatically picks
  the fastest calculation order irrespective of the input orientation.

- New function `_Exit` has been added which makes it possible to use
  `fork`/`_Exit` sequence to perform work in parallel child processes for
  potential performance gains.

- The `fzero` function is now more accurate (1-2 eps when TolX is eps).

- The `roots` function now accepts only a numeric argument.  Convert any
  non-numeric input to a numeric value with `double()`.

- The `mean` function no longer accepts character array inputs.  Convert any
  character input to a numeric value (e.g., use `double()` or `single()`).

- On dot-indexing of `classdef` objects, `subsref` now resolves to looking for
  a property first, and if a property is not found, then it looks for a method.

### Graphical User Interface

- The File Browser now has two tabs: the first for browsing the file system
  as usual, and the second for viewing and managing all open editor files.

- The Find File dialog now offers most recently used lists for input
  fields and the ability to search for/in several file patterns at the
  same time.

- The GUI now uses scalable SVG icons for beautiful display at any size.

- The GUI now uses the default Octave command prompt.  This prompt is
  completely customizable by calling `PS1` in an initialization file such as
  `.octaverc`.  To recover the former behavior of the GUI use `PS1 ('>> '); PS2
  ('')`.

- The GUI has two new user preferences: one for showing a small splash screen
  at startup, and one for using the directory of the currently focused editor
  file instead of the current Octave directory as the initial folder for the
  Open File dialog.

- The text of the Octave GUI has been translated to a new language: Asturian.
  It uses the ISO 639 language code ast_ES.

### Graphics backend

- `polar` plot background patch object has been moved to the Z=0 plane to
  permit view changes that previously resulted in an obscured plot area
  (bug #67574).

- Octave no longer strips leading and trailing spaces from user-specified
  ticklabels (bug #67302).  This makes it possible to use a monospaced font to
  align labels as the user intends.  This change is Matlab-compatible.

- The `xminortickvalues`, `yminortickvalues`, `zminortickvalues` axes
  properties that were previously hidden have been made visible to allow users
  to control the position of minor ticks (bug #67394).

### Matlab compatibility

- The warning `"Octave:language-extension"` is no longer emitted for automatic
  broadcasting which Matlab has supported since version 2016B.

- The function `zscore` now accepts the optional arguments `"all"` or `vecdim`,
  and `nanflag`.

- The functions `all` and `any` now accept the optional arguments `"all"` or
  `vecdim`.

- The functions `cumprod` and `cumsum` now accept the optional arguments
  `"all"` or `vecdim`, `direction`, and `nanflag`.

- The functions `prod`, `sum`, and `sumsq` now accept the optional arguments
 `"all"` or `vecdim`, `nanflag`, and `"default"` in `outtype`.  `prod` and
 `sumsq` functions now retain the class of the input argument by default.
 'sumsq' now also accepts an outtype argument.

- The function `gradient` with a numeric input now only accepts type double
  or single.

- The function `iqr` now calculates an optional second output `q` which
  contains the 25th and 75th quantiles used to calculate the interquartile
  range.

- The function `polyfit` now returns the highest powers possible and the
  constant term when there is insufficient data to create all N terms of the
  polynomial.  The output structure has a new field `rsquared` which is the
  coefficient of determination describing how well the polynomial fits the
  data.

- The function `cellfun` now fails if the returned type does not match in the
  invocations for each cell element.  In earlier versions, the elements were
  converted to a compatible type if possible.

- The function `qr` for a dense matrix input `A` and a single output now
  returns just the matrix `R`, rather than an augmented version from
  @sc{lapack} with additional information in the lower-triangular portion of
  the matrix.  This resolves the output incompatibility with the sparse calling
  syntax for this function, as well as the incompatibility with @sc{matlab}.

- The functions `fill` and `fill3 now handle all input combinations of vector
  and matrix vertex data and color data in a Matlab-compatible way.

- The `image` function now validates that `x` and `y` inputs are vectors, and
  accepts and processes 1-element vectors for compatibility.

- The `ind2rgb` and `ind2gray` functions now clip exceptional values such as
  `NaN` or `Inf` to the last value in the colormap for for compatibility.

- The function `colorbar` now requires the `location` input to be the first
  argument rather than the last argument.  This may require updating existing
  Octave scripts.  The graphics properties `AxisLocation`, `Direction`, and
  `TickLabels` have been implemented.

- The `weboptions` has been re-written internally.  Support for non-integer
  Timeout values, and the special value `Inf`, has been added.

- The `urlread` and `urlwrite` functions now make use of the various
  `weboptions` such as timeouts and user credentials.  The old calling form
  with passing a cell array is still supported for backward compatibility.

- The `newplot` function has been re-written to remove functionality that was
  possibly never used in Octave, and has been removed from Matlab.  The
  function is also more compatible, and does not change the current figure
  unexpectedly.  However, any user plotting code that depends on this broken
  behavior will need to be updated.

- The `uitable` property `"ColumnWidth"` now supports the argument `"fit"`
  which produces smaller column widths than `"auto"` (sized to width of data,
  rather than possibly including `ColumnName` in width calculation).
  Specifying small numeric widths down to 1-pixel now works correctly.

- The `KeyEvent` structure passed as an input paramater to graphics callback
  functions such as `KeyPressFcn` now returns fieldnames in the same order
  as Matlab: `Character`, `Modifier`, `Key`.

- The `KeyEvent` structure now includes the fields `Source` (graphics handle
  of object returning `KeyEvent`) and `EventName` (e.g., `KeyPress` or
  `KeyRelease`).

### Alphabetical list of new functions added in Octave 11

* `_Exit`
* `corrcov`
* `dither`

### Deprecated functions, properties, and operators

The following functions and properties have been deprecated in Octave 11
and will be removed from Octave 13 (or whatever version is the second
major release after 11):

- Functions

        Function               | Replacement
        -----------------------|------------------
        polyfit output s.X     | polyfit output s.V

- Core

        Function                  | Replacement
        --------------------------|--------------------------
        octave::math::x_nint      | octave::math::round
        octave::math::isinteger   | octave::math::is_integer

    The environment variable `OCTAVE_LATEX_DEBUG_FLAG` to enable debugging of
    the latex interpreter has been renamed to `OCTAVE_LATEX_DEBUG`.

    The environment variable `QTHANDLES_DEBUG` to enable debugging of the GUI
    has been renamed to `OCTAVE_QTHANDLES_DEBUG`.

The following features were deprecated in Octave 9 and have been removed
from Octave 11.

- Core

        Function                                     | Replacement
        ---------------------------------------------|-----------------------------------
        octave::initialized                          | octave::is_initialized
        octave::same_file                            | octave::sys::same_file
        octave::interpreter_initialized              | octave::interpreter_is_initialized
        all_ok (Array<octave::idx_vector>&)          |
        idx_vector::bool ()                          |
        octave_value (const Array<octave_value>& a)  | octave_value (const Cell&)

  - The `octave_value (const Array<octave_value>& a)` constructor was
    deprecated in Octave 10 and is removed after only one major version.

### Old release news

- [Octave 10.x](etc/NEWS.10.md)
- [Octave 9.x](etc/NEWS.9.md)
- [Octave 8.x](etc/NEWS.8.md)
- [Octave 7.x](etc/NEWS.7.md)
- [Octave 6.x](etc/NEWS.6.md)
- [Octave 5.x](etc/NEWS.5.md)
- [Octave 4.x](etc/NEWS.4)
- [Octave 3.x](etc/NEWS.3)
- [Octave 2.x](etc/NEWS.2)
- [Octave 1.x](etc/NEWS.1)
