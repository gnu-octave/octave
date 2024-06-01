Summary of bugs fixed for version 9.2.0 (2024-06-01):
-----------------------------------------------------

For (bug #XXXXX) see https://savannah.gnu.org/bugs/?XXXXX

### Improvements and fixes

- `hist.m`: Add input validation for `Y` restricting it to 2-D array
  (bug #65478).  Avoid error when `Y` value range is very small (bug #65714).
- `cross.m`: Add input validation for `dim` restricting it to a numeric
  integer valued scalar (bug #65544, bug #65527).
- `getframe.m`: Respect pixel ratio (high DPI) of screen with figure
  (bug #65540).
- `legend.m`: Fix error if root property `"showhiddenhandles"` is `"on"`
  (bug #65442)
- `savepath.m`: Correctly handle packages without binaries (bug #65559).
- Correctly scale figure position on screen with DPI scaling (high DPI).
- `profile ('on')` now clears any existing profile data as the documentation
  states (bug #65595).
- Fix segmentation fault when trying to set breakpoint in non-existent method
  of `classdef` class (bug #65610).
- Improve default display of `classdef` properties (bug #62432).
- Avoid crash with Qt6 6.7.0 (bug #65605).
- `bar.m`: Catch input number validation error.
- Prevent OOM crash or segmentation fault in `sort ()` when `dim` equals `Inf`
  (bug #65712).
- `legend.m`: Avoid setting more colors than coordinates for `patch` objects
  (bug #65632).
- `inputParser.m`: Allow default classdef objects which overload `struct`
  (bug #65667).
- Preserve `"position"` property of figure when object is `reset()`
  (bug #65750).
- `hist.m`: Avoid error when `y` value range is very small (bug #65714).
- `barh`: Add input validation check for `nargin < 1`.

### GUI

- Use first word for options in right-click menu of command window widget
  (bug #65518).
- Set `DontUseNativeDialog` flag as first property in `QFileDialog`.
- Explicitly raise non-modal message boxes ensuring visibility.
- Save and restore splitter state of documentation widget.
- Allow executing new files from built-in editor with F5 (bug #65548).
- Allow unbinding GUI shortcuts (bug #65580).
- Fix restoring headers in file browser and workspace view (bug #65030).
- File dialogs of the built-in editor are now modal.
- Fix dragging editor from main window into floating state (bug #65725).

### Build system / Tests

- Avoid overriding `save_*` variables from outer scope (bug #65476).
  This fixes an error that might have lead to overlinking of shared libraries
  (e.g., `.oct` files). Consider rebuilding shared libraries that have been
  built with Octave 9.1.0.
- Add BIST for `is_valid_file_id.m` (bug #65543).
- Update metainfo.xml with new fields for AppStream 1.0 (bug #65355).
- Show result of check for `std::pmr::polymorphic_allocator` in configure
  summary.
- Run test program for polymorphic allocators if possible instead of a simple
  build check.
- Speed up BIST for the central part of `convn` with `'full'` shape.
- Require Qt Widgets module when building the GUI (bug #65625).
- `bug-53027.tst`: Delete temporary file after test is done (bug #53027).
- Avoid build error with GCC 14 when targeting Windows.
- Try to clean up after BIST also in case test failed (bug #53027).
- `bar.m`, `barh.m`: Add plotting BISTs (bug #65671).
- Check if C and Fortran compilers accept `-fexceptions` flag (bug #65767).
  This affects building Octave itself from sources and also how .mex or .oct
  files are built by `mex` and `mkoctfile`.

### Documentation

- Describe shape of outputs for `hist` (bug #65471).
- Simplify programming notes for `patch` objects (bug #65421).
- `vecnorm.m`: Add missing parenthesis to equation in docstring.
- Add example to Minimizers section on using anonymous functions to pass
  additional arguments to functions called by minimizer functions
  (`fminsearch`, `fminbnd`, `fminunc`).
- Add application notes in `fminsearch`, `fminbnd`, `fminunc` indicating the
  preferred way to pass parameters is through anonymous functions.
- Update remaining copyright statements to 2024.
- Minor fix for `setappdata.m`.
- Section "Assignment Expressions": Use `@emph` rather than `@i` macro for
  better rendering in plaintext formats.
- Section "Running Octave": Tell new users how to start Octave on their
  computers.
- `tsearch`: Add programming note about expected performance.

### Deprecated functions, properties, and operators

- `fminsearch` parameter passing:  A legacy, undocumented, and only partially
  supported syntax for passing parameters to the minimized function `fcn`
  called by `fminsearch` by appending them to the input argument list has
  functioned intermittently since Octave 4.4.0.  Due to conflicts with other
  compatibility-required input methods the documentation of this syntax was
  removed in Octave 5.1.0, and the remaining functionality will be completely
  removed in Octave 10.  The preferred method of passing parameters to any of
  the minimization functions (including `fminsearch`, `fminbnd`, and `fminunc`)
  is through the use of anonymous functions.  Specific examples of this can be
  found in the "Minimizers" section of the GNU Octave manual.


Summary of important user-visible changes for version 9 (2024-03-12):
---------------------------------------------------------------------

### General improvements

- `dec2base`, `dec2bin`, and `dec2hex` have all been overhauled.  All three
  functions now accommodate negative inputs and fractional inputs, and repeated
  code between the functions has been reduced or eliminated.  Previously only
  `dec2bin` and `dec2hex` accepted negative inputs but `dec2base` did not, and
  none of the three accepted fractional inputs.  Now, `dec2base (100*pi, 16, 4,
  6)`, for example, returns a base-16 string with four places for the integer
  part and six places for the fractional part.  Omitting the number of decimal
  places (the fourth input) retains old behavior for backward compatibility,
  except that non-integer inputs will no longer produce errors.

- `quiver` and `quiver3` now properly plot non-float numeric inputs by
  internally casting them to 'double' (bug #59695).  Both functions now honor
  a previously ignored scaling factor input when there is only a single arrow
  (bug #39552), and `quiver3` no longer produces an error when a scaling factor
  is given without x and y inputs.  Both functions now also accept a scale
  factor of "off" which is equivalent to setting it to 0.  When a linestyle
  with a base marker is set suppressing arrowhead display, subsequent
  name-value property pairs in the `quiver`/`quiver3` function call will no
  longer turn arrowhead display back on (bug #64143).  The linewidth property
  now also affects the line width of the base marker.

- Classdef files now support breakpoints inside them.

- The `inputParser` function has been re-architected for a 60% performance
  improvement.

- The `perms` function has been made faster.

- The `audiowrite` function now supports writing to MPEG audio
  formats---including MP3---if the `sndfile` library supports it.

- `oruntests`: The current directory now changes to the directory
  containing the files with the tests for the duration of the test.  This
  aligns the behavior of this function with Octave's test suite.  This also
  means that the file encoding specified in the `.oct-config` file for the
  respective directory is taken into account for the tests.

### Graphical User Interface

- The Graphical User Interface (GUI) of Octave is now compatible with Qt6.  Qt6
  is selected by default if the necessary Qt modules are detected and the used
  compiler is compatible.  Support for building with Qt5 is still available but
  might be removed in a future version of Octave.  Qt5 is used if configuring
  with `--with-qt=5` or as a fallback if Qt6 cannot be used.

### Graphics backend

- The FLTK backend is not maintained and its use is discouraged.  The
  recommended backend is qt.  Enabling the fltk backend with `graphics_toolkit
  fltk` now emits a warning.

- The `set` function now accepts any combination of name/value pairs, cell
  array of names / cell array of values, or property structures.  This change
  is Matlab compatible.

- When the `hold` function is used without arguments to toggle the current
  state, the resulting value is now displayed in the Command Window for
  informational purposes.

- The axes graphics property `"TickDir"` now accepts the option `"none"`, which
  will not draw tick marks but will still draw tick labels.

- Stricter checks are performed for valid dimensions of the color data
  properties `"CData"` and `"FaceVertexCData"` when rendering `patch` objects.

### Matlab compatibility

- The `inputParser` function now implements the `PartialMatching` property for
  `Parameter` and `Switch` names.  For Matlab compatibility, `PartialMatching`
  is now set to true by default which may change the behavior of existing code.

- Overhauled `mean`, `median`, `var`, and `std` functions have been imported
  from statistics package v1.5.4 to compatibly implement 'nanflag'
  (bug #50571), 'all' (bug #58116), and 'vecdim' (bug #58089) options, preserve
  output class, and match expected output behavior for empty (bug #50583) and
  sparse inputs (bug #63291).  Both `median` and `mean` can handle large
  integer values without overflow or precision concerns (bug #54567), and
  `mean` avoids errors due to limits of single precision by processing as
  doubles (bug #63848).  `median` has also adopted the 'outtype' option from
  `mean`.

- Code such as `A = ones (3, 3); A(:, :, 1) = []` is now Matlab compatible.

- `mad` function now produces Matlab compatible output using improved `mean`
  and `median` functions.  'vecdim' and 'all' options are now supported.  `mad`
  ignores all NaN values (using 'omitnan' mean/median option) and produces
  expected output behavior for empty inputs.

- `mode` now produces Matlab compatible output for empty inputs (bug #50583).

- `linspace` and `logspace` now handle `Inf` inputs in a Matlab compatible way.

- `normalize` now produces Matlab compatible output for inputs containing NaN
  values (bug #50571).

- `cov` now processes the input form cov(x,y) with two separate data arrays x
  and y, as cov(x(:),y(:)) to maintain Matlab compatibility.  It also accepts a
  NANFLAG option to allow ignoring NaN entries in input data (bug #50571) and
  produces Matlab compatible outputs for empty inputs (bug #50583).  `corr` and
  `corrcoef` internal code has been adapted to the new `cov` behavior with no
  change to user interface.  Packages using the octave core's `cov` that rely
  on the previous calling form may need to make similar adaptations.  Calls for
  cov(x) with a vector x expecting a scalar return can get the previous results
  from `cov(x)(2)`, and calls to cov(x,y) with x and y matrices expecting
  columnwise covariance calculation can obtain the previous results using
  `cov([x,y])(1:nx, nx+1:end)`, where nx = columns(x).

- `qz` now handles input and output arguments in a Matlab compatible way.  It
  always computes the complex QZ decomposition if there are only two input
  arguments, and it accepts an optional third input argument 'real' or
  'complex' to select the type of decomposition.  The output arguments are
  always in the same order independently of the input arguments, and the
  generalized eigenvalues are no longer returned (one can use the `ordeig`
  function for that purpose).  The optional reordering of the generalized
  eigenvalues has been removed (one can use the `ordqz` function for that
  purpose).

- `pcolor` now sets the axes limits to be just large enough to display the
  plotted data (equivalent of `axis tight`).

- `xlim`, `ylim`, `zlim` functions can now query or set the limit calculation
  method which is one of "tickaligned", "tight", or "padded".

- `optimget` and `optimset` now produce an error on an ambiguous match of an
  incomplete option name instead of emitting a warning.

- `datevec` when provided a date format will match date string characters up to
  the length of the format string.  A date vector will be returned as long as
  the characters up to the format string length are a perfect match, and any
  trailing characters in the date string will be ignored. (bug #42241)

- `unwrap` now produces compatible output for inputs with non-finite elements
  (Inf, NaN, NA).  Such elements will now retained in the output but skipped
  over by the wrapping calculation.  The function also permits logical inputs
  and specified operating dimensions larger than the number of dimensions in
  the input array.

- The default string length for the function `rats` has changed from 9 to 13.

- *Continuing reminder of future breaking changes*:  There is ongoing work
  to implement a Matlab compatible string class that will differ from a vector
  of characters.  In Octave, single-quoted character arrays are currently
  compatible with Matlab, but double-quoted forms are not, and are likely to
  change in future as a consequence of implementing Matlab-style string syntax.
  For example, 'foo' will remain a three-element character vector, but
  "foo" will become a single-element string object.

  *What this means for user code*: If your code currently relies on
  double-quoted strings (e.g., "foo") representing character vectors as
  opposed to string objects, then start replacing all double-quoted strings
  with single-quoted strings in your code over time (e.g., replace "foo" with
  'foo').  Single-quoted strings will retain their current behavior.  Further,
  if your code relies on backslash escape sequence interpretation in
  double-quoted strings (except in special cases like the `printf` family),
  that code may need to change as well.

### Alphabetical list of new functions added in Octave 9

* `isenv`
* `ismembertol`
* `isuniform`
* `tensorprod`

### Deprecated functions, properties, and operators

The following functions and properties have been deprecated in Octave 9 and
will be removed from Octave 11 (or whatever version is the second major release
after 9):

- Functions

        Function               | Replacement
        -----------------------|------------------

- Properties

  The following property names are discouraged, but there is no fixed
  date for their removal.

        Object           | Property    | Replacement
        -----------------|-------------|------------

- Core

    * The `idx_vector::bool()` function is obsolete and always returns true.
      Any uses can simply be removed from existing code with no loss of
      function.

    * The `all_ok(const Array<octave::idx_vector>&)` function in `Array-util.h`
      is obsolete and always returns true.  Any uses can simply be removed from
      existing code with no loss of function.

    * The member variable `octave_base_value::count` is deprecated and will be
      removed from Octave 11.  Replace all instances with the new name
      `m_count`.

The following features were deprecated in Octave 7 and have been removed from
Octave 9.

- Functions

        Function                   | Replacement
        ---------------------------|------------------
        disable_diagonal_matrix    | optimize_diagonal_matrix
        disable_permutation_matrix | optimize_permutation_matrix
        disable_range              | optimize_range

- Operators

        Operator | Replacement
        ---------|------------
        .+       | +
        .+=      | +=
        .-       | -
        .-=      | -=
        **       | ^
        **=      | ^=
        .**      | .^
        .**=     | .^=

- Interpreter

    * The use of `'...'` for line continuations *inside* double-quoted strings
      has been removed.  Use `'\'` for line continuations inside strings
      instead.

    * The use of `'\'` as a line continuation *outside* of double-quoted
      strings has been removed.  Use `'...'` for line continuations instead.

    * Support for trailing whitespace after a `'\'` line continuation has been
      removed.  Delete unnecessary trailing whitespace.

- For plot functions, the use of numbers to select line colors in shorthand
  formats was an undocumented feature that was removed in Octave 9.

- The environment variable used by `mkoctfile` for linker flags is now
  `LDFLAGS` rather than `LFLAGS`.  `LFLAGS` was deprecated in Octave 6 and has
  been removed.

Summary of bugs fixed for version 9.1.0 (2024-03-12):
----------------------------------------------------

- Bugfixes to `whos -file`, `urlread`, `mat2cell`, `set`, `savepath`,
  `loadpath`, `griddata`, and the general interpreter stack-handling.

- Better input validation for `sparse`, `speye`.

- Memory usage reduced for `movfun` and `movslice`.

- Memory usage reduced when saving to file, preventing OOM and data loss.

- Memory usage improved when plotting grid tick marks.

- Memory management made more robust in the GUI code and with several GUI
  components (variable editor, doc browser, etc).

- Text encoding for non-UTF-8 generally made more robust.  File editor now
  lists available encodings.

- Octave on Windows now supports directory names and path names with non-ASCII
  characters (other platforms had it already for years).  Windows paths now
  also allow relative paths on different drives.

- Several race conditions removed in signal handler.

- Better compatibility when linking to `libc++`.

- Performance and stability improvements: avoid unnecessary string
  construction, use static casts instead of dynamic casts where possible,
  eliminate various isolated crash conditions.

### Old release news

- [Octave 8.x](etc/NEWS.8.md)
- [Octave 7.x](etc/NEWS.7.md)
- [Octave 6.x](etc/NEWS.6.md)
- [Octave 5.x](etc/NEWS.5.md)
- [Octave 4.x](etc/NEWS.4)
- [Octave 3.x](etc/NEWS.3)
- [Octave 2.x](etc/NEWS.2)
- [Octave 1.x](etc/NEWS.1)
