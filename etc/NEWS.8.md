Summary of bugs fixed for version 8.2.0 (2023-04-13):
----------------------------------------------------

For (bug #XXXXX) see https://savannah.gnu.org/bugs/?XXXXX

### Improvements and fixes
- `pr-output.cc`: Fix output for `format native-bit` (bug #63940).
- Fix evaluation of `&` and `|` expressions in conditional contexts (bug
  #63935).
- Avoid clang warning about very unlikely buffer overflow.
- `mpoles.m`: Overhaul function and use absolute tolerance for zero poles
  (bug #63937).
- `perms.m`: Change `"unique"` output order to reverse lexicographic to
  match non-unique order (bug #63962).
- Remove trailing `'\r'` from curl dir list (bug #63851).
- `fopen`: Use "UTF-8" as default encoding for `fopen` (bug #63930).
- Don't use encoding facet when writing bytes to stream (bug #63931).
- `fopen`, `unicode2native`: Fix converting the encoding of short char
  arrays with invalid UTF-8 (bug #63930).
- `fopen`: Try to gather complete UTF-8 surrogates when converting
  encoding (bug #63930).
- Fix display of scalar complex variables with mixed `Inf`/`NaN` and
  floating point values (bug #63961).
- `fopen`: Do not convert encoding for file streams with libc++
  (bug #63930).

### GUI
- Speedup loading and saving preferences dialog (bug #63909).

### Build system / Tests
- `inpolygon.m`: Fix demo code (bug #63865).
- `if.tst`: New test for bug #63935.
- `acinclude.m4`: Correct typo in `#define PCRE2_CODE_UNIT_WIDTH`.
- `lu`: Add self-test with complex valued input.
- Disable visibility flags by default (bug #61855, bug #63916).
- Check whether using STL from LLVM or Apple (bug #63930).

### Documentation
- Improve documentation for `linspace` and `logspace` functions.
- Correct and improve documentation for `sparse()` function.


Summary of important user-visible changes for version 8 (2023-03-07):
--------------------------------------------------------------------

### General improvements

- Octave's libraries are now built using symbol visibility by default.
  That means that fewer symbols are exported from these libraries.
  Configure with `--disable-lib-visibility-flags` to export all symbols
  (as in previous versions).

- `filter` is now 5X faster, which also speeds up `deconv`, `fftfilt`
  and `arma_rnd` by the same amount.

- `integral` can now return a second argument containing the error
  estimate from the underlying integrator.

- `perms` now accepts a second argument "unique" to return only unique
  permutations for inputs with repeated elements.  It is faster and
  takes less memory to call `perms ('aaaabbbbcccc', "unique")` than to
  call `unique (perms ('aaaabbbbcccc'), "rows")`.

- `quadgk` can now accept the `ArrayValued` input parameter to handle
  array-valued input functions.

- `delaunayn` now has consistent trivial simplex checking and removal
  for all input dimensions, simplex checking 3D inputs is now
  vectorized, and >3D simplex checking performance has been improved.
  Simplexes points are now ordered so they will all have positive
  outward normal vectors.  Input type checking has also been added for
  improved error handling.

- `factor` now factorizes all 64-bit integers in roughly the same time.
  Previously, the product of two large primes took much longer to factorize
  than highly composite inputs.

- `Refine` option is now implemented in functions `ode45`, `ode23`,
  and `ode23s`.

- Octave is now compatible with PCRE2 (UTF-8).  PCRE2 is preferred over PCRE
  if both are installed.  Configure with `--without-pcre2` if you prefer Octave
  to use PCRE in this case.
  
- `mean` now internally processes data as type double to reduce likelihood of
hitting overflow or precision limits with other types (bug #63848).

### Graphical User Interface

- The GUI has a dark style and several new icons in toolbars for better
  visibility and higher contrast.

- A new terminal widget has been added for the GUI.  It is still experimental
  and is disabled by default.  It can be enabled with the command line
  parameter `--experimental-terminal-widget`.

- More fonts for the documentation browser are included.

### Graphics backend

- When printing, the option `-svgconvert` is now the default.  If you want to
use the more limited EPS-based tool chain (the former default)&mdash;e.g., if
you encounter inaccurate image rendering&mdash;you can pass the `-nosvgconvert`
option to the `print` function.

- Additional properties have been added to the `figure` graphics object:
    * `"innerposition"` (equivalent to `"position"`)
    * `"windowstate"` (not yet implemented)

- Legend now features a new property `"itemhitfcn"`, allowing the
  execution of a callback function when a legend item is clicked.

### Matlab compatibility

- `inline` functions now support all Matlab methods.  The performance
  of `inline` functions has also been improved.

- `sub2ind` now supports index values outside of the size specified by
  the subscripts.

- `cylinder` now accepts a scalar for the radius argument.

- `clock` now has an optional second output `ISDST` which indicates if
  Daylight Savings Time is in effect for the system's time zone.

- `print` now accepts option `-image` to specify the "opengl" renderer
  and `-vector` to specify the "painters" renderer.

- `format` now accepts the option "default", which is equivalent to
  calling `format` without any options to reset the default state.

- `quadgk` now stops iterating when `error <= tolerance` while the previous
  condition was `error < tolerance`.

- `mean` now accepts vector dimensions and options to handle `NaN` values.
  The option `"a"` (arithmetic mean), `"g"` (geometric mean), and `"h"`
  (harmonic mean) are no longer accepted, only the arithmetic mean is computed.
  For the geometric and harmonic mean, please use respective functions
  `geomean` and `harmmean` from the Octave Statistics package.

- `var` and `std` now optionally output a second argument containing the mean
  or weighted mean.

- `integral` can now accept the 'ArrayValued' option in combination with
  'RelTol' and 'WayPoints'.

- File system operations that remove files (e.g., `unlink` or `rmdir`) now also
  remove files that have their read-only file attribute set on Windows.

- The default state for certain graphics properties has been made
  consistent with Matlab.

        Object      | Property         | Default State
        ------------|------------------|------------
        figure      | "dockcontrols"   | "on"

- `ode45`, `ode23`, and `ode23s` have improved results for options `Events`,
  `OutputFcn`, and `Refine`, along with corrected orientation of struct
  outputs.

- *Early notice of future breaking changes*:  Due to many user requests that
  Octave should have a Matlab-compatible string class, there is work under way
  to implement a string class that will differ from a vector of characters.  In
  Octave, single-quoted character arrays are currently compatible with Matlab,
  but double-quoted forms are not.  Currently in Octave, both 'foo' and "foo"
  are largely interchangeable, barring certain escape sequence interpretations
  such as "\n" (converted to a single newline character) as opposed to '\n'
  (two separate characters).  Matlab’s single-quoted character arrays and
  double-quoted strings do not process backslash escape sequences, unlike many
  other languages, and those escape sequences are instead processed by
  individual functions such as `fprintf`.

  Octave's behavior is likely to change in future as a consequence of
  implementing Matlab-style string syntax.  For example, 'foo' will remain a
  three-element character vector, but "foo" will become a single-element string
  object.  Some backslash escape sequences are likely to differ from their
  current Octave behavior, but will achieve greater Matlab compatibility.  The
  exact implementation is a work in progress, and may or may not include
  methods of preserving backward compatibility.

  *What this means for user code*: If your code currently relies on
  double-quoted strings (e.g., "foo") representing character vectors as
  opposed to string objects, and if you intend to update to a future version of
  Octave incorporating the above changes, then consider replacing all
  double-quoted strings with single-quoted strings in your code over time
  (e.g., replace "foo" with 'foo'). Single-quoted strings are expected to
  retain current behavior.  Further, if your code relies on backslash escape
  sequence interpretation in double-quoted strings (except for special cases
  like the `printf` family), that code may need to change as well.

### Alphabetical list of new functions added in Octave 8

* `clearAllMemoizedCaches`
* `matlab.lang.MemoizedFunction`
* `memoize`
* `normalize`
* `pagectranspose`
* `pagetranspose`
* `uifigure`

### Deprecated functions, properties, and operators

The following functions and properties have been deprecated in Octave 8
and will be removed from Octave 10 (or whatever version is the second
major release after 8):

- Functions

        Function               | Replacement
        -----------------------|------------------
        shift                  | circshift
        sparse_auto_mutate     | none (see below)

- The `sparse_auto_mutate` function no longer has any effect on Octave's
  behavior.  Previously, after calling `sparse_auto_mutate (true)`,
  Octave would automatically convert sparse matrices to full when a
  sparse matrix required more memory than simply using full matrix
  storage.  This setting was `false` by default for compatibility with
  Matlab.  Now you must manually convert to full storage when desired.

- Properties

  The following property names are discouraged, but there is no fixed
  date for their removal.

        Object           | Property    | Replacement
        -----------------|-------------|------------
        uimenu           | label       | text
        uimenu           | callback    | menuselectedfcn

The following functions were deprecated in Octave 6 and have been removed
from Octave 8.

- Functions

        Function     | Replacement
        -------------|------------
        runtests     | oruntests

- The environment variable used by `mkoctfile` for linker flags is now
  `LDFLAGS` rather than `LFLAGS`.  `LFLAGS` was deprecated in Octave 6,
  and will be removed in a future version of Octave.

Summary of bugs fixed for version 8.1.0 (2023-01-22):
----------------------------------------------------

- Improved input validation and/or output handling for `poly`, `pinv`, `patch`,
  `fill`, `fill3`, `qp`, `datevec`, `textscan`, `sub2ind`, `qr`, `airy`,
  `regexp`, `dec2bin`, `dec2hex`, `mean`, and many others.
- Improved performance for `complex`, `fftw`, `delaunayn`, `isfield`, `tsearch`,
  sparse matrix exponentiation, other sparse operations, and many others.
- Overhauled `@audiorecorder` and `@audioplayer` classes.

### Old release news

- [Octave 7.x](etc/NEWS.7)
- [Octave 6.x](etc/NEWS.6)
- [Octave 5.x](etc/NEWS.5)
- [Octave 4.x](etc/NEWS.4)
- [Octave 3.x](etc/NEWS.3)
- [Octave 2.x](etc/NEWS.2)
- [Octave 1.x](etc/NEWS.1)
