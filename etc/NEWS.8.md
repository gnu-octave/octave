Summary of important user-visible changes for version 8 (yyyy-mm-dd):
---------------------------------------------------------------------

### General improvements

- Octave's libraries are now built using symbol visibility by default.
  That means that less symbols are exported from these libraries.
  Configure with `--disable-lib-visibility-flags` to export all symbols
  (as in previous versions).

- `filter` is now 5X faster, which also speeds up `deconv`, `fftfilt`
  and `arma_rnd` by the same amount.

- `integral` can now output a second argument passing the error
  measurement used by the underlying integrator.

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

- Octave is now compatible to PCRE2 (UTF-8). PCRE2 is preferred over PCRE
  if both are installed.  Configure with `--without-pcre2` if you prefer
  that Octave uses PCRE in this case.

### Graphical User Interface


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

### Old release news

- [Octave 7.x](etc/NEWS.7)
- [Octave 6.x](etc/NEWS.6)
- [Octave 5.x](etc/NEWS.5)
- [Octave 4.x](etc/NEWS.4)
- [Octave 3.x](etc/NEWS.3)
- [Octave 2.x](etc/NEWS.2)
- [Octave 1.x](etc/NEWS.1)
