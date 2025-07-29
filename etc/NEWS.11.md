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
  * There is a new `pkg search` functionality.  Running `pkg search foo bar baz`
    searches Octave Packages online for packages having all those keywords
    in their descriptions.  Search terms can also be regular expressions.
  * The old command `pkg list -forge`, which returned a list of packages found
    online, has been replaced by `pkg search -all`.  Calling `pkg list -forge`
    now gives a warning and then executes `pkg search -all`.  The warning can
    be silenced with the ID `"Octave:pkg:list-forge"`.

- Convolution of short and wide arrays (especially row vectors) is now faster
  by anywhere from 10% to 150X, depending on the array size and shape.
  Previously, convolution of tall and skinny arrays (especially column vectors)
  was much faster than convolving short and wide ones, requiring transposition
  or permutation of the inputs for performance.  Now Octave automatically picks
  the fastest calculation order irrespective of the input orientation.

- The `roots` function now accepts only a numeric argument.  Convert any
  non-numeric input to a numeric value with `double()`.

### Graphical User Interface

- The GUI now uses scalable SVG icons for beautiful display at any size.

### Graphics backend

- `polar` plot background patch object has been moved to the Z=0 plane to
  permit view changes that previously resulted in an obscured plot area
  (bug #67574).

### Matlab compatibility

- The warning `"Octave:language-extension"` is no longer emitted for automatic
  broadcasting which Matlab has supported since version 2016B.

- The function `zscore` now accepts the optional arguments `"all"` or `vecdim`,
  and `"omitnan"`.

- The function `gradient` with a numeric input now only accepts type double
  or single.

- The function `polyfit` now returns the highest powers possible and the
  constant term when there is insufficient data to create all N terms of the
  polynomial.  The output structure has a new field `rsquared` which is the
  coefficient of determination describing how well the polynomial fits the
  data.

### Alphabetical list of new functions added in Octave 11

* `corrcov`

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
