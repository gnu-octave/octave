Summary of important user-visible changes for version 9 (yyyy-mm-dd):
---------------------------------------------------------------------

### General improvements

- Octave now has an experimental virtual machine (VM) for just-in-time (JIT)
compilation of m-code.  This VM can be enabled by the end user with the
command `__enable_vm_eval__ (1)` as long as the configuration option
`--disable-vm-evaluator` was *not* used when building Octave.  Speedups from 2X
to 40X have been observed for different kinds of m-code.  This feature is
considered experimental for now.  M-code that cannot be handled by the VM yet
falls back automatically to the existing interpreter.

- `oruntests`: The current directory now changes to the directory
containing the files with the tests for the duration of the test.  This
aligns the behavior of this function with Octave's test suite.  This also
means that the file encoding specified in the `.oct-config` file for the
respective directory is taken into account for the tests.

- `dec2base`, `dec2bin`, and `dec2hex` have all been overhauled.  All three
functions now accommodate negative inputs and fractional inputs, and repeated
code between the functions has been reduced or eliminated.  Previously only
`dec2bin` and `dec2hex` accepted negative inputs but `dec2base` did not, and
none of the three accepted fractional inputs.  But now,
`dec2base (100*pi, 16, 4, 6)` for exampele returns a base-16 string with four
places for the integer part and six places for the fractional part.  Omitting
the number of decimal places (the fourth input) retains old behavior for
backward compatibility, except that non-integer inputs will no longer error.

- `quiver` and `quiver3` now properly plot non-float numeric inputs by
internally recasting them to 'double' (bug #59695).  Both functions now
honor a previously ignored scaling factor input when there is only a
single arrow (bug #39552), and `quiver3` no longer produces an error when a
scaling factor is input without x and y inputs. Both functions now also
accept a scale factor of "off" which is equivalent to setting it to 0.
When a linestyle with a base marker is set suppressing arrowhead display,
subsequent name-value property pairs in the quiver/quiver3 function call
will no longer turn arrowhead display back on (bug #64143).  The linewdith
property now also affect the line width of the base marker.

- The `inputParser` function has been re-architected for a 60% performance
improvement.

- The `perms` function has been made faster.

- The `audiowrite` function now supports writing to MPEG audio formats --
including MP3 -- if the `sndfile` library supports it.

### Graphical User Interface

### Graphics backend

* The FLTK backend is not maintained and its use is discouraged.  The
recommended backend is qt.  Enabling the fltk backend with
`graphics_toolkit fltk` now emits a warning.

* The `set` function now accepts any combination of name/value pairs,
cell array of names / cell array of values, or property structures.
This change is Matlab-compatible.

* When the `hold` function is used without arguments to toggle the current
state the resulting value is now displayed in the Command Window for
informational purposes.

* The axes graphics property "TickDir" now accepts the option "none" which
will not draw tick marks, but will still draw tick labels.

### Matlab compatibility

- The `inputParser` function now implements the `PartialMatching` property
for `Parameter` and `Switch` names.  For Matlab compatibility,
`PartialMatching` is now set to true by default which may change the behavior
of existing code.

- Overhauled `mean`, `median`, `var`, and `std` functions have been imported
from statistics package v1.5.4 to compatibly implement 'nanflag' (bug #50571),
'all' (bug #58116), and 'vecdim' (bug #58089) options, preserve output class,
and match expected output behavior for empty (bug #50583) and sparse inputs
(bug #63291).  Both `median` and `mean` can handle large int values without
overflow or precision concerns (bug #54567), and `mean` avoids errors due to
limits of single precision by processing as doubles (bug #63848).  `median`
has also adopted the 'outtype' option from `mean`.

- Code such as `A = ones (3, 3); A(:, :, 1) = []` is now Matlab-compatible.

- `mad` function now produces Matlab compatible output using improved `mean`
and `median` functions.  'vecdim' and 'all' options are now supported.  `mad`
ignores all NaN values (using 'omitnan' mean/median option) and produces
expected output behavior for empty inputs.

- `mode` now produces Matlab compatible output for empty inputs (bug #50583).

- `linspace` and `logspace` now handle `inf` inputs in a Matlab-compatible way.

- `normalize` now produces Matlab compatible output for inputs containing NaN
values (bug #50571).

- `cov` now processes the input form cov(x,y) with two separate data arrays
x and y, as cov(x(:),y(:)) to maintain Matlab compatibility.  It also accepts
a NANFLAG option to allow ignoring NaN entries in input data (bug #50571)
and produces Matlab compatible outputs for empty inputs (bug #50583).  `corr`
and `corrcoef` internal code has been adapted to the new `cov` behavior with
no change to user interface.  Packages using the octave core's `cov` that rely
on the previous calling form may need to make similar adaptations.  Calls for
cov(x) with a vector x expecting a scalar return can get the previous results
from `cov(x)(2)`, and calls to cov(x,y) with x and y matrices expecting
columnwise covariance calculation can obtain the previous results using
`cov([x,y])(1:nx, nx+1:end)`, where nx = columns(x).

- `qz` now handles input and output arguments in a Matlab compatible
way. It always computes the complex QZ decomposition if there are only
two input arguments, and it accepts an optional third input argument
'real' or 'complex' to select the type of decomposition. The output
arguments are always in the same order independently of the input
arguments, and the generalized eigenvalues are no longer returned (one
can use the `ordeig` function for that purpose). The optional reordering
of the generalized eigenvalues has been removed (one can use the `ordqz`
function for that purpose).

- `pcolor` now sets the axes limits to be just large enough to display the
plotted data (equivalent of `axis tight`).

- `xlim`, `ylim`, `zlim` functions can now query or set the limit calculation
method which is one of "tickaligned", "tight", or "padded".

- `optimget` and `optimset` now error on an ambiguous match of an incomplete
option name instead of emitting a warning.

### Alphabetical list of new functions added in Octave 9

* `isenv`
* `ismembertol`
* `isuniform`
* `tensorprod`

### Deprecated functions, properties, and operators

The following functions and properties have been deprecated in Octave 9
and will be removed from Octave 11 (or whatever version is the second
major release after 9):

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
Any uses can simply be removed from existing code with no loss of function.

    * The `all_ok(const Array<octave::idx_vector>&)` function in `Array-util.h`
is obsolete and always returns true.  Any uses can simply be removed from
existing code with no loss of function.

    * The member variable `octave_base_value::count` is deprecated and will be
removed from Octave 11.  Replace all instances with the new name `m_count`.

The following features were deprecated in Octave 7 and have been removed
from Octave 9.

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

    * The use of `'...'` for line continuations *inside* double-quoted
    strings has been removed.  Use `'\'` for line continuations inside strings
    instead.

    * The use of `'\'` as a line continuation *outside* of double-quoted
    strings has been removed.  Use `'...'` for line continuations instead.

    * Support for trailing whitespace after a `'\'` line continuation has been
    removed.  Delete unnecessary trailing whitespace.

- For plot functions, the use of numbers to select line colors in
  shorthand formats was an undocumented feature that was removed in Octave 9.

- The environment variable used by `mkoctfile` for linker flags is now
  `LDFLAGS` rather than `LFLAGS`.  `LFLAGS` was deprecated in Octave 6
  and has been removed.

Summary of bugs fixed for version 9.1.0 (yyyy-mm-dd):
----------------------------------------------------

- Bugfixes to `whos -file`, `urlread`, `mat2cell`, `set`.

- Better input validation for `sparse`, `speye`.

- Memory usage reduced for `movfun` and `movslice`.

- Memory usage reduced when saving to file, preventing OOM and data loss.

- Memory usage improved when plotting grid tick marks.

- Text encoding for non-UTF-8 generally made more robust.  File editor now
  lists available encodings.

- Several race conditions removed in signal handler.

- Performance improvements: avoid unnecessary string construction, use
  static casts instead of dynamic casts where possible.

### Old release news

- [Octave 8.x](etc/NEWS.8)
- [Octave 7.x](etc/NEWS.7)
- [Octave 6.x](etc/NEWS.6)
- [Octave 5.x](etc/NEWS.5)
- [Octave 4.x](etc/NEWS.4)
- [Octave 3.x](etc/NEWS.3)
- [Octave 2.x](etc/NEWS.2)
- [Octave 1.x](etc/NEWS.1)
