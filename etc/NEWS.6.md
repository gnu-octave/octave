Summary of bugs fixed for version 6.4.0 (2021-10-30)
----------------------------------------------------

For (bug #XXXXX) see https://savannah.gnu.org/bugs/?XXXXX

### Improvements and fixes

- Reduce memory usage in BISTs for `copyobj`, `hgsave` (bug #57591).
- `hgsave.m`, `copyobj.m`: Use `'qt'` graphics toolkit in BISTs.
- `main.cc`: Use `getopt` to parse command line arguments (bug #60886).
- `main.cc`: Remove invalid case (bug #60886).
- Disable `getopt` error reporting in wrapper program (bug #60886).
- `interp1.m`: Don't interpret later numeric input as `xi` (bug #60967).
- `pkg`: Improve similar package name suggestion (bug #61067).
- Store parent name in function object when caching parents in scope (bug #61105).
- Avoid internal error and segfault with `eval` and scripts (bug #61191).
- `rmpath`: Prevent removing the current directory from the load path (bug #61216).

### GUI

- Fix missing interpreter event in `octave-scintilla`.
- Fix opening a file in a custom editor (bug #60990).

### Documentation

- Improve docstring for `disable_diagonal_matrix`, `disable_diagonal_matrix`,
  and `disable_range` (patch #10089).
- `cbrt`: Clarify that function errors for non-real input.
- `dsearchn.m`: Added optional distance output description (bug #61156).
- Add Hungarian translation for project description files.
- Document `fsolve` output `"info"` -2 (bug #61310).

### Build system

- Correct error message for incompatible CXSparse (bug #61385).


Summary of bugs fixed for version 6.3.0 (2021-07-11)
----------------------------------------------------

For (bug #XXXXX) see https://savannah.gnu.org/bugs/?XXXXX

### Important notice

- This bug fix release breaks ABI compatiblity with Octave 6.2.0. Re-build
  binaries (like .oct or .mex files) when updating to this version.

### Improvements and fixes

- `ls-hdf5.cc`: Avoid throwing inside HDF5 function (bug #60081).
- `ls-hdf5.cc`: Handle non-zero terminated type strings (bug #60081).
- Fix occasional segfault in `symbfact` (bug #60101).
- `fsolve.m`: Fix undefined output error when using `Output` function (bug #60144).
- Fix compilation error with `iconv_t` on Solaris (bug #60162).
- build: Check for `stropts.h` (bug #60163).
- Avoid ambiguous call to `pow` (bug #60169).
- Fix context link when creating handle to nested function.
- `print.m`: Warn when figure is too large to be printed to output page (bug #60236).
- Defer clearing function vars until stack frame is deleted (bug #60137).
- Avoid memory leaks when returning handles to nested functions.
- Hold references to closure frames in anon functions if needed (bug #60237).
- `eigs`: Prevent possible segmentation fault at exit (bug #60311).
- Issue warning when gnuplot graphics toolkit is initialized.
- `mpoles.m`: Fix detection of pole multiplicity (bug #60384).
- Perform shutdown actions in interpreter destructor (bug #60334).
- build: Make relocation of dependencies with Octave optional (bug #60413).
- `qz.cc`: Return correct number of eigenvalues (bug #60357).
- `qz.cc`: Let test pass with LAPACK 3.9.1 and earlier versions (bug #60357).
- `pkg.m`: Use default prefixes unless otherwise set previously (bug #60472).
- `betaincinv.m`: Correctly handle small inputs (bug #60528).
- `betaincinv.m`: Correctly handle inputs very close to 1.0 (bug #60528).
- `unistd-wrappers.c`: Allocate sufficient memory for `new_argv` (bug #60535).
- Mark system functions correctly if `OCTAVE_HOME` is non-canonical (bug #60554).
- Mark compiled system functions correctly if `OCTAVE_HOME` is non-canonical (bug #60554).
- Fix error if test suite is run before Octave is installed (bug #60554).
- `lo-array-errwarn.cc`: Include `<limits>` (bug #60470).
- Use `std::size_t` and `std::ptrdiff_t` in C++ code (bug #60471).
- Use `std::size_t` in more instances (bug #60471).
- Return proper number of stack frames for `dbstack (N)` call (bug #60531).
- Avoid ambiguous match of overloaded function (bug #60531).
- `lscov.m`: Relax BIST tolerance to pass with OpenBLAS (bug #60552).
- `print`: Fix error when `"px"` word is present in a figure (bug #60641).
- `logm.m`: Fix check for real negative values in complex vector (bug #60738).
- build: Set necessary flags to allow execution on Windows Vista (bug #60746).
- Declare base_parser destructor virtual.
- `hist.m`: Improve handling and docstring for third parameter `"norm"` (bug #60783).
- `logm.m`: Allow tolerance in check for real negative values in complex vector (bug #60738).
- `expm.m`, `logm.m`: Use function `isdiag` to detect if input is a diagonal matrix (bug #60738).
- tests: Relax tolerance for some tests on macOS.
- `logspace.m`: Mark tests as known to fail on macOS (bug #55538).
- `hist.m`: Use deterministic test (bug #60783).
- `rgb2ind.m`: Reduce memory usage and eliminate randomness in test.
- `logm.m`: Allow larger tolerance for test on macOS.
- build: Use correct path to `octave` binary in build tree on macOS.
- build: Fix typo in folder to libraries when building `.oct` or `.mex` files.
- build: Set `DL_LDFLAGS` in the build rules for `.oct` or `.mex` files.
- `rgb2ind.m`: Suppress output in test.
- Improve documentation for `log2` function (bug #60817).
- `ind2sub`: Fix typo in "see also" section of docstring (bug #60842).
- `mrdivide`, `mldivide`: Document that functions might return minimum norm solutions (bug #60839).
- Fix scoping issue for handles to sibling nested functions (bug #60845).
- `ls-mat5.cc`: Avoid integer overflow in calculation of buffer size for zlib (bug #55427).
- Move top-level REPL from interpreter to evaluator.
- Avoid crash with `dbquit` when executing command in terminal from GUI (bug #60813).

### GUI

- Fix calling external editor (bug #60198).
- Fix missing file suffix .m when saving a new script (bug #60214).
- Do not run files that are not saved as Octave files (bug #60214).
- Fix confirm overwrite for native editor file "save as" dialogs (bug #60214).
- Fix crash when GUI tries to restore missing previous Octave dir (bug #60255).
- Fix restoring the horizontal position of docked GUI widgets (bug #59426).
- Prevent floating widgets from re-opening after restart (bug #60279).
- Avoid crash in GUI for `rmdir("")` (bug #60473).
- Fix EOL mode when saving files under new names (bug #60585).
- Fix auto indentation of switch-structure in GUI editor (bug #60649).
- Avoid crash when closing GUI with open editor tabs (bug #60500).
- `octave-qscintilla.cc` (`contextmenu_run`): Fix keyboard command handling.

### Documentation

- Improve Differential Equations chapter and example for `lsode` (bug #57508).
- Clarify usage of `"Depends"` keyword in package `DESCRIPTION` file (bug #60053).
- Add note that wildcard patterns for `save` are glob patterns (bug #60157).
- Change example for Delaunay triangulation to match the generating code (bug #60216).
- Document single precision issues with OpenGL graphics toolkits (bug #59418).
- Minor changes to documentation of single precision issues with OpenGL (bug #59418).
- Expand on documentation for command syntax (bug #60084).
- `isprop.m`: Document that function only works on graphics objects in Octave 6.X (bug #60295).
- Explain how to write dual-purpose executable scripts and Octave functions (bug #60291).
- Update keyword docstrings (bug #60275).
- Use Texinfo commands to improve `transpose()` docstring rendering.
- `betainc.m`, `betaincinv.m`: Correct non-TeX definition of beta incomplete integral.
- Grammarcheck documentation ahead of 6.3 release.
- Spellcheck documentation ahead of 6.3 release.


Summary of bugs fixed for version 6.2.0 (2021-02-19):
----------------------------------------------------

See: https://www.octave.org/news/release/2021/02/20/octave-6.2.0-released.html

For (bug #XXXXX) see https://savannah.gnu.org/bugs/?XXXXX

### Improvements

- `bicgstab.m`, `cgs.m`: Fix typo in `"iter_min"` variable name (bug #60071).
- Compute with `NA` correctly on MIPS architecture (bug #59830).
- Fix lookup of `"caller"` stack frame (bug #59847).
- Also wait on `main_thread` after interpreter shuts down (bug #56952).
- Fix symbol lookup issue with anonymous functions (bug #55989).
- Line buffer input in `terminal_reader` class.
- `qr`: Error for dense `A` and `B` with three output arguments (bug #58944).
- `strmatch.m`: Always return column vector for Matlab compatibility (bug #59917)
- Avoid crash when `evalin` global variables into existence in script (bug #59937)
- Avoid crash on null statement list (bug #59938).
- Fix ignored output from user function in left side of assignment (bug #59704).
- Temporarily set lvalue list to null (bug #59704).
- `fminbnd.m`: do not ignore `"OutputFcn"` (bug #59901).
- `load-path.cc`: Reduce number of times `"canonicalize_file_name"` is called (bug #59711).
- `interpn.m`: Use `size_equal` for 10X speedup in cset 067b663529bb (bug #59856).
- `interpn.m`: Fix check for scattered point coordinates (bug #59856).
- Avoid `YYUSE` in Octave parser files (see bug #59806).
- `struct2hdl.m`: Set `"units"` property early.
- `load-path.cc`: Avoid copying string for loop variable.
- `pcg.m`: Return correct `FLAG` and correct `RELRES` output (bug #59776).
- Use static keyword on regexp pattern in `file_stat` (bug #59706).
- `stat`: Improve regular expression for UNC roots on Windows (bug #59706).
- `stat`: Use `"make_absolute"` instead of `"canonicalize_file_name"` on Windows (bug #59706).
- Improve `class_simple` function handle function lookup (bug #59661).
- `hdl2struct.m`: store hidden text properties (bug #57241).
- Mark script created with commands from history as modified.
- `replem.m`: Fix operations with sparse matrices (bug #59705).
- `ode_event_handler.m`: Fix mishandling of event edge types and multiple events (bug #59709).
- Increase size of dynamic variable `new_argv` by 1 to avoid indexing out of array.
- Fix incorrect results for set functions with `"legacy"` option (bug #59708).
- `dir.m`: Return folder (not including file) in field `"folder"` (bug #59689).
- Avoid memory leak with function handles (bug #59659).
- Avoid dispatch error if method argument is a function handle (bug #59617).
- Avoid crash due to accessing first element of empty list (bug #59656).
- Don't propagate prevailing `isargout` info through `mexCallMATLAB` (bug #59597).
- Show original error when failing to create a graphics object (bug #59620).
- Fix regression with superclass lookup in classdef constructors (bug #59602).
- Allow Octave class `execution_exception` to catch `std::exception` objects (bug #59592).

### GUI

- Fix restoring editor session after having closed all tabs (bug #60051).
- Maybe convert TAB to SPC in GUI terminal pasted text (bug #59916).
- Make bracketed paste mode work in GUI terminal.
- Fix regression in variable editor when printing without selection.
- Avoid gui when octave is launched in non-interactive mode (bug #59628).
- `file-editor-tab.cc` (dtor): do not delete `m_edit_area` (bug #59628).
- Fix error when restoring previous main window layout (bug #59426).
- Improve default sizes of gui dock widgets.
- Clean up constructing main window layout of the gui.
- Fix focus command window after command execution (bug #59609).
- Check object size before plotting from variable editor (bug #56685).
- `documentation.cc`: Include missing header (bug #59553).

### Build system / Tests

- Add default value to `OCTAVE_MIPS_NAN` configure macro for cross-compiling (bug #59830).
- tests: Function name should match file name (bug #59704).
- Avoid build errors with Qt4 (bug #59813).
- `eigs.m`: Make tests that depend on CHOLMOD conditional.
- tests: Make tests that depend on CXSparse conditional.
- build: Use `SPARSE_XCPPFLAGS` in `CPP_FLAGS` for `libcorefcn` (bug #59806).
- Add test case for bug #59661.
- `hgsave.m`: Allow test to run with qt or gnuplot graphics toolkits (bug #57241).

### Documentation

- `embedded.cc`: Fix syntax error interpreter shutdown.
- Update Octave Project Developers copyright for the new year.
- Use the same comment style for copyright headers in `.m` files and shell scripts.


Summary of important user-visible changes for version 6.1.0 (2020-11-26):
------------------------------------------------------------------------

### General improvements

- The `intersect`, `setdiff`, `setxor`, `union`, and `unique` functions
  accept a new sorting option `"stable"` which will return output values
  in the same order as the input, rather than in ascending order.

- Complex RESTful web services can now be accessed by the `webread` and
  `webwrite` functions alongside with the `weboptions` structure.  One
  major feature is the support for cookies to enable RESTful
  communication with the web service.

  Additionally, the system web browser can be opened by the `web`
  function.

- The `linspace` function now produces symmetrical sequences when the
  endpoints are symmetric.  This is more intuitive and also compatible
  with recent changes made in Matlab R2019b.

- The underlying algorithm of the `rand` function has been changed.
  For single precision outputs, the algorithm has been fixed so that it
  produces values strictly in the range (0, 1).  Previously, it could
  occasionally generate the right endpoint value of 1 (See bug #41742).
  In addition, the new implementation uses a uniform interval between
  floating point values in the range (0, 1) rather than targeting a
  uniform density (# of random integers / length along real number
  line).

- Numerical integration has been improved.  The `quadv` function has
  been re-written so that it can compute integrands of periodic
  functions.  At the same time, performance is better with ~3.5X fewer
  function evaluations required.  A bug in `quadgk` that caused complex
  path integrals specified with `"Waypoints"` to occasionally be
  calculated in the opposite direction was fixed.

- The `edit` function option `"editinplace"` now defaults to `true` and
  the option `"home"` now defaults to the empty matrix `[]`.  Files will
  no longer be copied to the user's `HOME` directory for editing.  The old
  behavior can be restored by setting `"editinplace"` to `false` and
  `"home"` to `"~/octave"`.

- The `format` command supports two new options: `uppercase` and
  `lowercase` (default).  With the default, print a lowercase `'e'` for
  the exponent character in scientific notation and lowercase `'a-f'` for
  the hex digits representing 10-15.  With `uppercase`, print `'E'` and
  `'A-F'` instead.  The previous uppercase formats, `E` and `G`, no longer
  control the case of the output.

  Additionally, the `format` command can be called with multiple options
  for controlling the format, spacing, and case in arbitrary order.
  For example:

        format long e uppercase loose

  Note, in the case of multiple competing format options the rightmost
  one is used, and, in case of an error, the previous format remains
  unchanged.

- L-value references (e.g., increment (`++`), decrement (`--`), and all
  in-place assignment operators (`+=`, `-=`, `*=`, `/=`, etc.)) are no longer
  allowed in anonymous functions.

- New warnings have been added about questionable uses of the colon `':'`
  range operator.  Each has a new warning ID so that it can be disabled
  if desired.

  >  `Octave:colon-complex-argument`   : when any arg is complex<br>
  >  `Octave:colon-nonscalar-argument` : when any arg is non-scalar

- The `regexp` and related functions now correctly handle and *require*
  strings in UTF-8 encoding.  As with any other function that requires
  strings to be encoded in Octave's native encoding, you can use
  `native2unicode` to convert from your preferred locale.  For example,
  the copyright symbol in UTF-8 is `native2unicode (169, "latin1")`.

- The startup file `octaverc` can now be located in the platform
  dependent location for user local configuration files (e.g.,
  `${XDG_CONFIG_HOME}/octave/octaverc` on Unix-like operating systems or
  `%APPDATA%\octave\octaverc` on Windows).

- `pkg describe` now lists dependencies and inverse dependencies
  (i.e., other installed packages that depend on the package in
  question).

- `pkg test` now tests all functions in a package.

- When unloading a package, `pkg` now checks if any remaining loaded
  packages depend on the one to be removed.  If this is the case `pkg`
  aborts with an explanatory error message.  This behavior can be
  overridden with the `-nodeps` option.

- The command

        dbstop in CLASS at METHOD

  now works to set breakpoints in classdef constructors and methods.

#### Graphics backend

- The use of Qt4 for graphics and the GUI is deprecated in Octave
  version 6 and no further bug fixes will be made.  Qt4 support will be
  removed completely in Octave version 7.

- The `legend` function has been entirely rewritten.  This fixes a
  number of historical bugs, and also implements new properties such as
  `"AutoUpdate"` and `"NumColumns"`.  The gnuplot toolkit---which is no
  longer actively maintained---still uses the old legend function.

- The `axis` function was updated which resolved 10 bugs affecting
  axes to which `"equal"` had been applied.

- Graphic primitives now accept a color property value of `"none"`
  which is useful when a particular primitive needs to be hidden
  (for example, the Y-axis of an axes object with `"ycolor" = "none"`)
  without hiding the entire primitive `"visibility" = "off"`.

- A new property `"FontSmoothing"` has been added to text and axes
  objects that controls whether anti-aliasing is used during the
  rendering of characters.  The default is `"on"` which produces smooth,
  more visually appealing text.

- The figure property `"windowscrollwheelfcn"`is now implemented.
  This makes it possible to provide a callback function to be executed
  when users manipulate the mouse wheel on a given figure.

- The figure properties `"pointer"`, `"pointershapecdata"`, and
  `"pointershapehotspot"` are now implemented.  This makes it possible
  to change the shape of the cursor (pointer in Matlab-speak) displayed
  in a plot window.

- The figure property `"paperpositionmode"` now has the default `"auto"`
  rather than `"manual"`.  This change is more intuitive and is
  Matlab compatible.

- The appearance of patterned lines `"LineStyle" = ":"|"--"|"-."` has
  been improved for small widths (`"LineWidth"` less than 1.5 pixels)
  which is a common scenario.

- Printing to EPS files now uses a tight bounding box (`"-tight"`
  argument to print) by default.  This makes more sense for EPS
  files which are normally embedded within other documents, and is
  Matlab compatible.  If necessary use the `"-loose"` option to
  reproduce figures as they appeared in previous versions of Octave.

- The following print devices are no longer officially supported: cdr,
  corel, aifm, ill, cgm, hpgl, mf and dxf.  A warning will be thrown
  when using those devices, and the code for supporting those formats
  will eventually be removed from a future version of Octave.

- The placement of text subscripts and superscripts has been
  re-engineered and now produces visually attractive results similar to
  Latex.

### Matlab compatibility

- The function `unique` now returns column index vectors for the second
  and third outputs.  When duplicate values are present, the default
  index to return is now the `"first"` occurrence.  The previous Octave
  behavior, or Matlab behavior from releases prior to R2012b, can be
  obtained by using the `"legacy"` flag.

- The function `setdiff` with the `"rows"` argument now returns Matlab
  compatible results.  The previous Octave behavior, or Matlab behavior
  from releases prior to R2012b, can be obtained by using the `"legacy"`
  flag.

- The functions `intersect`, `setxor`, and `union` now accept a
  `"legacy"` flag which changes the index values (second and third
  outputs) as well as the orientation of all outputs to match Matlab
  releases prior to R2012b.

- The function `streamtube` is Matlab compatible and plots tubes along
  streamlines which are scaled by the vector field divergence. The
  Octave-only extension `ostreamtube` can be used to visualize the flow
  expansion and contraction of the vector field due to the local
  crossflow divergence.

- The interpreter now supports handles to nested functions.

- The graphics properties `"LineWidth"` and `"MarkerSize"` are now
  measured in points, *not* pixels.  Compared to previous versions
  of Octave, some lines and markers will appear 4/3 larger.

- The meta.class property "SuperClassList" has been renamed
  "Superclasslist" for Matlab compatibility.  The original name will
  exist as an alias until Octave version 8.1.

- Inline functions created by the function `inline` are now of type
  `"inline"` when interrogated with the `class` function.  In previous
  versions of Octave, the class returned was `"function_handle"`.  This
  change is Matlab compatible.  Inline functions are deprecated in
  both Matlab and Octave and support may eventually be removed.
  Anonymous functions can be used to replace all instances of inline
  functions.

- The function `javaaddpath` now prepends new directories to the
  existing dynamic classpath by default.  To append them instead, use
  the new `"-end"` argument.  Multiple directories may now be specified
  in a cell array of strings.

- An undocumented function `gui_mainfcn` has been added, for compatibility
  with figures created with Matlab's GUIDE.

- Several validator functions of type `mustBe*` have been added.  See
  the list of new functions below.

### Alphabetical list of new functions added in Octave 6

* `auto_repeat_debug_command`
* `commandhistory`
* `commandwindow`
* `filebrowser`
* `is_same_file`
* `lightangle`
* `mustBeFinite`
* `mustBeGreaterThan`
* `mustBeGreaterThanOrEqual`
* `mustBeInteger`
* `mustBeLessThan`
* `mustBeLessThanOrEqual`
* `mustBeMember`
* `mustBeNegative`
* `mustBeNonempty`
* `mustBeNonNan`
* `mustBeNonnegative`
* `mustBeNonpositive`
* `mustBeNonsparse`
* `mustBeNonzero`
* `mustBeNumeric`
* `mustBeNumericOrLogical`
* `mustBePositive`
* `mustBeReal`
* `namedargs2cell`
* `newline`
* `ode23s`
* `ostreamtube`
* `rescale`
* `rotx`
* `roty`
* `rotz`
* `stream2`
* `stream3`
* `streamline`
* `streamtube`
* `uisetfont`
* `verLessThan`
* `web`
* `weboptions`
* `webread`
* `webwrite`
* `workspace`


### Deprecated functions and properties

The following functions and properties have been deprecated in Octave 6
and will be removed from Octave 8 (or whatever version is the second
major release after 6):

- Functions

        Function         | Replacement
        -----------------|------------------
        runtests         | oruntests

- Properties

        Object           | Property      | Value
        -----------------|---------------|------------
                         |               |

- The environment variable used by `mkoctfile` for linker flags is now
  `LDFLAGS` rather than `LFLAGS`.  `LFLAGS` is deprecated, and a warning
  is emitted if it is used, but it will continue to work.


### Removed functions and properties

The following functions and properties were deprecated in Octave 4.4
and have been removed from Octave 6.

- Functions

        Function         | Replacement
        -----------------|------------------
        chop             | sprintf for visual results
        desktop          | isguirunning
        tmpnam           | tempname
        toascii          | double
        java2mat         | __java2mat__

- Properties

        Object           | Property                  | Value
        -----------------|---------------------------|-----------------------
        annotation       | edgecolor ("rectangle")   |
        axes             | drawmode                  |
        figure           | doublebuffer              |
                         | mincolormap               |
                         | wvisual                   |
                         | wvisualmode               |
                         | xdisplay                  |
                         | xvisual                   |
                         | xvisualmode               |
        line             | interpreter               |
        patch            | interpreter               |
        surface          | interpreter               |
        text             | fontweight                | "demi" and "light"
        uibuttongroup    | fontweight                | "demi" and "light"
        uicontrol        | fontweight                | "demi" and "light"
        uipanel          | fontweight                | "demi" and "light"
        uitable          | fontweight                | "demi" and "light"


### Old release news

- [Octave 5.x](etc/NEWS.5)
- [Octave 4.x](etc/NEWS.4)
- [Octave 3.x](etc/NEWS.3)
- [Octave 2.x](etc/NEWS.2)
- [Octave 1.x](etc/NEWS.1)
