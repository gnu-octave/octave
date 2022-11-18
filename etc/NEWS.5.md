Summary of bugs fixed for version 5.2.0 (2020-01-31):
----------------------------------------------------

See: https://www.octave.org/news/release/2020/01/31/octave-5.2.0-released.html

For (bug #XXXXX) see https://savannah.gnu.org/bugs/?XXXXX

### Improvements

- Lock amd and symbfact to avoid segmentation fault with SuiteSparse (bug #57435)
- Fix segfault when calculating patch vertex normals (bug #57353)
- Fix segfault in constructing patch normals for lighting (bug #57353)
- `mat2str.m`: Fix for logical matrix (add `transpose`).
- improve file equality check for MS Windows (bug #55161)
- `ismember.m`: Fix second return argument when values are complex (bug #52437)
- `findobj.m`: Fix typo and correctly handle numeric properties (bug #57030)
- Correctly display integer types with format hex (bug #56966)
- Cast `winqueryreg` `REG_DWORD` types to `DWORD` (bug #56703)
- Fix numerous errors in `audiowrite` / `audioread` (bug #56889)
- `print.m`: Pass quoted path to `octave-svgconvert` (bug #56768)
- `dir.m`: Fix occasionally returning incorrect folder field (bug #55448)
- Stop segfault when calling 3-input form of `diag` with cell arrays (bug #56711)
- improve message for setting breakpoint in nonexistent function (bug #56157)
- Use framebuffer object for printing invisible Qt figures on mac (bug #55268)
- Escape backslash characters in EPS output (bug #56448)
- Use replacement characters to display non UTF-8 strings in figures (bug #55974)
- `inputrc`: Add warning about modification (bug #56079)
- `documentation.cc` (global_search): just return if query string is empty (bug #56388)
- Default (c)transpose for old style class arrays (bug #56323)
- Always reserve at least 1 element of storage for sparse matrices (bug #56232)
- Don't segfault at exit after reading malformed HDF5 file (bug #56149)
- Fix `pause()` with no arguments called on Windows (bug #55943)
- refactor minimum eigenvalue index search in `qp` (bug #56037)
- qp: fix obscure corner case when calculating `qp` caused by a typo (bug #56037)
- use `std::streampos` instead of `std::ios::streampos` (patch #9782).
- fix `pause` and `kbhit` with glibc 2.28 end-of-file state behavior (bug #55029)
- Fix return of left-handed vectors when inputs are complex (bug #56026)
- `waitbar.m`: Fix hang when using `createcancelbtn` property (bug #55963)
- `shading.m`: Fix unexpected error when multiple `hggroups` present (bug #55993)
- `dec2base.m`: Correctly handle zero matrix input (bug #56005)
- eliminate duplicate graphics callback object stack (bug #55908)
- Improve performance when closing figures (bug #55908)
- Reduce time to process `pkg -forge list` by 30X (bug #39479)
- Feed `fig2dev` with PDF files converted from svg (bug #55906)
- Improve error checking for `iconv_open`.
- `poly.m`: fix the fix for input of complex conjugate pairs (bug #53897)
- `mkoctfile`: use the `TMPDIR` environment variable if set (bug #55814)
- `line.m`: Fix creation of unwanted axes (bug #55840)
- Accept 4-input form for `quiver3` (bug #55570)
- `pkg.m`: restore installing packages from zip archives (bug #55788)
- `axis.m`: Fix issues with `equal` argument (bug #55619, bug #55614, bug #55574,
  bug #55514, bug #54848, bug #53724, bug #53504, bug #53336, bug #51938, bug #50356).
- `ordeig.m`: Do not fail on 1x1 matrices (bug #55779)

### GUI

- fix editors search and replace in selection (bug #56405)
- undo a complete replace all action in the editor (bug #56405)
- fix missing unlock of mutex when gui editor is not present (bug #56097)
- prevent unnamed editor tab from being closed by `rmdir` (bug #55888)
- prevent editor from closing files that are not affected by `rmdir` (bug #55823)
- fix creating keyword files for editor autocompletion (bug #55835)
- fix dock widget position when being dragged out of the main window (bug #55704)

### Build system / Tests

- configure: prevent overlinking when SUNDIALS is disabled (bug #55956)
- use QtCore and Qt5Gui modules instead of Qt5OpenGL (bug #55735)
- make building with Qt4 QGLWidget work again (bug #55735)
- build: adapt `mk-qthelp.pl` to changes in Texinfo 6.7.
- BIST should not rely on the current directory to be writable.
- `bp-table.cc`: Fix test.
- maint: Make old style class tests Matlab compatible.
- Add BIST tests for `unlink` (bug #56097)

### Documentation

- fix many spelling errors (bug #57613)
- help: improve documentation for the `startup.m` user script.
- avoid conflict with `@seealso` macro in Texinfo 6.6 (bug #55952)
- `mk-doc-cache.pk`: Also translate `@seealso` here. (bug #55952)
- `__makeinfo__.m`: Also translate `@seealso` here (bug #55952)
- `polar.m`: Document that input is expected to be in radians (bug #57052)
- `gallery.m`: Fix typo in docstring for `poisson` (bug #56267)
- Add 1024x1024 Octave logo icon (bug #55836)
- add `.editorconfig` for default file encoding and indentation.
- `octave.doap`: Copy localized (short) description over from appdata (bug #55279)
- `org.octave.Octave.appdata.xml`: Add German and French description (bug #55279)
- `languages/*.ts`: updated language files (bug #55772)
- `quiver3.m`: Fix texinfo typo in docstring from cset f7b10bd40045.
- doc: fix `.` Operator Index entry to build with Texinfo 6.7.
- doc: improve example of using global variables from oct-files.
- doc: Create en-dashes and em-dashes correctly in documentation.
- doc: expand `page_output_immediately` doc string, mention `page_screen_output`.
- doc: Clarify documentation of `history_file` (bug #57027)
- doc: Improve documentation for `get_help_text`, `get_help_text_from_file`.
- doc: Add documentation for `GNUTERM` variable used with gnuplot (bug #56906)
- doc: Improve `contour` docstring example (bug #56849)
- doc: Add example to show limitations of the given BLAS integer size.
- doc: Improve documentation of sparse functions.
- doc: Improve spelling of `bicgstab` (bug #56812)
- doc: Remove `Map` functions `keys`, `values`, `remove` from unimplemented list (bug #56718)
- doc: Add function index entry for alias `inverse` (bug #56629)
- add content rating declaration to AppStream metadata (bug #56466)
- doc: Remove stray semicolons from `pie`, `pie3` calling forms.
- doc: Redo documentation for `rats` function.
- doc: Specify `position` property for text objects is a three-element vector (bug #56303)
- doc: state that Octave can only load HDF5 files created by itself (bug #56148)
- doc: Fix names of options in `issorted` (bug #56087)
- doc: Small fixes to `mat2cell` and `polyeig` docstrings (bug #55985)
- doc: Improve example code for using `-pdflatexstandalone` (bug #55945)
- doc: Refer to "root object" rather than "root figure object".
- doc: Place cross-reference from list of graphics object properties back to object (bug #46076)
- doc: document the newline character in warning messages (bug #49158)
- doc: pkg versions can be more general than "x.y.z" (bug #55798)


Summary of important user-visible changes for version 5 (2019-02-23):
--------------------------------------------------------------------

See: https://www.octave.org/NEWS-5.1.html

### General improvements

- The Octave plotting system now supports high resolution screens,
  i.e., those with greater than 96 DPI which are referred to as
  HiDPI/Retina monitors.

- Unicode character support for files and folders in Windows.

- A new core function `movfun` will apply a function to a sliding
  window of arbitrary size on a dataset and accumulate the results.
  Many common cases have been implemented using the naming
  scheme `movXXX` where `XXX` is the function that will be applied.
  For example, the moving average over a dataset is `movmean`.
  New moving window functions:

  `movfun`  `movslice`
  `movmad`  `movmax`   `movmean`  `movmedian`  `movmin`  `movprod`
  `movstd`  `movsum`   `movvar`

- The `fsolve` function has been tweaked to use larger step sizes when
  calculating the Jacobian of a function with finite differences.
  This leads to faster convergence.

- The `ranks` function has been recoded for performance and is now 25X
  faster.  In addition, it now supports a third argument that specifies
  how to resolve the ranking of tie values.

- The function `randi` has been recoded to produce an unbiased (all
  results are equally likely) sample of integers.  This may produce
  different results in existing code.  If it is necessary to reproduce
  the exact random integer sequence as in previous versions use

        ri = imin + floor ((imax - imin + 1) * rand ());

- The function `isdefinite` now returns `true` or `false` rather than
  `-1`, `0`, or `1`.  To test for a positive semi-definite matrix (old
  output of `0`) check whether the following two conditions hold:

    `isdefinite (A) => 0`  and  `isdefinite (A + 5*TOL, TOL) => 1`

- The `intmax`, `intmin`, and `flintmax` functions now accept a variable
  as input.  Existing code to query the range of an existing variable can
  be simplified by removing the call to `class` that was previously
  required.  For example defining the variable `x = int8 (3)` in the
  workspace, calls like

        range = [ intmin(class(x)), intmax(class(x)) ]

  can in Octave 5 be simplified to `range = [ intmin(x), intmax(x) ]`.

- The path handling functions no longer perform variable or brace
  expansion on path elements and Octave's load-path is no longer
  subject to these expansions.

- A new printing device is available, `"-ddumb"`, which produces ASCII
  art for plots.  This device is only available with the gnuplot toolkit.

- The `msgbox` function has changed in two respects: the default `WindowStyle`
  is now `"non-modal"`, and the default interpreter for the message is now
  `"tex"`.  Both `WindowStyle` and `Interpreter` can be controlled by passing
  an option struct argument.

### Dependencies

- The GUI requires Qt libraries.  The minimum Qt4 version supported is
  Qt4.8.  Qt5 of any version is preferred.

- The OSMesa library is no longer used.  To print invisible figures
  when using OpenGL graphics, the Qt `QOFFSCREENSURFACE` feature must be
  available and you must use the qt graphics toolkit.

- The FFTW library is now required to perform FFT calculations.
  The FFTPACK sources have been removed from Octave.


### Matlab compatibility

- The determination of an object's dimensions, size, and shape by the
  functions `ndims`, `rows`, `columns`, `isscalar`, `isvector`,
  `isrow`, `iscolumn`, `ismatrix`, and `issquare` now fully depends
  on the function size.  Thus, any user-defined object can ensure correct
  treatment by the aforementioned functions by properly overloading the
  `size` function.

- The functions `issymmetric` and `ishermitian` accept an option
  `"nonskew"` or `"skew"` to calculate the symmetric or skew-symmetric
  property of a matrix.  Performance has also been increased.

- The `issorted` function now uses a direction option of `"ascend"`
  or `"descend"`.  Change all uses of `"ascending"` and `"descending"`
  in existing code to the new options.

- The `strncmp` and `strncmpi` functions now return `true` if the two
  input strings match, even though the number of characters specified
  by `n` exceeds the string length.  For Example:

    `strncmp ("abc", "abc", 100)`

  returns `true` in Octave 5 and `false` in older versions of Octave.

- The `str2func` function no longer accepts a second `"global"` argument.
  This argument was typically used to allow functions that accept
  function names as arguments to avoid conflicts with subfunctions or
  nested functions.  Instead, it's best to avoid this situation
  entirely and require users to pass function handles rather than
  function names.

- Using `clear` with no arguments now removes only local variables
  from the current workspace.  Global variables will no longer be
  visible, but they continue to exist in the global workspace and
  possibly other workspaces such as the base workspace.


#### Nonlinear Equations

Several default solver options have been changed to be Matlab compatible.
This *may* result in existing code producing different results.

- `fsolve`

        Option         |   New Default    | Old Default
        ---------------|------------------|-------------
        FinDiffType    |    "forward"     |  "central"
        MaxFunEvals    | 100*length(x0)   |    Inf
        TolFun         |     1e-6         |    1e-7
        TolX           |     1e-6         |    1e-7
        Updating       |     "off"        |    "on"

- `fminsearch`

        Option   | New Default | Old Default
        ---------|-------------|------------
        TolFun   |   1e-7      |   1e-4

- `fminbnd`

        Option         | New Default | Old Default
        ---------------|-------------|------------
        MaxFunEvals    |     500     |   Inf
        MaxIter        |     500     |   Inf
        TolX           |    1e-4     |   1e-8

- `fminunc`

        Option         |   New Default    | Old Default
        ---------------|------------------|------------
        FinDiffType    |   "forward"      | "central"
        MaxFunEvals    | 100*length(x0)   |   Inf
        TolX           |     1e-6         |   1e-7
        TolFun         |     1e-6         |   1e-7


#### Graphic objects

- Figure graphic objects have a new property `"Number"` which is
  read-only and will return the handle (number) of the figure.
  However, if the property `"IntegerHandle"` has been set to `"off"`
  then the property will return an empty matrix `[]`.

- Patch and surface graphic objects now use the `"FaceNormals"` property
  for flat lighting.

- `"FaceNormals"` and `"VertexNormals"` for patch and surface graphic
  objects are now calculated only when necessary to improve graphics
  performance.  In order for any normals to be calculated the
  `"FaceLighting"` property must be set to `"flat"` (FaceNormals) or
  `"gouraud"` (VertexNormals), **and** a light object must be present
  in the axes.

- The `"Margin"` property of `text`-objects has a new default of `3`
  rather than `2`.

- Printing to raster formats (bitmaps like PNG or JPEG) now uses an
  OpenGL-based method by default.  The print options `"-opengl"`
  (raster) and `"-painters"` (vector) have been added ("qt" toolkit
  only).  The figure property `"renderer"` specifies which renderer to
  use.  When the property `"renderermode"` is `"auto"` Octave will select
  `"-opengl"` for a raster output format and `"-painters"` for a vector
  output format.

- A new print option `"-RGBImage"` has been added which captures the
  pixels of a figure as an image.  This is similar to screen capture
  tools, except that print formatting options can be used to, for
  example, change the resolution or display the image in black and
  white.

- Two new print options for page-based formats (PDF, PostScript) have
  been added.  The `"-fillpage"` option will stretch the plot to occupy
  the entire page with 0.25 inch margins all around.  The `"-bestfit"`
  option will expand the plot to take up as much room as possible on
  the page without distorting the original aspect ratio of the plot.

- Printing using the `"-dtiff"` output device will now create compressed
  images using LZW compression.  To produce uncompressed images use the
  `"-dtiffn"` device.


### Legacy functions

The following functions have been declared legacy functions which
means they are obsolete and should not be used in any new code.
Unlike deprecated functions, however, their removal from Octave has
not yet been scheduled.

        Function              | Replacement
        ----------------------|------------------
        findstr               | strfind
        flipdim               | flip
        isdir                 | isfolder or dir_in_loadpath
        isequalwithequalnans  | isequaln
        isstr                 | ischar
        setstr                | char
        strmatch              | strncmp or strcmp
        strread               | textscan
        textread              | textscan


### Deprecated functions and properties

The following functions and graphics properties have been deprecated
in Octave 5 and will be removed from Octave 7 (or whatever version
is the second major release after 5):

- Functions

        Function                 | Replacement
        -------------------------|-------------------
        output_max_field_width   | output_precision
        is_keyword               | iskeyword

- Graphics properties

        Object           | Property      | Value
        -----------------|---------------|------------
        text             | fontangle     | "oblique"
        uibuttongroup    | fontangle     | "oblique"
        uicontrol        | fontangle     | "oblique"
        uipanel          | fontangle     | "oblique"
        uitable          | fontangle     | "oblique"

- Specifying `legend` position with a numeric argument is deprecated.
  Use a string argument instead.

- The environment variable used by `mkoctfile` for linker flags is now
  `LDFLAGS` rather than `LFLAGS`.  `LFLAGS` is deprecated, and a warning
  is emitted if is used, but it will continue to work.


### Removed functions and properties

The following functions and properties were deprecated in Octave 4.2
and have been removed from Octave 5.

- Functions

        Function             | Replacement
        ---------------------|------------------
        bitmax               | flintmax
        mahalanobis          | mahal in Octave Forge statistics pkg
        md5sum               | hash
        octave_config_info   | __octave_config_info__
        onenormest           | normest1
        sleep                | pause
        usleep               | pause
        wavread              | audioread
        wavwrite             | audiowrite

- Properties

        Object      | Property          | Value
        ------------|-------------------|---------
        axes        | xaxislocation     | "zero"
                    | yaxislocation     | "zero"
        hggroup     | erasemode         |
        image       | erasemode         |
        line        | erasemode         |
        patch       | erasemode         |
        patch       | normalmode        |
        surface     | erasemode         |
        surface     | normalmode        |
        text        | erasemode         |

### Alphabetical list of new functions added in 5

- `clearvars`
- `isfile`
- `isfolder`
- `matlab.lang.makeUniqueStrings`
- `matlab.lang.makeValidName`
- `movegui`
- `movfun`
- `movie`
- `movmad`
- `movmax`
- `movmean`
- `movmedian`
- `movmin`
- `movprod`
- `movslice`
- `movstd`
- `movsum`
- `movvar`
- `openfig`
- `ordeig`
- `savefig`
- `uitable`


### Old release news

- [Octave 4.4.x](etc/NEWS.4)
- [Octave 4.2.x](etc/NEWS.4)
- [Octave 4.0.x](etc/NEWS.4)
- [Octave 3.8.x](etc/NEWS.3)
- [Octave 3.6.x](etc/NEWS.3)
- [Octave 3.4.x](etc/NEWS.3)
- [Octave 3.2.x](etc/NEWS.3)
- [Octave 3.0.x](etc/NEWS.3)
- [Octave 2.x](etc/NEWS.2)
- [Octave 1.x](etc/NEWS.1)
