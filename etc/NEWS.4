Summary of important user-visible changes for version 4.4 (2018-04-30):
----------------------------------------------------------------------

 ** A graphical Variable Editor has been added to the GUI interface.
    It uses a spreadsheet-like interface for quick, intuitive editing
    of variables.  The Variable Editor is launched by double-clicking
    on a variable name in the Workspace Window or by typing
    "openvar VARIABLE_NAME" in the Command Window.

 ** On systems with 64-bit pointers, --enable-64 is now the default and
    Octave always uses 64-bit indexing.  However, if the configure
    script determines that the BLAS library uses 32-bit integers, then
    operations using the following libraries are limited to arrays with
    dimensions that are smaller than 2^31 elements:

      BLAS  LAPACK  QRUPDATE  SuiteSparse  ARPACK

    Additionally, the following libraries use "int" internally, so
    maximum problem sizes are always limited:

      glpk  Qhull

 ** The octave command no longer starts the GUI by default.  Most users
    starting Octave from a shell were expecting the command line
    interface, and desktop launchers already required the `--force-gui'
    option.  With this change, desktop launchers should be modified to
    use the new option `--gui'.  The previous `--force-gui' option will
    continue to work, and maps to `--gui', but it will be removed in
    Octave 6.

 ** A known bug in Qt (https://bugreports.qt.io/browse/QTBUG-55357) is
    addressed by limiting GUI sub-panel relocation capabilities for Qt
    versions in the range >= 5.6.1 and < 5.7.1.  However, this may not
    thoroughly avoid issues on all platforms.

 ** A new container data type--containers.Map--is available.  Map is a
    key/value storage container (a.k.a, a hash) that efficiently allows
    storing and retrieving values by name, rather than by position which
    is how arrays work.

 ** The bareword "import" is now recognized in scripts and functions.
    However, the functionality to import functions and classes from
    other namespaces into the local scope has not yet been implemented.
    Attempting to use "import" will provoke an error message.

 ** hex2num and num2hex now work for integer and char types and num2hex
    may optionally return a cell array of strings instead of a character
    array.  If given a cell array of strings, hex2num now returns a
    numeric array of the same size as the input cell array.  Previously,
    hex2num would accept a cell array of strings of arbitrary dimension
    but would always return a column vector.

 ** New special functions cosint, sinint, and gammaincinv have been added.

 ** Special functions in Octave have been rewritten for larger input
    domains, better accuracy, and additional options.
    * gammainc now accepts negative real values for X.
    * improved accuracy for gammainc, betainc, betaincinv, expint.
    * gammainc has new options "scaledlower" and "scaledupper".
    * betainc, betaincinv have new option "upper".

 ** The "names" option used in regular expressions now returns a struct
    array, rather than a struct with a cell array for each field.  This
    change was made for Matlab compatibility.

 ** The quadcc function now uses both absolute tolerance and relative
    tolerance to determine the stopping criteria for an integration.
    To be compatible with other quadXXX functions, such as quadgk, the
    calling syntax has changed to

      quadcc (f, a, b, [AbsTol, [RelTol]])

    To update existing code, change instances of RelTol to [0, RelTol].

      quadcc (f, a, b, tol) => quadcc (f, a, b, [0, tol])

    A warning that a single tolerance input is now interpreted as an
    absolute tolerance will be issued in Octave versions 4.4 and 5,
    after which it will be removed.  The warning has ID
    "Octave:quadcc:RelTol-conversion" and can be disabled with

      warning ("off", "Octave:quadcc:RelTol-conversion")

 ** The qr function now returns a standard factorization unless
    explicitly instructed to perform an economy factorization by using a
    final argument of 0.

 ** The Qt graphics toolkit now supports offscreen printing without osmesa
    if Octave was built with Qt >= 5.1.

 ** The built-in pager for display of large data is now disabled by
    default.  To re-enable it for every Octave session add the following
    to your .octaverc file:

      more on;

 ** The FLTK toolkit is no longer prioritized for development.  The
    number of Octave Maintainers is too small to support three different
    graphic toolkits.  New development will target the Qt toolkit.
    While no longer prioritized, the FLTK toolkit is not deprecated and
    there is no schedule for its removal.

 ** The graphic object property "PickableParts" has been implemented
    which controls whether an object can accept mouse clicks.

 ** The graphic object property "Interruptible" has been fully
    implemented which controls whether a running callback function can
    be interrupted by another callback function.

 ** The graphic object property "HitTest" has been updated to be fully
    compatible with Matlab.

 ** Text objects now implement the properties "BackgroundColor",
    "EdgeColor", "LineStyle", "LineWidth", and "Margin".

 ** An initial implementation of alpha transparency has been made for
    patch and surface objects.  Printing to svg and pdf is supported.

 ** ishandle now returns true for both graphics handle objects and
    Java objects.  The latter change was made for Matlab compatibility.
    Use ishghandle or isgraphics if it is important not to include Java
    objects.

 ** The pkg command now accepts a URL as an argument, allowing a valid
    Octave package to be installed from any remote host with one command,
    for example

      pkg install https://example.org/download/example-package.tar.gz

 ** The following statistical functions have been moved from core
    Octave to the statistics package available from Octave Forge.

    BASE
      cloglog
      logit
      prctile
      probit
      qqplot
      table  (renamed to crosstab)

    DISTRIBUTIONS
      betacdf
      betainv
      betapdf
      betarnd
      binocdf
      binoinv
      binopdf
      binornd
      cauchy_cdf
      cauchy_inv
      cauchy_pdf
      cauchy_rnd
      chi2cdf
      chi2inv
      chi2pdf
      chi2rnd
      expcdf
      expinv
      exppdf
      exprnd
      fcdf
      finv
      fpdf
      frnd
      gamcdf
      gaminv
      gampdf
      gamrnd
      geocdf
      geoinv
      geopdf
      geornd
      hygecdf
      hygeinv
      hygepdf
      hygernd
      kolmogorov_smirnov_cdf
      laplace_cdf
      laplace_inv
      laplace_pdf
      laplace_rnd
      logistic_cdf
      logistic_inv
      logistic_pdf
      logistic_rnd
      logncdf
      logninv
      lognpdf
      lognrnd
      nbincdf
      nbininv
      nbinpdf
      nbinrnd
      normcdf
      norminv
      normpdf
      normrnd
      poisscdf
      poissinv
      poisspdf
      poissrnd
      stdnormal_cdf
      stdnormal_inv
      stdnormal_pdf
      stdnormal_rnd
      tcdf
      tinv
      tpdf
      trnd
      unidcdf
      unidinv
      unidpdf
      unidrnd
      unifcdf
      unifinv
      unifpdf
      unifrnd
      wblcdf
      wblinv
      wblpdf
      wblrnd
      wienrnd

    MODELS
      logistic_regression

    TESTS
      anova
      bartlett_test
      chisquare_test_homogeneity
      chisquare_test_independence
      cor_test
      f_test_regression
      hotelling_test
      hotelling_test_2
      kolmogorov_smirnov_test
      kolmogorov_smirnov_test_2
      kruskal_wallis_test
      manova
      mcnemar_test
      prop_test_2
      run_test
      sign_test
      t_test
      t_test_2
      t_test_regression
      u_test
      var_test
      welch_test
      wilcoxon_test
      z_test
      z_test_2

 ** The following image functions have been moved from core Octave to
    the image package available from Octave Forge.

      ntsc2rgb
      rgb2ntsc

 ** Other new functions added in 4.4:

      bounds
      camlookat
      camorbit
      campos
      camroll
      camtarget
      camup
      camva
      camzoom
      corrcoef
      cosint
      decic
      erase
      gammaincinv
      getframe
      groot
      gsvd
      hgtransform
      humps
      integral
      integral2
      integral3
      isgraphics
      isstring
      mad
      ode15i
      ode15s
      openvar
      quad2d
      repelem
      rgb2gray
      rticks
      sinint
      tfqmr
      thetaticks
      vecnorm
      winqueryreg
      xticklabels
      xticks
      yticklabels
      yticks
      zticklabels
      zticks

 ** Deprecated functions.

    The following functions have been deprecated in Octave 4.4 and will
    be removed from Octave 6 (or whatever version is the second major
    release after 4.4):

      Function             | Replacement
      ---------------------|------------------
      chop                 | sprintf for visual results
      desktop              | isguirunning
      tmpnam               | tempname
      toascii              | double
      java2mat             | __java2mat__


 ** The following functions were deprecated in Octave 4.0 and have been
    removed from Octave 4.4.

      allow_noninteger_range_as_index
      bicubic
      delaunay3
      do_braindead_shortcircuit_evaluation
      dump_prefs
      find_dir_in_path
      finite
      fmod
      fnmatch
      gmap40
      loadaudio
      luinc
      mouse_wheel_zoom
      nfields
      octave_tmp_file_name
      playaudio
      saveaudio
      setaudio
      syl
      usage

 ** The "Octave:undefined-return-values" warning ID is obsolete.  Octave
    now throws an error for any attempts to assign undefined values that
    might be returned from functions.

 ** Deprecated graphics properties.

    The following properties or allowed corresponding values have been
    deprecated in Octave 4.4 and will be removed from Octave 6 (or whatever
    version is the second major release after 4.4):

      Object               | Property                | Value
      ---------------------|-------------------------|-------------------
      figure               | doublebuffer            |
                           | mincolormap             |
                           | wvisual                 |
                           | wvisualmode             |
                           | xdisplay                |
                           | xvisual                 |
                           | xvisualmode             |
      axes                 | drawmode                |
      annotation           | edgecolor ("rectangle") |
      text                 | fontweight              | "demi" and "light"
      uicontrol            | fontweight              | "demi" and "light"
      uipanel              | fontweight              | "demi" and "light"
      uibuttongroup        | fontweight              | "demi" and "light"

 ** The rectangle and ellipse annotation property "edgecolor" has been
    deprecated and will be removed from Octave 6 (or whatever version
    is the second major release after 4.4).  Use the property "color"
    instead.

 ** The header file oct-alloc.h has been removed along with the macros
    that it defined (DECLARE_OCTAVE_ALLOCATOR, DEFINE_OCTAVE_ALLOCATOR,
    and DEFINE_OCTAVE_ALLOCATOR2).


Summary of bugs fixed for version 4.2.2 (2018-03-13):
----------------------------------------------------

Using the bug numbers listed below, find bug reports on the web using
the URL https://savannah.gnu.org/bugs/?NNNNN

 ** make leftdiv work for scalar \ int-matrix (bug #51682)

 ** inputdlg.m: Avoid crash when prompt and defaults sizes differ (bug #53209)

 ** tie octave_classdef::numel method to "numel" user override method
    (bug #46571)

 ** fix performance of Sparse fsolve for complex sparse matrices (bug #53140)

 ** fix performance of Sparse fsolve (bug #53140)

 ** octave.desktop.in: No repetition of Name in Comment field and start I10n
    (bug #53078)

 ** don't create partially invalid graphic objects (bug #52904)

 ** test for incorrect regexprep on ARM platforms (bug #52810)

 ** fix incorrect regexprep on ARM platforms (bug #52810)

 ** correctly handle reading of characters >127 in scanf family (bug #52681)

 ** fix addpath for UNC paths on Windows (bug #51268)

 ** protect being-deleted objects on figure list from second deletion
    (bug #52666)

 ** dlmwrite.m: Close fid if filename is only one char long (bug #52679)

 ** set gnuplot color data to half output range when autoscaling zero input
    range (bug #52624)

 ** add polarplot() to the list of unimplemented functions (bug #52643)

 ** configure.ac: Fix test for Java version (bug #52617)

 ** for gnuplot toolkit, do not map TrueColor data to colormap size (bug #52599)

 ** make wheel scroll behave more consistently in pan mode (bug #52588)

 ** make gnuplot color have three components for interpolated edge color
    (bug #52595)

 ** simplify gnuplot toolkit scripts for image/non-image data plots (bug #52589)

 ** fix concatenation of empty char matrices with other strings (bug #52542)

 ** build: Fix compiling OCTAVE_ARPACK_OK_2 Fortran code (bug #52425)

 ** trisurf.m, trimesh.m: Fix input validation (bug #48109)

 ** allow uncommenting in editor when line begins with whitespace (bug #52406)

 ** do not extend selection when indenting/commenting in editor (bug #45610)

 ** remove all delimiters from whitespace list in textscan function (bug #52479)

 ** calculate 1-norm of matrices to assess whether NaN or Inf are present
    (bug #39000)

 ** prevent extra ampersand under KDE in cd-or-add-to-path dialog (bug #52423)

 ** plotyy.m: Fix error when using FUN2 argument (bug #48115)

 ** check ARPACK library for buggy behavior in configure (bug #52425)

 ** fix printing integer type images (bug #51558)

 ** fix segfault in delaunayn when Qhull memory is not properly cleared
    (bug #52410)

 ** fix segfault with CHOLMOD library and empty matrices (bug #52365)

 ** tag global and persistent symbols as variables when parsing (bug #52363)

 ** properly restore the input stream pointer at end of textscan (bug #52116 et
    al.)

 ** fix building with Qt4 for Windows (bug #52237)

 ** ensure numeric values are passed for the axes "clim" property (bug #52053)

 ** avoid abort on exit from GUI (bug #50664)

 ** correct auto limits on log axes with negative and zero values (bug #51861)

 ** fix warning in quadgk with zero size interval (bug #51867)

 ** sparse: correctly handle scalar column index (bug #51880)

 ** fix segfault in ichol under certain conditions (bug #51736)

 ** configure: ensure empty pkg-config results are actually empty (bug #51680)

 ** fix 'legend hide' for gnuplot (bug #50483)

 ** qqplot.m: Fix typo in input validation (bug #51458)

 ** add possible '\r' to smartindent regex exprepression (Bug #51279)

 ** make strncmp case sensitive again (bug #51384)

 ** fix possible infinite loop in normest1.m (bug #51241)

 ** also run unwind protect cleanup code on interrupt exceptions (bug #51209)

 ** fix crash when inverting complex matrices with NaNs (bug #51198)

 ** improve accuracy of residue for inputs with very different magnitudes
    (bug #51148)

 ** publish.m: Fix corruption of results for some code inputs (bug #51178)

 ** residue.m: Remove code that filters out small return values (bug #34266, bug
    #49291)

 ** avoid possible double free at interpreter exit (bug #51088)

 ** show stack trace for errors in command line and startup files (bug #49346)

 ** interp1.m: Return NA for all columns which are out of bounds (bug #51030)

 ** use idx_type for dimensions instead of int (bug #50934)

 ** show stack trace for wrong type arg errors (bug #50894)

 ** let mouse selection of Qt figures update "currentfigure" (bug #50666)

 ** disable qscintilla editor drag and drop so parent will handle it (Bug
    #50559)

 ** quadgk.m: Correct error messages which point to quadv (bug #50604)

 ** set version on AppUserModelId (Bug #50428)

 ** version-rcfile: Don't try to execute startup directory, only startup.m
    (bug #50593)

 ** dlmread: Return empty matrix when requested range is outside data
    (bug #50102)

 ** fix eigs for generalized nonsymmetric and shift-invert problems (bug #39573)

 ** fix eigs for the generalized eigenvalue problem (bug #50546)

 ** datetick.m: Fix uneven range bugs (bug #50493)

 ** datenum.m: Correct calculation for fractional leap years (bug #50508)

 ** datenum.m: Allow horizontal vectors of dates with fractional months
    (bug #50508)

 ** datenum.m: Accept legal input of vectors with fractional months (bug #50508)

 ** fix the anchor position in the info text of the doc browser (bug #50422)

 ** fix order of legend labels with plotyy axes (bug #50497)

 ** correct hggroup plot legends for gnuplot toolkit, add legend demo 17 items
    (bug #49341)

 ** for gnuplot graphics toolkit, show only one key entry for errorbars
    (bug #49260)

 ** fix compilation of jit caused by cset d0562b3159c7 (bug #50398)

 ** remove inline keyword on file_stat destructor which breaks MacOS compilation
    (bug #50234)

Documentation bugs fixed:

 ** playblocking.m: Correct documentation about start and limits inputs
    (bug #51217)

 ** fix eig output argument description (bug #50524)

 ** remove backslashes before double quotes in m-file docstrings (bug #52870)

 ** tweaks to use single quotes instead of double quotes (bug #52870)

 ** correct fieldname of returned struct in ver (bug #52845)

 ** cleanup @code example in Appendix on test functions (bug #52852)

 ** fixes for signal, image, audio, and OOP chapters (bug #52844)

 ** fix issues in geometry, polynomial, and interpolation chapters (bug #52835)

 ** fix TeX documentation for qp and clarify size of inputs (bug #52829)

 ** correct errors in Diagonal matrix chapter of manual (bug #52814)

 ** replace @math{1e^{XXX}} sequences with raw 1eXXX (bug #52827)

 ** use '...' rather than deprecated '\' for line continuation (bug #52828)

 ** make documentation Sec 26.1 more consistent and Sec 25.4 clearer
    (bug #52685)

 ** documentation fixes for linspace, logspace, lookup (bug #52785)

 ** atan2d.m: Correct documentation to match atan docstring (bug #52786)

 ** small tweaks to fplot and surfnorm docstrings (bug #52761)

 ** rewrite documentation for Advanced Indexing (bug #52723)

 ** delete extra ']' in scanf docstring (bug #52742)

 ** fix mistaken use of space between function and '(' in documentation
    (bug #52723)

 ** fix various inconsistencies in manual (bug #52712)

 ** fix typo in cset 8354b505ad6b (bug #52702)

 ** fix inconsistencies with char, strvcat, strread docstrings (bug #52702.

 ** explain Matlab compatibility of fopen modes (bug #52644)

 ** update documentation for keywords to include classdef statements
    (bug #52591)

 ** fix documentation of third input to lsode() (bug #52664)

 ** clarify quiver/quiver3 documentation when a linestyle is given (bug #52608)

 ** new section about classdef classes with example (bug #44590)

 ** correct surface plot explanation of  meshgridded results of 1 input
    (bug #52536)

 ** fix definition of Delaunay triangulation in docstrings (bug #52416)

 ** accumarray.m: Add '@' to function handles in docstring (bug #52418)

 ** update manual to explain \deg and \circ symbols (bug #52287)

 ** correct documentation for randg (bug #52118)

 ** add documentation about PCRE library regexp stack overflow (bug #51589)

 ** play.m: Correct documentation about start and limits inputs (bug #51217)

 ** redo docstring for qz (bug #50846)

 ** describe optional install dependencies PortAudio and SUNDIALS (bug #50513)

 ** update CITATION date, version, and permalink to manual (bug #47058)


Summary of bugs fixed for version 4.2.1 (2017-02-22):
----------------------------------------------------

Using the bug numbers listed below, find bug reports on the web using
the URL https://savannah.gnu.org/bugs/?NNNNN

 ** guarantee returning std::string from tilde_expand functions (bug #50234)

 ** workaround segfault in file_stat (bug #50234)

 ** genpropdoc.m: document more graphics properties (bug #50337)

 ** always fork and exec when starting the gui (bug #49609)

 ** print.m: fix regression with -append option (bug #50318)

 ** don't display legend, colorbar, and annotation axes coordinates
    (bug #50272)

 ** qp.m: Fix regression with incorrect vector dimensions (bug #50067)

 ** prevent infinite loop in global documentation search (bug #50177)

 ** connect execute command signal in editor constructor (bug #50171)

 ** connect editors execute command signal to the required slot (bug #50171)

 ** check if input is class method before declaring it unimplemented
    (patch #9238) (bug #49694)

 ** workaround segfault when an error occurs while printing (bug #49779)

 ** axis.m: Do not set plotboxaspectratio to 0 (bug #49755)

 ** don't rethrow exception in destructor (bug #49304)

 ** rethrow octave::exit_exception (bug #49304)

 ** update appdata.xml to follow conventions (bug #49952)

 ** mexproto.h (mxAssert, mxAssertS): ensure operator precedence (bug #50050)

 ** calculate error in solution for ode solvers correctly (bug #49950)

 ** use GetModuleFileName for getting octave path in windows (bug #48671)

 ** use C++ updaters for labels color (bug #49980)

 ** distinguish elements vs. bytes in fread (bug #49699)

 ** move frame2im and im2frame to image/ directory (bug #49939)

 ** fix undefined return argument for more than 2 outputs from ode solver
    (bug #49890)

 ** fix inv for hermitian matrices (bug #49904)

 ** fix gzip for certain types of gzip files (bug #49760)

 ** fix typo in liboctave version info (bug #49860)

 ** initialize ODE Event function with start time (bug #49846)

 ** allow configure test to succeed without implicit fcn decls (bug #49782)

 ** allow external docstrings from .oct files to be found again (bug #49687)

 ** don't require semicolon between property list elements (bug #49819)

 ** display.m: Correctly display output for non-class objects
    (bug #49753, #49794)

 ** don't run publish.tst unless OSMESA or gnuplot are available (bug #49767)

 ** find help for function aliases again (bug #49687)

 ** legend.m: backport cset 7184b4516a68 (bug #49675)

 ** preserve lasterror info on rethrow (bug #49642)

 ** norm: fix error in input argument validation leading to segfault
    (bug #49634)

Documentation bugs fixed:

 ** overhaul Java interface description (bug #50299)

 ** add documentation for hex and binary prefix and _ separator
    (bug #50305, #50334)

 ** fix build of docs broken in sub2ind (bug #50348)

 ** version.m: document that "-release" returns an empty string (bug #50294)

 ** remove trailing "\n\" from sleep and usleep docstrings (bug #50301)

 ** expand documentation for cast() (bug #50201)

 ** correct two entries in Table 34.1 (bug #50203)

 ** oop.txi: Improve table formatting (bug #50203)

 ** fix '##' in middle of docstring/comment lines (bug #50145)

 ** reword documentation about subplots in 15.2.4 (bug #50148)

 ** update unimplemented list of functions and where to find them
    (bug #50098)

 ** compare_plot_demos: fix HTML syntax, simplify output, remove
    external deps (bug #49709)

 ** add more depth to explanation of '~' function argument (bug #49444)

 ** correct documentation for javaclasspath file (bug #49873)

 ** small fixes to docstrings (bug #49733)

 ** change text describing demo plots to reflect new ColorOrder (bug #49288)

Other bugs fixed:

 ** add missing classdef test files (bug #49819)


Summary of important user-visible changes for version 4.2 (2016-11-13):
----------------------------------------------------------------------

 ** The parser has been extended to accept, but ignore, underscore
    characters in numbers.  This facilitates writing more legible code
    by using '_' as a thousands separator or to group nibbles into bytes
    in hex constants.

    Examples: 1_000_000 == 1e6  or  0xDE_AD_BE_EF

 ** The parser has been extended to understand binary numbers which
    begin with the prefix '0b' or '0B'.  The value returned is Octave's
    default numeric class of double, not at unsigned integer class.
    Therefore numbers greater than flintmax, i.e., 2^53, will lose some
    precision.

    Examples: 0b101 == 5  or  0B1100_0001 == 0xC1

 ** gnuplot 4.4 is now the minimum version supported by Octave.

 ** The default set of colors used to plot lines has been updated to be
    compatible with Matlab's new default color scheme.  The line plot
    color scheme can be set with the axes property "ColorOrder".

 ** The default colormap is now set to "viridis" which is also the
    default colormap in matplotlib.  This new colormap fixes some of the
    main issues with the old default colormap "jet" such as its bad
    "luminance profile" and is also more similar to Matlab's new default
    colormap "parula".

 ** The colormap function no longer supports the input argument "list"
    to show built-in colormaps.  Use "help colormap" to find the
    built-in colormaps.

 ** The graphics command "hold on" now ensures that each new plot added
    to an existing plot has a different color or linestyle according to
    the "ColorOrder" and/or "LineStyleOrder" properties.  This is
    equivalent to the old command "hold all" and was made for Matlab
    compatibility.  Existing code *may* produce differently colored
    plots if it did not specify the color for a plot and relied on each
    new plot having the default first color in the "ColorOrder"
    property.

 ** When starting, Octave now looks in the function path for a file
    startup.m and executes any commands found there.  This change was
    made to accommodate Matlab users.  Octave has it's own configuration
    system based on the file .octaverc which is preferred.

 ** Octal ('\NNN') and hex ('\xNN') escape sequences in single quoted
    strings are now interpreted by the function do_string_escapes().
    The *printf family of functions now supports octal and hex escape
    sequences in single-quoted strings for Matlab compatibility.

 ** Special octal and hex escape sequences for the pattern and
    replacement strings in regular expressions are now interpreted for
    Matlab compatibility.

    octal: '\oNNN' or '\o{NNN}'
    hex  : '\xNN'  or '\x{NN}'

 ** Unknown escape sequences in the replacement string for regexprep are
    now substituted with their unescaped version and no warning is
    emitted.  This change was made for Matlab compatibility.

    Example: regexprep ('a', 'a', 'x\yz')
             => 'xyz'

 ** mkfifo now interprets the MODE argument as an octal, not decimal,
    integer.  This is consistent with the equivalent shell command.

 ** linspace now returns an empty matrix if the number of requested
    points is 0 or a negative number.  This change was made to be
    compatible with Matlab releases newer than 2011.  In addition,
    Octave no longer supports matrix inputs for A or B.

 ** The cov function now returns the complex conjugate of the result
    from previous versions of Octave.  This change was made for
    compatibility with Matlab.

 ** condest now works with a normest1 compatible syntax.

 ** The griddata function no longer plots the interpolated mesh if no
    output argument is requested, instead the vector or array of
    interpolated values is always returned for Matlab compatibility.

 ** The new function "light" and the corresponding graphics object
    provide light and shadow effects for patch and surface objects.

 ** The surfnorm function now returns unnormalized (magnitude != 1)
    normal vectors for compatibility with Matlab.

 ** The normal vectors returned from isonormals have been reversed to
    point towards smaller values for compatibility with Matlab.

 ** The quadl function now uses an absolute, rather than relative,
    tolerance for Matlab compatibility.  The default tolerance is 1e-6
    which may result in lower precision results than previous versions
    of Octave which used eps as the relative tolerance.  The quadl
    function has also been extended to return a second output with the
    total number of function evaluations.

 ** The textscan function is now built-in and is much faster and much
    more Matlab-compatible than the previous m-file version.

 ** Dialog boxes--errordlg, helpdlg, inputdlg, listdlg, msgbox,
    questdlg, and warndlg--now exclusively use Qt for rendering.
    Java based versions have been removed.

 ** The axes properties "TitleFontSizeMultiplier" and "TitleFontWeight"
    are now implemented which control the default appearance of text
    created with title().
    The axes property "LabelFontSizeMultiplier" is now implemented
    which controls the default appearance of text created with
    xlabel(), ylabel(), or zlabel().

 ** The graphics property "box" for axes now defaults to "off".
    To obtain equivalent plots to previous versions of Octave use
      set (0, "DefaultAxesBox", "on");
    in your .octaverc file.

 ** The graphics property "boxstyle" has been implemented.  The default
    is "back" which draws only the back planes in a 3-D view.  If the
    option is "full" then all planes are drawn.

 ** The graphics property "erasemode" has been hidden, and will
    eventually be removed.  This property has also been removed
    from Matlab, and was never implemented in Octave.

 ** The graphics property "graphicssmoothing" for figures now controls
    whether anti-aliasing will be used for lines.  The default is "on".

 ** The value "zero" for the axes properties "xaxislocation" and
    "yaxislocation" has been deprecated and will be removed from
    Octave 5.  Use "origin" instead.

 ** The publish function allows easy publication of Octave script files
    in HTML or other formats, including figures and output created by
    this script.  It comes with its counterpart grabcode, which lets one
    literally grab the HTML published code from a remote website, for
    example.

 ** The value of the MEX variable TrapFlag now defaults to 0, which will
    cause Octave to abort execution of a MEX file and return to the
    prompt if an error is encountered in mexCallMATLAB.

 ** The MEX API now includes the function mexCallMATLABWithTrap.  This
    function will not abort if an error occurs during mexCallMATLAB, but
    instead will return execution to the MEX function for error
    handling.

 ** The MEX API functions for input validation that begin with "mxIs"
    (e.g., mxIsDouble, mxIsEmpty, etc.) now return type bool rather than
    type int.

 ** The functions mxAssert and mxAssertS for checking assertions have
    been added.  In order to avoid a performance penalty they are only
    compiled in to debug versions of a MEX file, i.e., that are produced
    when the '-g' option is given to mex or mkoctfile.

 ** Other new MEX API functions include mexEvalStringWithTrap,
    mxIsScalar, mxCreateUninitNumericArray, mxCreateUninitNumericMatrix.

 ** Other new functions added in 4.2:

      audioformats
      camlight
      condeig
      deg2rad
      dialog
      evalc
      hash
      im2double
      isocaps
      lighting
      localfunctions
      material
      normest1
      ode23
      ode45
      odeget
      odeplot
      odeset
      padecoef
      profexport
      psi
      rad2deg
      reducepatch
      reducevolume
      smooth3
      uibuttongroup

 ** Deprecated functions.

    The following functions have been deprecated in Octave 4.2 and will
    be removed from Octave 5 (or whatever version is the second major
    release after 4.2):

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

 ** The following functions were deprecated in Octave 3.8 and have been
    removed from Octave 4.2.

      default_save_options    java_new
      gen_doc_cache           java_unsigned_conversion
      interp1q                javafields
      isequalwithequalnans    javamethods
      java_convert_matrix     re_read_readline_init_file
      java_debug              read_readline_init_file
      java_invoke             saving_history

 ** The global error_state variable in Octave's C++ API has been
    deprecated and will be removed in a future version.  Now the error
    and print_usage functions throw an exception
    (octave::execution_exception) after displaying the error message.
    This makes the error and print_usage functions in C++ work more like
    the corresponding functions in the scripting language.

 ** The default error handlers in liboctave have been updated to use
    exceptions.  After displaying an error message they no longer return
    control to the calling program.  The error handler function can be
    customized through the global variables
    "current_liboctave_error_handler" and
    "current_liboctave_error_with_id_handler".  If a programmer has
    installed their own custom error handling routines when directly
    linking with liboctave then these must be updated to throw an
    exception and not return to the calling program.

 ** The system for common errors and warnings has been renamed from
    gripe_XXX to either err_XXX if error is called or warn_XXX if
    warning is called.  The gripe_XXX functions are deprecated and will
    be removed in version 5.

 ** New configure option, --enable-address-sanitizer-flags, to build
    Octave with memory allocator checks (similar to those in valgrind)
    built in.

Summary of important user-visible changes for version 4.0 (2015-05-23):
----------------------------------------------------------------------

 ** A graphical user interface is now the default when running Octave
    interactively.  The start-up option --no-gui will run the familiar
    command line interface, and still allows use of the GUI dialogs and
    qt plotting toolkit.  The option --no-gui-libs runs a minimalist
    command line interface that does not link with the Qt libraries and
    uses the fltk toolkit for plotting.

 ** Octave now uses OpenGL graphics with Qt widgets by default.  If
    OpenGL libraries are not available when Octave is built, gnuplot is
    used.  You may choose to use the fltk or gnuplot toolkit for
    graphics by executing the command

      graphics_toolkit ("fltk")
        OR
      graphics_toolkit ("gnuplot")

    Adding such a command to your ~/.octaverc file will set the default
    for each session.

 ** A new syntax for object oriented programming termed classdef has
    been introduced.  See the manual for more extensive documentation of
    the classdef interface.

    New keywords:

      classdef      endclassdef
      enumeration   endenumeration
      events        endevents
      methods       endmethods
      properties    endproperties

 ** New audio functions and classes:

      audiodevinfo  audioread      sound
      audioinfo     audiorecorder  soundsc
      audioplayer   audiowrite

 ** Other new classes in Octave 4.0:

      audioplayer    inputParser
      audiorecorder

 ** Optional stricter Matlab compatibility for ranges, diagonal
    matrices, and permutation matrices.

    Octave has internal optimizations which use space-efficient storage
    for the three data types above.  Three new functions have been added
    which control whether the optimizations are used (default), or
    whether the data types are stored as full matrices.

    disable_range   disable_diagonal_matrix   disable_permutation_matrix

    All three optimizations are disabled if Octave is started with the
    --braindead command line option.

 ** The preference

      do_braindead_shortcircuit_evaluation

    is now enabled by default.

 ** The preference

      allow_noninteger_range_as_index

    is now enabled by default and the warning ID

      Octave:noninteger-range-as-index

    is now set to "on" by default instead of "error" by default and "on"
    for --traditional.

 ** The "backtrace" warning option is now enabled by default.  This
    change was made for Matlab compatibility.

 ** For compatibility with Matlab, the "ismatrix (x)" function now only
    checks the dimension of "x".  The old behavior of "ismatrix" is
    obtained by "isnumeric (x) || islogical (x) || ischar (x)".

 ** The nextpow2 function behavior has been changed for vector inputs.
    Instead of computing `nextpow2 (length (x))', it will now compute
    nextpow2 for each element of the input.  This change is Matlab
    compatible, and also prevents bugs for "vectors" of length 1.

 ** polyeig now returns a row vector of eigenvalues rather than a matrix
    with the eigenvalues on the diagonal.  This change was made for
    Matlab compatibility.

 ** Interpolation function changes for Matlab compatibility

    The interpolation method 'cubic' is now equivalent to 'pchip' for
    interp1, interp2, and interp3.  Previously, 'cubic' was equivalent
    to 'spline' for interp2.  This may produce different results as
    'spline' has continuous 1st and 2nd derivatives while 'pchip' only
    has a continuous 1st derivative.  The methods 'next' and 'previous'
    have been added to interp1 for compatibility.

 ** The delaunay function has been extended to accept 3-D inputs for
    Matlab compatibility.  The delaunay function no longer plots the
    triangulation if no output argument is requested, instead, the
    triangulation is always returned.  The delaunay3 function which
    handles 3-D inputs has been deprecated in favor of delaunay.

 ** The trigonometric functions asin and acos return different phase
    values from previous versions of Octave when the input is outside
    the principal branch ([-1, 1]).  If the real portion of the input is
    greater than 1 then the limit from below is taken.  If the real
    portion is less than 1 then the limit from above is taken.  This
    criteria is consistent with several other numerical analysis
    software packages.

 ** The hyperbolic function acosh now returns values with a phase in the
    range [-pi/2, +pi/2].  Previously Octave returned values in the
    range [0, pi].  This is consistent with several other numerical
    analysis software packages.

 ** strfind changes when using empty pattern ("") for Matlab
    compatibility

    strfind now returns an empty array when the pattern itself is empty.
    In previous versions of Octave, strfind matched at every character
    location when the pattern was empty.

      NEW
      strfind ("abc", "") => []
      OLD
      strfind ("abc", "") => [1, 2, 3, 4]

 ** Integer formats used in the printf family of functions now work for
    64-bit integers and are more compatible with Matlab when printing
    non-integer values.  Now instead of truncating, Octave will switch
    the effective format to '%g' in the following circumstances:

      * the value of an integer type (int8, uint32, etc.) value exceeds
        the maximum for the format specifier.  For '%d', the limit is
        intmax ('int64') and for '%u' it is intmax ('uint64').

      * round(x) != x or the value is outside the range allowed by the
        integer format specifier.

    There is still one difference: Matlab switches to '%e' and Octave
    switches to '%g'.

 ** The functions intersect, setdiff, setxor, and union now return a
    column vector as output unless the input was a row vector.  This
    change was made for Matlab compatibility.

 ** The inpolygon function now returns true for points that are within
    the polygon OR on it's edge.  This change was made for Matlab
    compatibility.

 ** The archive family of functions (bzip2, gzip, zip, tar) and their
    unpacking routines (bunzip2, gunzip, unzip, untar, unpack) have been
    recoded.  Excepting unpack, the default is now to place files in the
    same directory as the archive (on unpack) or as the original files
    (on archiving).

 ** Qt and FLTK graphics toolkits now support offscreen rendering on
    Linux.  In other words, print will work even when the figure
    visibility is "off".

 ** Z-order stacking issues with patches, grid lines, and line object
    plot markers for on screen display and printing have all been
    resolved.  For 2-D plots the axis grid lines can be placed on top of
    the plot with set (gca, "layer", "top").

 ** The patch graphic object has been overhauled.  It now produces
    visual results equivalent to Matlab even for esoteric combinations
    of faces/vertices/cdata.

 ** The polar() plot function now draws a circular theta axis and radial
    rho axis rather than using a rectangular x/y axis.

 ** linkprop has been completely re-coded for performance and Matlab
    compatibility.  It now returns a linkprop object which must be
    stored in a variable for as long as the graphic objects should
    remain linked.  To unlink properties use 'clear hlink' where hlink
    is the variable containing the linkprop object.

 ** isprime has been extended to operate on negative and complex inputs.

 ** xor has been extended to accept more than two arguments in which
    case it performs cumulative XOR reduction.

 ** The following functions now support N-dimensional arrays:

      fliplr   flipud   rot90   rectint

 ** The new warning ID "Octave:data-file-in-path" replaces the three
    previous separate warning IDs "Octave:fopen-file-in-path",
    "Octave:load-file-in-path", and "Octave:md5sum-file-in-path".

 ** The warning ID Octave:singular-matrix-div has been replaced by
    Octave:nearly-singular-matrix and Octave:singular-matrix.

 ** The warning ID Octave:matlab-incompatible has been replaced by
    Octave:language-extension to better reflect its meaning.

 ** The warning ID Octave:broadcast has been removed.  Instead automatic
    broadcasting will throw an Octave:language-extension warning.  This
    warning ID is used for broadcasting as well as other features not
    available in Matlab.

 ** Other new functions added in 4.0:

      annotation
      bandwidth
      cubehelix
      dir_in_loadpath
      flip
      frame2im
      get_home_directory
      hgload
      hgsave
      ichol
      ilu
      im2frame
      isbanded
      isdiag
      isstudent
      istril
      istriu
      javachk
      jit_failcnt
      linkaxes
      lscov
      metaclass
      numfields
      open
      ordschur
      pan
      qmr
      rotate
      rotate3d
      sylvester
      unsetenv
      validateattributes
      zoom

 ** inline() scheduled for eventual deprecation by Matlab

    Functions created through the use of inline are scheduled for
    deprecation by Matlab.  When this occurs Octave will continue to
    support inline functions for an indeterminate amount of time before
    also removing support.  All new code should use anonymous functions
    in place of inline functions.

 ** Deprecated functions.

    The following functions have been deprecated in Octave 4.0 and will
    be removed from Octave 4.4 (or whatever version is the second major
    release after 4.0):

      Function             | Replacement
      ---------------------|------------------
      bicubic              | interp2
      delaunay3            | delaunay
      dump_prefs           | individual preference get/set routines
      find_dir_in_path     | dir_in_loadpath
      finite               | isfinite
      fmod                 | rem
      fnmatch              | glob or regexp
      gmap40               | ----
      loadaudio            | audioread
      luinc                | ilu or ichol
      mouse_wheel_zoom     | mousewheelzoom axes property
      nfields              | numfields
      octave_tmp_file_name | tempname
      playaudio            | audioplayer
      saveaudio            | audiowrite
      syl                  | sylvester
      usage                | print_usage

      allow_noninteger_range_as_index
      do_braindead_shortcircuit_evaluation
      setaudio

 ** The following functions were deprecated in Octave 3.8 and will be
    removed from Octave 4.2 (or whatever version is the second major
    release after 3.8):

      default_save_options    java_new
      gen_doc_cache           java_unsigned_conversion
      interp1q                javafields
      isequalwithequalnans    javamethods
      java_convert_matrix     re_read_readline_init_file
      java_debug              read_readline_init_file
      java_invoke             saving_history

 ** The following functions were deprecated in Octave 3.6 and have been
    removed from Octave 4.0.

      cut                polyderiv
      cor                shell_cmd
      corrcoef           studentize
      __error_text__     sylvester_matrix
      error_text

 ** The following keywords were deprecated in Octave 3.8 and have been
    removed from Octave 4.0

      static

 ** The following configuration variables were deprecated in Octave 3.8
    and have been removed from Octave 4.0

      CC_VERSION  (now GCC_VERSION)
      CXX_VERSION (now GXX_VERSION)

 ** The internal function atan2 of the sparse matrix class has been
    deprecated in Octave 4.0 and will be removed from Octave 4.4 (or
    whatever version is the second major release after 4.0).  Use the
    Fatan2 function with sparse inputs as a replacement.

 ** The internal class Octave_map was deprecated in Octave 3.8 and has
    been removed from Octave 4.0.  Replacement classes are octave_map
    (struct array) or octave_scalar_map for a single structure.

 ** Octave now has OpenMP enabled by default if the system provides a
    working OpenMP implementation.  This allows oct-file modules to take
    advantage of OpenMP if desired.  This can be disabled when building
    Octave with the configure option --disable-openmp.

 ** Octave now automatically truncates intermediate calculations done
    with floating point values to 64 bits.  Some hardware math
    co-processors, such as the x87, maintain extra precision, but this
    leads to disagreements in calculations when compared to reference
    implementations in software using the IEEE standard for double
    precision.  There was no measurable performance impact to this
    change, but it may be disabled with the configure option
    --disable-float-truncate.  MinGW and Cygwin platforms, as well as
    GCC compilers >= 5.0 require this feature.  Non-x87 hardware, or
    hardware using SSE options exclusively, can disable float truncation
    if desired.

---------------------------------------------------------

See NEWS.3 for old news.
