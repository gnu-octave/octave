## Copyright (C) 2010-2012 John W. Eaton
## Copyright (C) 2010 VZLU Prague
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{txt} =} unimplemented (@var{fcn})
## Return specific help text for the unimplemented function @var{fcn}.
## This is usually a suggestion for an existing compatible function to use in
## place of @var{fcn}.
##
## This function is not called by users, but by the Octave interpreter when
## it fails to recognize an input string as a valid function name.  See
## @code{missing_function_hook} for using a different handler for this event.
## @seealso{missing_function_hook}
## @end deftypefn


function txt = __unimplemented__ (fcn)

  if (nargin != 1)
    print_usage ();
  endif

  is_matlab_function = true;

  ## Some smarter cases, add more as needed.
  switch (fcn)
    case {"avifile", "aviinfo", "aviread"}
      txt = ["Basic video file support is provided in the video package.  ", \
             "See @url{http://octave.sf.net/video/}."];

    case "gsvd"
      txt = ["gsvd is not currently part of core Octave.  ", \
             "See the linear-algebra package at ", \
             "@url{http://octave.sourceforge.net/linear-algebra/}."];

    case "funm"
      txt = ["funm is not currently part of core Octave.  ", \
             "See the linear-algebra package at ", \
             "@url{http://octave.sourceforge.net/linear-algebra/}."];

    case "griddedInterpolant"
      txt = ["griddedInterpolant is not implemented.  ", \
             "Consider using griddata."];

    case "integral"
      txt = ["Octave provides many routines for 1-D numerical integration.  ", \
             "Consider quadcc, quad, quadv, quadl, quadgk."];

    case "integral2"
      txt = ["integral2 is not implemented.  Consider using dblquad."];

    case "integral3"
      txt = ["integral3 is not implemented.  Consider using triplequad"];

    case "linprog"
      txt = ["Octave does not currently provide linprog.  ", \
             "Linear programming problems may be solved using @code{glpk}.  ",\
             "Try @code{help glpk} for more info."];

    case "matlabrc"
      txt = ["matlabrc is not implemented.  ", \
             'Octave uses the file ".octaverc" instead.'];

    case {"ode113", "ode15i", "ode15s", "ode23", "ode23s", "ode23t", \
          "ode23tb", "ode45", "odeget", "odeset"}
      txt = ["Octave provides lsode for solving differential equations.  ", \
             "For more information try @code{help lsode}.  ", \
             "Matlab-compatible ODE functions are provided by the odepkg ", \
             "package.  See @url{http://octave.sourceforge.net/odepkg/}."];

    case "startup"
      txt = ["startup is not implemented.  ", \
             'Octave uses the file ".octaverc" instead.'];

    case "quad2d"
      txt = ["quad2d is not implemented.  Consider using dblquad."];

    case {"xlsread", "xlsfinfo", "xlswrite", "wk1read", "wk1finfo", "wk1write"}
      txt = ["Functions for spreadsheet style I/O ", \
             "(.xls .xlsx .sxc .ods .dbf .wk1 etc.) " ,  \
             "are provided in the io package. ", \
             "See @url{http://octave.sf.net/io/}."];

    otherwise
      if (ismember (fcn, missing_functions ()))
        txt = ["the '" fcn "' function is not yet implemented in Octave"];
      else
        is_matlab_function = false;
        txt = "";
      endif
  endswitch

  if (is_matlab_function)
    txt = [txt, "\n\n@noindent\nPlease read ", \
           "@url{http://www.octave.org/missing.html} to learn how ", \
           "you can contribute missing functionality."];
    txt = __makeinfo__ (txt);
  endif

  if (nargout == 0)
    warning ("Octave:missing-function", "%s", txt);
  endif

endfunction

function list = missing_functions ()
  persistent list = {
  "MException",
  "RandStream",
  "Tiff",
  "VideoReader",
  "VideoWriter",
  "align",
  "alim",
  "alpha",
  "alphamap",
  "annotation",
  "audiodevinfo",
  "audioinfo",
  "audioplayer",
  "audioread",
  "audiorecorder",
  "audiowrite",
  "bar3",
  "bar3h",
  "bench",
  "bicgstabl",
  "brush",
  "builddocsearchdb",
  "bvp4c",
  "bvp5c",
  "bvpget",
  "bvpinit",
  "bvpset",
  "bvpxtend",
  "callSoapService",
  "calllib",
  "camdolly",
  "cameratoolbar",
  "camlight",
  "camlookat",
  "camorbit",
  "campan",
  "campos",
  "camproj",
  "camroll",
  "camtarget",
  "camup",
  "camva",
  "camzoom",
  "cdf2rdf",
  "cdfepoch",
  "cdfinfo",
  "cdfread",
  "cdfwrite",
  "cellplot",
  "checkin",
  "checkcode",
  "checkout",
  "cholinc",
  "clearvars",
  "clipboard",
  "cmopts",
  "colordef",
  "colormapeditor",
  "commandhistory",
  "commandwindow",
  "condeig",
  "coneplot",
  "containers.Map",
  "contourslice",
  "createClassFromWsdl",
  "createSoapMessage",
  "customverctrl",
  "datacursormode",
  "dbmex",
  "dde23",
  "ddeget",
  "ddensd",
  "ddesd",
  "ddeset",
  "decic",
  "delaunayTriangulation",
  "depdir",
  "depfun",
  "deval",
  "dialog",
  "dither",
  "docsearch",
  "dragrect",
  "dynamicprops",
  "echodemo",
  "evalc",
  "exifread",
  "export2wsdlg",
  "figurepalette",
  "filebrowser",
  "fill3",
  "fitsdisp",
  "fitsinfo",
  "fitsread",
  "fitswrite",
  "flow",
  "frame2im",
  "freqspace",
  "funm",
  "gammaincinv",
  "getframe",
  "getpixelposition",
  "gobjects",
  "grabcode",
  "graymon",
  "griddedInterpolant",
  "gsvd",
  "guidata",
  "guide",
  "guihandles",
  "handle",
  "h5create",
  "h5disp",
  "h5info",
  "h5read",
  "h5readatt",
  "h5write",
  "h5writeatt",
  "hdfinfo",
  "hdfread",
  "hgexport",
  "hgload",
  "hgsave",
  "hgsetget",
  "hgtransform",
  "ichol",
  "ilu",
  "im2frame",
  "im2java",
  "imapprox",
  "import",
  "inmem",
  "inputParser",
  "inspect",
  "instrcallback",
  "instrfind",
  "instrfindall",
  "integral",
  "integral2",
  "integral3",
  "interpstreamspeed",
  "iscom",
  "isinterface",
  "isjava",
  "isocaps",
  "isstudent",
  "javachk",
  "ldl",
  "libfunctions",
  "libfunctionsview",
  "libisloaded",
  "libpointer",
  "libstruct",
  "light",
  "lightangle",
  "lighting",
  "linkaxes",
  "linkdata",
  "linsolve",
  "listfonts",
  "loadlibrary",
  "lscov",
  "lsqr",
  "makehgtform",
  "material",
  "matfile",
  "matlabrc",
  "memmapfile",
  "memory",
  "metaclass",
  "methodsview",
  "minres",
  "mlintrpt",
  "mmfileinfo",
  "movegui",
  "movie",
  "movie2avi",
  "multibandread",
  "multibandwrite",
  "native2unicode",
  "nccreate",
  "ncdisp",
  "ncinfo",
  "ncread",
  "ncreadatt",
  "ncwrite",
  "ncwriteatt",
  "ncwriteschema",
  "noanimate",
  "notebook",
  "ode113",
  "ode15i",
  "ode15s",
  "ode23",
  "ode23s",
  "ode23t",
  "ode23tb",
  "ode45",
  "odeget",
  "odeset",
  "odextend",
  "open",
  "openfig",
  "opengl",
  "openvar",
  "ordeig",
  "ordqz",
  "ordschur",
  "padecoef",
  "pan",
  "parseSoapResponse",
  "pathtool",
  "pcode",
  "pdepe",
  "pdeval",
  "plotbrowser",
  "plotedit",
  "plottools",
  "printdlg",
  "printopt",
  "printpreview",
  "profsave",
  "propedit",
  "propertyeditor",
  "psi",
  "publish",
  "qmr",
  "quad2d",
  "rbbox",
  "reducepatch",
  "reducevolume",
  "readasync",
  "rng",
  "rotate",
  "rotate3d",
  "scatteredInterpolant",
  "selectmoveresize",
  "sendmail",
  "serial",
  "serialbreak",
  "setpixelposition",
  "showplottool",
  "smooth3",
  "snapnow",
  "sound",
  "soundsc",
  "ss2tf",
  "startup",
  "stopasync",
  "stream2",
  "stream3",
  "streamline",
  "streamparticles",
  "streamribbon",
  "streamslice",
  "streamtube",
  "strings",
  "subvolume",
  "superclasses",
  "surf2patch",
  "symmlq",
  "syntax",
  "texlabel",
  "textwrap",
  "tfqmr",
  "timer",
  "timeseries",
  "todatenum",
  "toolboxdir",
  "triangulation",
  "tscollection",
  "tstool",
  "uibuttongroup",
  "uicontextmenu",
  "uicontrol",
  "uigetpref",
  "uiimport",
  "uiopen",
  "uipanel",
  "uipushtool",
  "uiresume",
  "uisave",
  "uisetcolor",
  "uisetfont",
  "uisetpref",
  "uistack",
  "uitable",
  "uitoggletool",
  "uitoolbar",
  "uiwait",
  "undocheckout",
  "unicode2native",
  "unloadlibrary",
  "unmesh",
  "userpath",
  "validateattributes",
  "verctrl",
  "verLessThan",
  "viewmtx",
  "visdiff",
  "volumebounds",
  "web",
  "whatsnew",
  "winopen",
  "winqueryreg",
  "workspace",
  "xmlread",
  "xmlwrite",
  "xslt",
  "zoom",
  };
endfunction


%!test
%! str = __unimplemented__ ("no_name_function");
%! assert (isempty (str));
%! str = __unimplemented__ ("quad2d");
%! assert (str(1:51), "quad2d is not implemented.  Consider using dblquad.");
%! str = __unimplemented__ ("MException");
%! assert (str(1:58), "the 'MException' function is not yet implemented in Octave");
