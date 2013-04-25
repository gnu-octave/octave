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

  case "quad2d"
    txt = ["quad2d is not implemented.  Consider using dblquad."];

  case "gsvd"
    txt = ["gsvd is not currently part of core Octave.  See the ",...
    "linear-algebra package at ",...
    "@url{http://octave.sourceforge.net/linear-algebra/}."];

  case "funm"
    txt = ["funm is not currently part of core Octave.  See the ",...
    "linear-algebra package at @url{http://octave.sf.net/linear-algebra/}."];

  case "linprog"
    txt = ["Octave does not currently provide linprog.  ",...
    "Linear programming problems may be solved using @code{glpk}.  ",...
    "Try @code{help glpk} for more info."];

  case {"ode113", "ode15i", "ode15s", "ode23", "ode23s", "ode23t", "ode45", "odeget", "odeset"}
    txt = ["Octave provides lsode for solving differential equations.  ",...
    "For more information try @code{help lsode}.  ",...
    "Matlab-compatible ODE functions are provided by the odepkg package.  ",...
    "See @url{http://octave.sourceforge.net/odepkg/}."];

  case {"javaArray", "javaMethod", "javaMethodEDT", "javaObject", "javaObjectEDT", "javaaddpath", "javaclasspath", "javarmpath"}
    txt = ["Java objects and methods can be used with the java package. ",...
    "See @url{http://octave.sf.net/java/}."];

  case {"errordlg", "helpdlg", "inputdlg", "listdlg", "questdlg", "warndlg"}
    txt = ["Several dialog functions are provided in the java package. ",...
    "See @url{http://octave.sf.net/java/}."];

  case {"xlsread", "xlsfinfo", "xlswrite", "wk1read", "wk1finfo", "wk1write"}
    txt = ["Functions for spreadsheet style I/O (.xls .xlsx .sxc .ods .dbf .wk1 etc.) " , ...
    "are provided in the io package. ",...
    "See @url{http://octave.sf.net/io/}."];

  case {"avifile", "aviinfo", "aviread"}
    txt = ["Basic video file support is provided in the video package. ",...
    "See @url{http://octave.sf.net/video/}."];

  otherwise
    if (ismember (fcn, missing_functions ()))
      txt = sprintf ("the '%s' function is not yet implemented in Octave", fcn);
    else
      is_matlab_function = false;
      txt = "";
    endif
  endswitch

  if (is_matlab_function)
    txt = [txt, "\n\n@noindent\nPlease read ",...
           "@url{http://www.octave.org/missing.html} to learn how ",...
           "you can contribute missing functionality."];
    txt = __makeinfo__ (txt);
  endif

  if (nargout == 0)
    warning ("Octave:missing-function", "%s", txt);
  endif

endfunction

function list = missing_functions ()
  persistent list = {
  "DelaunayTri",
  "MException",
  "RandStream",
  "TriRep",
  "TriScatteredInterp",
  "align",
  "alim",
  "alpha",
  "alphamap",
  "annotation",
  "audiodevinfo",
  "audioplayer",
  "audiorecorder",
  "aufinfo",
  "auread",
  "auwrite",
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
  "checkout",
  "cholinc",
  "clearvars",
  "clipboard",
  "cmopts",
  "cmpermute",
  "cmunique",
  "colordef",
  "colormapeditor",
  "commandhistory",
  "commandwindow",
  "condeig",
  "coneplot",
  "contourslice",
  "createClassFromWsdl",
  "createSoapMessage",
  "customverctrl",
  "daqread",
  "datacursormode",
  "datatipinfo",
  "dbmex",
  "dde23",
  "ddeget",
  "ddesd",
  "ddeset",
  "decic",
  "depdir",
  "depfun",
  "deval",
  "dialog",
  "dither",
  "docopt",
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
  "fitsinfo",
  "fitsread",
  "flow",
  "frame2im",
  "freqspace",
  "funm",
  "gallery",
  "gammaincinv",
  "getframe",
  "getpixelposition",
  "grabcode",
  "graymon",
  "gsvd",
  "guidata",
  "guide",
  "guihandles",
  "handle",
  "hdf",
  "hdf5",
  "hdf5info",
  "hdf5read",
  "hdf5write",
  "hdfinfo",
  "hdfread",
  "hdftool",
  "helpbrowser",
  "helpdesk",
  "helpwin",
  "hgexport",
  "hgload",
  "hgsave",
  "hgsetget",
  "hgtransform",
  "hostid",
  "ilu",
  "im2frame",
  "im2java",
  "imapprox",
  "imformats",
  "import",
  "inmem",
  "inputParser",
  "inspect",
  "instrfind",
  "instrfindall",
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
  "matlabrc",
  "maxNumCompThreads",
  "memmapfile",
  "memory",
  "metaclass",
  "methodsview",
  "minres",
  "mlint",
  "mlintrpt",
  "mmfileinfo",
  "mmreader",
  "movegui",
  "movie",
  "movie2avi",
  "msgbox",
  "multibandread",
  "multibandwrite",
  "native2unicode",
  "noanimate",
  "ode113",
  "ode15i",
  "ode15s",
  "ode23",
  "ode23s",
  "ode23t",
  "ode23tb",
  "ode45",
  "odefile",
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
  "pagesetupdlg",
  "pan",
  "parseSoapResponse",
  "path2rc",
  "pathtool",
  "pcode",
  "pdepe",
  "pdeval",
  "playshow",
  "plotbrowser",
  "plotedit",
  "plottools",
  "prefdir",
  "preferences",
  "printdlg",
  "printopt",
  "printpreview",
  "profsave",
  "propedit",
  "propertyeditor",
  "publish",
  "qmr",
  "quad2d",
  "rbbox",
  "reducepatch",
  "reducevolume",
  "resample",
  "root",
  "rotate",
  "rotate3d",
  "selectmoveresize",
  "sendmail",
  "serial",
  "setpixelposition",
  "showplottool",
  "smooth3",
  "snapnow",
  "sound",
  "soundsc",
  "ss2tf",
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
  "support",
  "surf2patch",
  "symmlq",
  "syntax",
  "texlabel",
  "textwrap",
  "tfqmr",
  "timer",
  "timerfind",
  "timerfindall",
  "timeseries",
  "toolboxdir",
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
  "verLessThan",
  "viewmtx",
  "visdiff",
  "volumebounds",
  "waitfor",
  "wavfinfo",
  "wavplay",
  "wavrecord",
  "web",
  "whatsnew",
  "wk1finfo",
  "wk1read",
  "wk1write",
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
