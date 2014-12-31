## Copyright (C) 2014 Pantxo Diribarne
## 
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*- 
## @deftypefn {Function File} {@var{retval} =} genpropdoc (@var{OBJNAME}, @var{FILENAME})
##
## Print FILENAME texinfo source file associated to OBJNAME objects.
## This function is meant to be run for generating octave
## documentation (see doc/interpreter/graphics_properties.mk).
##
## All the hard coded documentation is written in getdoc
## function.  See the comments in getdoc bellow for instruction on how
## to document a graphics property.
## 
## @seealso{}
## @end deftypefn

function genpropdoc (objname, fname)  
  objnames = {"root", "figure", "axes", "line", ...
              "text", "image", "patch", "surface"};

  ## Base properties
  base = getstructure ("base");

  ## Object properties
  if (any (strcmp (objname, objnames)))
    obj = getstructure (objname, base);
  else
    error ("genpropdoc: unknown object %s", objname);
  endif

  ## Docstring
  str = printdoc (objname, obj);
  
  fid = fopen (fname,  "w+");
  if (fid < 0)
    error ("genpropdoc: couldn't open %s.", fname);
  endif
  
  fprintf (fid, str);
  fclose (fid);
endfunction

function s = getdoc (objname, field, base)
  ## Properties are represented by a struct with fields :
  ## 
  ## -"doc": string to be printed verbatim after being expanded
  ##   through expand_doc function.  Special keywords are:
  ##   "__objname__" further replaced by the current object name;
  ##   "__prop__"  further replaced by the current property name;
  ##   "__modemsg__"  further replaced by a message explaining that
  ##   the propmode will be toggled to "manual".
  ##   You may also cross reference properties using the label format
  ##   OBJNAMEPROPERTY, e.g, "@xref{XREFaxescolor, , axes color
  ##   property}." 
  ## 
  ## -"valid": string that describes valid values for the
  ##   current property.  Use "packopt" function to join options with
  ##   " | " separator and "markdef" to mark default among valid
  ##   values between curly braces. 
  ##   If not provided, valid values for radio properties are
  ##   automatically retrieved using set function.
  ## 
  ## -"default": string.  If not provided the default value is
  ##   automatically retrieved using get function.
  ##
  ## -"printdefault": a boolean (def. true) that specifies whether the
  ## default value should be printed.  It is useful for properties
  ## like root "screendepth" that default to screen dependant values. 

  packopt = @(c) strjoin (c, ' | ');
  markdef = @(s) ["@{" s "@}"];
  
  ## Some generic templates: 
  valid_color = "colorspec";
  valid_handle = "graphics handle";
  valid_string = "string";
  valid_fcn = packopt ({"string", "function handle"});
  valid_cellstring = packopt ({"string", "cell array of strings"});
  valid_2elvec = "two elements vector";
  valid_3elvec = "three elements vector";
  valid_4elvec = "four elements vector";
  valid_vecmat = packopt ({"vector", "matrix"});
  valid_scalmat = packopt ({"scalar", "matrix"});
  
  doc_notimpl =  "%s is not yet implemented for __objname__ \
objects.  __prop__ is unused.";
  doc_unused =  "__prop__ is unused.";


  ## Initialize structure
  if (isfield (base, field))
    s = base.(field);
  else
    s = struct ("valid", "", "default", "", "doc", "", ...
                "printdefault", true);
  endif

  ## Base properties: write generic documentation as it will, by
  ## default, be included in the list of each graphics object.
  ## If a given graphics object interprets the property differently
  ## than most others, the doc will have to be rewritten for this object. 
  if (strcmp (objname, "base"))
    switch field
      case "beingdeleted"
      case "busyaction"
      case "buttondownfcn"
        s.valid = valid_fcn;
      case "children"
        s.doc = "Graphics handles of the __objname__'s children.";
        s.valid = "vector of graphics handles";
        
      case "clipping"
        s.doc = "If __prop__ is @qcode{\"on\"}, the __objname__ is \
clipped in its parent axes limits.";
        
      case "createfcn"
        s.doc = "Callback functions to be executed right after \
the __objname__ has been created.  Those functions have to be set by \
default using e.g.  @code{set (0, \"default__objname__createfcn\", \
'disp (\"__objname__ created!\")') }.";
        s.valid = valid_fcn;
        
      case "deletefcn"
        s.doc = "Callback functions to be executed right before \
the __objname__ is deleted.";
        s.valid = valid_fcn;
        
      case "handlevisibility"
        s.doc = "If __prop__ is @qcode{\"off\"}, the __objname__'s \
handle is not visible into its parent's children list.";
        
      case "hittest"
      case "interruptible"
      case "parent"
        s.doc = "Handle of the parent graphics object.";
        s.valid = valid_handle;
        
      case "selected"
      case "selectionhighlight"
      case "tag"
        s.valid = valid_string;
        
      case "type"
        s.doc = "Class name of the graphics object.  __prop__ is \
always @qcode{\"__objname__\"}";
        s.valid = valid_string;
        s.printdefault = false;
        
      case "uicontextmenu"
      case "userdata"
      case "visible"
        s.doc = "If __prop__ is @qcode{\"off\"}, the __objname__ is \
not rendered on screen.";
    endswitch
    
  ## Root properties: 
  elseif (strcmp (objname, "root"))
    switch field
      ## Overridden shared properties
      case {"beingdeleted", "busyaction", "buttondownfcn", ...
            "clipping", "createfcn", "deletefcn", "handlevisibility", ...
            "hittest", "interruptible", "selected", ...
            "selectionhighlight", "uicontextmenu", "visible"}
        s.doc = doc_unused;
        
      case "parent"
        s.doc = "Root figure has no parent graphics object.  __prop__ \
is always empty.";
        
      ## Specific properties
      case "callbackobject"
        s.valid = valid_handle;
        
      case "commandwindowsize"
      case "currentfigure"
        s.doc = "Graphics handle of the current figure.";
        s.valid = valid_handle;
        
      case "diary"
        s.doc = "If __prop__ is @qcode{\"on\"}, the octave \
command window session is saved to file.  @xref{XREFrootdiaryfile, , \
diaryfile property}.";
        s.valid = valid_string;
        
      case "diaryfile"
        s.doc = "The name of the diary file.  \
@xref{XREFdiary, , diary function}.";
        s.valid = valid_string;
        
      case "echo"
        s.doc = "Control whether octave displays commands executed \
from scripts.  @xref{XREFecho, , echo function}.";
        
      case "errormessage"
        s.doc = "The last error message octave raised.  \
@xref{XREFlasterr, , lasterr function}.";
        s.valid = valid_string;
        
      case "fixedwidthfontname"
        s.valid = valid_string;
        
      case "format"
        s.doc = "This property is a wrapper around @code{format} \
function.  @xref{XREFformat, , format function}.";
        
      case "formatspacing"
        s.doc = "This property is a wrapper around @code{format} \
function.  @xref{XREFformat, , format function}.";
        
      case "language"
        s.valid = valid_string;
        
      case "monitorpositions"
        s.valid = valid_4elvec;
        
      case "pointerlocation"
        s.valid = valid_2elvec;
        
      case "pointerwindow"
        s.valid = valid_handle;
        
      case "recursionlimit"
        s.doc = "The maximum number of times a function can be \
called recursively.  @xref{XREFmax_recursion_depth, , \
max_recursion_depth function}.";
        s.valid = "double";
        
      case "screendepth"
        s.valid = "double";
        s.printdefault = false;
        
      case "screenpixelsperinch"
        s.valid = "double";
        s.printdefault = false;
        
      case "screensize"
        s.valid = valid_4elvec;
        s.printdefault = false;
        
      case "showhiddenhandles"
        s.doc = "If __prop__ is @qcode{\"on\"}, all graphics objects \
handle are visible in their parents' children list, regardless of \
the value of their @code{handlevisibility} property."; 
        
      case "units"
    endswitch

  ## Figure properties
  elseif (strcmp (objname, "figure"))
    switch field
      ## Overridden shared properties
      case "clipping"
        s.doc = doc_unused;

      ## Specific properties
      case "alphamap"
        s.doc = sprintf (doc_notimpl, "Transparency");
      case "closerequestfcn"
        s.valid = valid_fcn;
        
      case "color"
        s.doc = "Color of the figure background.  @xref{Colors, , \
colorspec}.";
        s.valid = valid_color;
        
      case "colormap"
        s.doc = "A matrix containing the RGB color map for \
the current axes.";
        s.valid = "N-by-3 matrix";
        
      case "currentaxes"
        s.doc = "Handle to the graphics object of the current axes.";
        s.valid = valid_handle;
        
      case "currentcharacter"
        s.doc = doc_unused;
        
      case "currentobject"
        s.valid = valid_handle;
        
      case "currentpoint"
        s.doc = "A 1-by-2 matrix which holds the coordinates of \
the point over which the mouse pointer was when a mouse event \
occurred.  The x and y coordinates are in units defined by the \
figures @qcode{\"units\"} property and their origin is the lower \
left corner of the plotting area.\
\n\nEvents which set @qcode{\"currentpoint\"} are\n\
@table @asis\n\
@item A mouse button was pressed\n\
always\n\
@item A mouse button was released\n\
only if the figures callback @qcode{\"windowbuttonupfcn\"} is defined\n\
@item The pointer was moved while pressing mouse button (drag)\n\
 only if the figures callback @qcode{\"windowbuttonmotionfcn\"} is \
defined \n\
@end table";
        s.valid = valid_2elvec;
        
      case "dockcontrols"
        s.doc = doc_unused;
        
      case "doublebuffer"
      case "filename"
      case "integerhandle"
      case "inverthardcopy"
      case "keypressfcn"
        s.valid = valid_fcn;
        
      case "keyreleasefcn"
        s.doc = "With @code{keypressfcn}, the keyboard callback \
functions.  These callback functions get called when a key is \
pressed/released respectively.  The functions are called with two \
input arguments.  The first argument holds the handle of the calling \
figure.  The second argument holds the event structure which has the \
following members:\n\
@table @code\n\
@item Character:\n\
The ASCII value of the key\n\
@item Key:\n\
lowercase value of the key\n\
@item Modifier:\n\
A cell array containing strings representing the modifiers pressed \
with the key.\n\
@end table";
        s.valid = valid_fcn;
        
      case "menubar"
      case "mincolormap"
      case "name"
        s.doc = "Name to be displayed in the figure title bar.  If \
__prop__ is empty, the title of the figure is \"figure\" followed \
by the figure handle value.";
        s.valid = valid_string;
        
      case "nextplot"
      case "numbertitle"
      case "outerposition"
        s.valid = valid_4elvec;
        
      case "paperorientation"
      case "paperposition"
        s.doc = "Vector @qcode{[x0 y0 width height]} defining the \
position of the figure (in @code{paperunits} units) in the printed \
page.  __modemsg__."; 
        s.valid = valid_4elvec;
        
      case "paperpositionmode"
        s.doc = "If __prop__ is set to @qcode{\"auto\"}, the \
@qcode{\"paperposition\"} property is automatically computed: the \
printed figure will have the same size as on-screen figure and will \
be centered in the output page."; 
      case "papersize"
        s.doc = "Vector @qcode{[width height]} defining the size of the \
printing paper.  Setting this property forces the @code{papertype} \
property to be set to @qcode{\"<custom>\"}.";
        s.valid = valid_2elvec;
        
      case "papertype"
        s.doc = "Name of the paper to be used for printed output.  \
Setting __prop__ also changes @code{papersize} accordingly.";
        
      case "paperunits"
        s.doc = "The unit used to compute the @code{paperposition} \
property.";
        
      case "pointer"
      case "pointershapecdata"
        s.doc = doc_unused;
        
      case "pointershapehotspot"
        s.doc = doc_unused;
        
      case "position"
        s.valid = valid_4elvec;
        
      case "renderer"
      case "renderermode"
      case "resize"
      case "resizefcn"
        s.valid = valid_fcn;
        
      case "selectiontype"
      case "toolbar"
      case "units"
        s.doc = "The unit used to compute the @code{position} and \
@code{outerposition} properties.";
      case "windowbuttondownfcn" 
        s.doc = "@xref{XREFfigurewindowbuttonupfcn, , \
windowbuttonupfcn property}.";
        s.valid = valid_fcn;
        
      case "windowbuttonmotionfcn"
        s.doc = "@xref{XREFfigurewindowbuttonupfcn, , \
windowbuttonupfcn property}.";
        s.valid = valid_fcn;
        
      case "windowbuttonupfcn"
        s.doc = "With @code{windowbuttondownfcn} and \
@code{windowbuttonmotionfcn}, the mouse callback functions.  These \
callback functions get called when the mouse button is pressed, \
dragged, and released respectively.  When these callback functions \
are called, the @code{currentpoint} property holds the current \
coordinates of the cursor.";
        s.valid = valid_fcn;
        
      case "windowkeypressfcn"
        s.valid = valid_fcn;
        
      case "windowkeyreleasefcn"
        s.valid = valid_fcn;
        
      case "windowscrollwheelfcn"
        s.valid = valid_fcn;
        
      case "windowstyle"
      case "wvisual"
      case "wvisualmode"
      case "xdisplay"
      case "xvisual"
      case "xvisualmode"
      case "__graphics_toolkit__"
        s.doc = "The graphics toolkit that is used to render the \
figure.  @xref{XREFavailable_graphics_toolkits, , \
available_graphics_toolkits function}.";
    endswitch
    
  ## Axes properties
  elseif (strcmp (objname, "axes"))
    switch field
      ## Overridden shared properties
      case "clipping"
        s.doc = doc_unused;

      ## Specific properties
      case "activepositionproperty"
      case "alim"
        s.doc = sprintf (doc_notimpl, "Transparency");
      case "alimmode"
      case "ambientlightcolor"
        s.doc = sprintf (doc_notimpl, "Light");
      case "box"
        s.doc = "Control wether the axes has a surrounding box.";
        
      case "cameraposition"
        s.valid = valid_3elvec;
        
      case "camerapositionmode"
      case "cameratarget"
        s.valid = valid_3elvec;
        
      case "cameratargetmode"
      case "cameraupvector"
        s.valid = valid_3elvec;
        
      case "cameraupvectormode"
      case "cameraviewangle"
        s.valid = "scalar";
        
      case "cameraviewanglemode"
      case "clim"
        s.valid = valid_2elvec;
        s.doc = "Define the limits for the color axis of image \
children.  __modemsg__.  @xref{XREFpcolor, , pcolor function}.";
        
      case "climmode"
      case "color"
        s.doc = "Color of the axes background.  @xref{Colors, , \
colorspec}.";
        s.valid = valid_color;
        
      case "colororder"
        s.doc = "RGB values to be used by plot function for \
automatic line coloring.";
        s.valid = "N-by-3 RGB matrix";
        
      case "currentpoint"
        s.doc = "Matrix @qcode{[xf, yf, zf; xb, yb, zb]} which holds \
the coordinates of the point over which the mouse pointer was when \
the mouse button was pressed in axes data units.  If a mouse \
callback function is defined, @code{currentpoint} holds the \
pointer coordinates at the time the mouse button was pressed.  For \
3D plots, the first row of the returned matrix specifies the point \
nearest to the current camera position and the second rows the \
furthest point.  The two points forms a line which is perpendicular \
to the screen.";
        s.valid = "2-by-3 matrix";
        
      case "dataaspectratio"
        s.doc = "Specify the relative height and width of the data \
displayed in the axes.  Setting @code{dataaspectratio} to @samp{[1, \
2]} causes the length of one unit as displayed on the y-axis to be \
the same as the length of 2 units on the x-axis.  __modemsg__.";
        s.valid = valid_3elvec;
        
      case "dataaspectratiomode"
      case "drawmode"
      case "fontangle"
      case "fontname"
        s.doc = "Name of the font to be used for axes annotations.";
        s.valid = valid_string;
        
      case "fontsize"
        s.doc = "Size of the font to be used for axes annotations.  \
@xref{XREFaxesfontunits, , fontunits property}.";
        s.valid = "scalar";
        
      case "fontunits"
        s.doc = "Unit used to interpret @code{fontsize} property.";
        
      case "fontweight"
      case "gridlinestyle"
      case "interpreter"
      case "layer"
      case "linestyleorder"
      case "linewidth"
      case "minorgridlinestyle"
      case "nextplot"
      case "outerposition"
        s.doc = "Specify the position of the plot, including titles, \
axes and legend.  The four elements of the vector are the \
coordinates of the lower left corner and width and height of the \
plot, in units normalized to the width and height of the plot \
window.  For example, @qcode{[0.2, 0.3, 0.4, 0.5]} sets the lower \
left corner of the axes at @math{(0.2, 0.3)} and the width and \
height to be 0.4 and 0.5 respectively.  @xref{XREFaxesposition, , position property}.";
        s.valid = valid_4elvec;
        
      case "plotboxaspectratio"
      case "plotboxaspectratiomode"
      case "position"
        s.doc = "Specify the position of the plot, excluding titles, \
axes and legend.  The four elements of the vector are the \
coordinates of the lower left corner and width and height of the \
plot, in units normalized to the width and height of the plot \
window.  For example, @qcode{[0.2, 0.3, 0.4, 0.5]} sets the lower \
left corner of the axes at @math{(0.2, 0.3)} and the width and \
height to be 0.4 and 0.5 respectively.  @xref{XREFaxesouterposition, , \
outerposition property}."; 
        s.valid = valid_4elvec;
        
      case "projection"
      case "tickdir"
      case "tickdirmode"
      case "ticklength"
      case "tightinset"
      case "title"
        s.doc = "Graphics handle of the title text object.";
        s.valid = valid_handle;
        
      case "units"
      case "view"
        s.doc = "Specify the view point for three-dimensional plots";
        s.valid = valid_2elvec;
        
      case "xaxislocation"
      case "xcolor"
        s.doc = "Color of the x-axis.  @xref{Colors, , colorspec}.";
        s.valid = packopt ({markdef(valid_color), ...
                            "@qcode{\"none\"}"});
        
      case "xdir"
      case "xgrid"
        s.doc = "Control wether major x grid lines are displayed.";
        
      case "xlabel"
        s.doc = "Graphics handle of the x label text object.";
        s.valid = valid_handle;
        
      case "xlim"
        s.doc = "Specify the limits for x-axis.  __modemsg__.  \
@xref{XREFxlim, , xlim function}.";
        s.valid = valid_2elvec;
        
      case "xlimmode"
      case "xminorgrid"
        s.doc = "Control wether minor x grid lines are displayed.";
        
      case "xminortick"
      case "xscale"
      case "xtick"
        s.doc = "Position of x tick marks.  __modemsg__.";
        s.valid = "vector";
        
      case "xticklabel"
        s.doc = "Labels of x tick marks.  __modemsg__.";
        s.valid = valid_cellstring;
        
      case "xticklabelmode"
      case "xtickmode"
      case "yaxislocation"
      case "ycolor"
        s.doc = "Color of the y-axis.  @xref{Colors, , colorspec}.";
        s.valid = packopt ({markdef(valid_color), ...
                            "@qcode{\"none\"}"});
        
      case "ydir"
      case "ygrid"
        s.doc = "Control wether major y grid lines are displayed.";
        
      case "ylabel"
        s.doc = "Graphics handle of the y label text object.";
        s.valid = valid_handle;
        
      case "ylim"
        s.doc = "Specify the limits for y-axis.  __modemsg__.  \
@xref{XREFylim, , ylim function}.";
        s.valid = valid_2elvec;
        
      case "ylimmode"
      case "yminorgrid"
        s.doc = "Control wether minor y grid lines are displayed.";
        
      case "yminortick"
      case "yscale"
      case "ytick"
        s.doc = "Position of y tick marks.  __modemsg__.";
        s.valid = "vector";
        
      case "yticklabel"
        s.doc = "Labels of y tick marks.  __modemsg__.";
        s.valid = valid_cellstring;
        
      case "yticklabelmode"
      case "ytickmode"
      case "zcolor"
        s.doc = "Color of the z-axis.  @xref{Colors, , colorspec}.";
        s.valid = packopt ({markdef(valid_color), ...
                            "@qcode{\"none\"}"});
        
      case "zdir"
      case "zgrid"
        s.doc = "Control wether major z grid lines are displayed.";
        
      case "zlabel"
        s.doc = "Graphics handle of the z label text object.";
        s.valid = valid_handle;
        
      case "zlim"
        s.doc = "Specify the limits for z-axis.  __modemsg__.  \
@xref{XREFzlim, , zlim function}.";
        s.valid = valid_2elvec;
        
      case "zlimmode"
      case "zminorgrid"
        s.doc = "Control wether minor z grid lines are displayed.";
        
      case "zminortick"
      case "zscale"
      case "ztick"
        s.doc = "Position of z tick marks.  __modemsg__.";
        s.valid = "vector";
        
      case "zticklabel"
        s.doc = "Labels of z tick marks.  __modemsg__.";
        s.valid = valid_cellstring;
        
      case "zticklabelmode"
      case "ztickmode"
    endswitch
    
  ## Line properties
  elseif (strcmp (objname, "line"))
    switch field
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;
        
      ## Specific properties
      case "color"
        s.doc = "Color of the line object.  @xref{Colors, , \
colorspec}.";
        s.valid = valid_color;
        
      case "displayname"
        s.doc = "The text of the legend entry corresponding to this \
line.";
        s.valid = valid_cellstring;
        
      case "erasemode"
        s.doc = doc_unused;
        
      case "interpreter"
      case "linestyle"
        s.doc = "@xref{Line Styles}.";
        
      case "linewidth"
        s.doc = "Width in points of the line object.";
        
      case "marker"
        s.doc = "The shape of the marker to be used.  @xref{Marker \
Styles}.";
        
      case "markeredgecolor"
        s.doc = "Color of the edge of the markers.  If set \
@qcode{\"auto\"}, the markers edges have the same color as the line.  If \
set @qcode{\"none\"}, the markers edges are not displayed.  This property \
can also be set to any color.  @xref{Colors, , colorspec}.";
        
      case "markerfacecolor"
        s.doc = "Color of the face of the markers.  If set \
@qcode{\"auto\"}, the markers faces have the same color as the line.  If \
set @qcode{\"none\"}, the markers faces are not displayed.  This property \
can also be set to any color.  @xref{Colors, , colorspec}.";
        
      case "markersize"
        s.doc = "Size of the markers  in points.";
        s.valid = "scalar";
        
      case "xdata"
        s.doc = "Vector of x data to be plotted.";
        s.valid = "vector";
        
      case "xdatasource"
        s.valid = valid_string;
        s.doc = "Name of the vector in the current base workspace \
that should be used as x data.";
        
      case "ydata"
        s.doc = "Vector of y data to be plotted.";
        s.valid = "vector";
        
      case "ydatasource"
        s.valid = valid_string;
        s.doc = "Name of the vector in the current base workspace \
that should be used as y data.";
        
      case "zdata"
        s.doc = "Vector of z data to be plotted.";
        s.valid = "vector";
        
      case "zdatasource"
        s.valid = valid_string;
        s.doc = "Name of the vector in the current base workspace \
that should be used as z data.";
        
    endswitch

  ## Text properties
  elseif (strcmp (objname, "text"))
    switch field
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;

      ## Specific properties
      case "backgroundcolor"
        s.doc = sprintf (doc_notimpl, "Background area");
        s.valid = valid_color;
        
      case "color"
        s.doc = "Color of the text.  @xref{Colors, ,colorspec}.  ";
        s.valid = valid_color;
        
      case "displayname"
      case "edgecolor"
        s.doc = sprintf (doc_notimpl, "Background area");
        s.valid = valid_color;
        
      case "editing"
      case "erasemode"
        s.doc = doc_unused;
        
      case "extent"
      case "fontangle"
        s.doc = "Flag whether the font is italic or normal.  \
@code{fontangle} is currently unused.";
        
      case "fontname"
        s.doc = "The font used for the text.";
        s.valid = valid_string;
        
      case "fontsize"
        s.doc = "The font size of the text.";
        s.valid = "scalar";
        
      case "fontunits"
        s.doc = "The units used to interpret @code{fontsize} \
property.";
        
      case "fontweight"
        s.doc = "Flag whether the font is bold, etc.";
        
      case "horizontalalignment"
      case "interpreter"
      case "linestyle"
        s.doc = sprintf (doc_notimpl, "Background area");
        
      case "linewidth"
        s.doc = sprintf (doc_notimpl, "Background area");
        s.valid = "scalar";
        
      case "margin"
        s.doc = sprintf (doc_notimpl, "Background area");
        s.valid = "scalar";
        
      case "position"
        s.doc = "Vector @qcode{[X0 Y0 Z0]} where X0, Y0 and Z0 \
indicate the position of the text anchor as defined by \
@code{verticalalignment} and @code{horizontalalignment}.";
        s.valid = valid_4elvec;
        
      case "rotation"
        s.doc = "The angle of rotation for the displayed text, \
measured in degrees.";
        s.valid = "scalar";
        
      case "string"
        s.doc = "The text object string content.";
        s.valid = valid_string;
        
      case "units"
      case "verticalalignment"
    endswitch

  ## Image properties
  elseif (strcmp (objname, "image"))
    switch field
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;

      ## Specific properties
      case "alphadata"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = valid_scalmat;
        
      case "alphadatamapping"
        s.doc = sprintf (doc_notimpl, "Transparency");
        
      case "cdata"
        s.valid = "matrix";
        
      case "cdatamapping"
      case "displayname"
        s.doc = "The text of the legend entry corresponding to this \
image.";
        s.valid = valid_cellstring;
        
      case "erasemode"
        s.doc = doc_unused;
        
      case "xdata"
        s.doc = "Two element vector @qcode{[xmin xmax]} specifying the x \
coordinates of the first and last columns of the image.  \
\n\nSetting @code{xdata} empty matrix makes octave automatically \
affect it the value @qcode{[1 columns(image)]}.";
        s.valid = valid_2elvec;
        
      case "ydata"
        s.doc = "Vector @qcode{[ymin ymax]} specifying the y \
coordinates of the first and last columns of the image.  \
\n\nSetting @code{ydata} empty matrix makes octave automatically \
affect it the value @qcode{[1 rows(image)]}.";
        s.valid = valid_2elvec;
        
    endswitch
    
  ## Surface properties
  elseif (strcmp (objname, "surface"))
    switch field
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;

      ## Specific properties
      case "alphadata"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = valid_scalmat;
        
      case "alphadatamapping"
        s.doc = sprintf (doc_notimpl, "Transparency");
        
      case "ambientstrength"
        s.doc = sprintf (doc_notimpl, "Light");
        
      case "backfacelighting"
        s.doc = sprintf (doc_notimpl, "Light");
        
      case "cdata"
        s.valid = "matrix";
        
      case "cdatamapping"
      case "cdatasource"
      case "diffusestrength"
        s.doc = sprintf (doc_notimpl, "Light");
        
      case "displayname"
        s.doc = "The text of the legend entry corresponding to this \
surface.";
        
      case "edgealpha"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = "scalar";
        
      case "edgecolor"
      case "edgelighting"
        s.doc = sprintf (doc_notimpl, "Light");
        
      case "erasemode"
        s.doc = doc_unused;
      case "facealpha"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = valid_scalmat;
        
      case "facecolor"
      case "facelighting"
        s.doc = sprintf (doc_notimpl, "Light");
        
      case "interpreter"
      case "linestyle"
        s.doc = "@xref{Line Styles}.";
        
      case "linewidth"
        s.doc = "@xref{XREFlinelinewidth, , line linewidth \
property}.";
        
      case "marker"
        s.doc = "@xref{Marker Styles}.";
        
      case "markeredgecolor"
        s.doc = "@xref{XREFlinemarkeredgecolor, , line \
markeredgecolor property}."; 
        
      case "markerfacecolor"
        s.doc = "@xref{XREFlinemarkerfacecolor, , line \
markerfacecolor property}."; 
        
      case "markersize"
        s.doc = "@xref{XREFlinemarkersize, , line \
markersize property}."; 
        s.valid = "scalar";
        
      case "meshstyle"
      case "normalmode"
      case "specularcolorreflectance"
        s.doc = sprintf (doc_notimpl, "Light");
        
      case "specularexponent"
        s.doc = sprintf (doc_notimpl, "Light");
        
      case "specularstrength"
        s.doc = sprintf (doc_notimpl, "Light");
        
      case "vertexnormals"
      case "xdata"
        s.valid = "matrix";
        
      case "xdatasource"
      case "ydata"
        s.valid = "matrix";
        
      case "ydatasource"
      case "zdata"
        s.valid = "matrix";
        
      case "zdatasource"
    endswitch
    
  ## Patch properties
  elseif (strcmp (objname, "patch"))
    switch field
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;

      ## Specific properties
      case "alphadatamapping"
        s.doc = sprintf (doc_notimpl, "Transparency");
        
      case "ambientstrength"
        s.doc = sprintf (doc_notimpl, "Light");
        s.valid = "scalar";
        
      case "backfacelighting"
        s.doc = sprintf (doc_notimpl, "Light");
        
      case "cdata"
        s.doc = "Data defining the patch object color.\n\
Patch color can be defined for faces or for vertices.  \n\n\
If @code{cdata} is a scalar index into the current colormap or a RGB \
triplet, it defines the color of all faces.  \n\n\
If @code{cdata} is a N-by-1 vector of indices or a N-by-3 (RGB) \
matrix, it defines the color of each one of the N faces.\n\n\
If @code{cdata} is a N-by-M or a N-by-M-by-3 (RGB) \
matrix, it defines the color all vertices.";
        s.valid = valid_scalmat;
        
      case "diffusestrength"
        s.doc = sprintf (doc_notimpl, "Light");
        s.valid = "scalar";
        
      case "displayname"
        s.doc = "The text of the legend entry corresponding to this \
patch.";
      case "edgealpha"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = valid_scalmat;
        
      case "edgecolor"
      case "edgelighting"
        s.doc = sprintf (doc_notimpl, "Light");
        
      case "erasemode"
        s.doc = doc_unused;
        
      case "facealpha"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = valid_scalmat;
        
      case "facecolor"
        ## Don't provide a default value, and mark colorspec with
        ## braces, this forces the default rgb triplet to be displayed
        s.valid = packopt ({markdef(valid_color), ...
                            "@qcode{\"flat\"}", ...
                            "@qcode{\"none\"}", ...
                            "@qcode{\"interp\"}"});
        
      case "facelighting"
        s.doc = sprintf (doc_notimpl, "Light");
        
      case "faces"
      case "xdata"
        s.valid = valid_vecmat;
        
      case "facevertexalphadata"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = valid_scalmat;
        
      case "facevertexcdata"
      case "interpreter"
        s.doc = doc_unused;
        
      case "linestyle"
      case "linewidth"
      case "marker"
        s.doc = "@xref{XREFlinemarker, , line marker property}.";
        
      case "markeredgecolor"
        s.doc = "@xref{XREFlinemarkeredgecolor, , line \
markeredgecolor property}."; 
        
      case "markerfacecolor"
        s.doc = "@xref{XREFlinemarkerfacecolor, , line \
markerfacecolor property}."; 
        
      case "markersize"
        s.doc = "@xref{XREFlinemarkersize, , line \
markersize property}."; 
        s.valid = "scalar";
        
      case "normalmode"
      case "specularcolorreflectance"
        s.doc = sprintf (doc_notimpl, "Light");
        s.valid = "scalar";
        
      case "specularexponent"
        s.doc = sprintf (doc_notimpl, "Light");
        s.valid = "scalar";
        
      case "specularstrength"
        s.doc = sprintf (doc_notimpl, "Light");
        s.valid = "scalar";
        
      case "vertexnormals"
      case "vertices"
        s.valid = valid_vecmat;
        
      case "xdata"
        s.valid = valid_vecmat;
        
      case "ydata"
        s.valid = valid_vecmat;
        
      case "zdata"
        s.valid = valid_vecmat;
        
    endswitch
  endif
  
  ## Replace keywords
  if (! isempty (s.doc) && ! strcmp (objname, "base"))
    s.doc = expand_doc (s.doc, field, objname);
  endif
  
endfunction

function strout = expand_doc (strin, field, objname)
  strout = strrep (strin, "__objname__", objname);
  strout = strrep (strout, "__prop__", ["@code{" field "}"]);

  modemsg = "Setting @code{%s} also forces the @code{%smode} \
property to be set to @qcode{\"manual\"}";
  modemsg = sprintf (modemsg, field, field);
  strout = strrep (strout, "__modemsg__", modemsg);
endfunction

function s = getstructure (objname, base = [])
  hf = [];
  if (! strcmp (objname, "root"))
    ## Use an improbable figure number to avoid ishandle to return
    ## true for 1 
    hf = figure (2265465, "visible", "off");
  endif

  ## Build a default object to extract its properties list and default
  ## values.
  if (strcmp (objname, "base"))
    ## Base properties are extracted from hggroup that only have 2
    ## additional regular (non-hidden) properties, "displayname" and
    ## "erasemode".
    h = hggroup ();
  elseif (strcmp (objname, "root"))
    h = 0;
  elseif (strcmp (objname, "figure"))
    h = hf;
  else
    eval (["h = " objname " ();"]);
  endif
  
  gprop = get (h);
  sprop = set (h);

  fields = fieldnames (gprop);
  nf = numel (fields);
  args = cell (2*nf, 1);
  for ii = 1:nf
    field = fields{ii};

    ## Get hard coded documentation
    val = getdoc (objname, field, base);
    
    ## Extract the default values that are not hard coded in getdoc
    if (isempty (val.default) && val.printdefault)
      val.default = getdefault (h, objname, field);
    endif
    
    val.isreadonly = ! isfield (sprop, field);

    ## Extract the valid values that are not hard coded in getdoc
    if (! val.isreadonly && isempty (val.valid))
      val.valid = sprop.(field);
      if (! isempty (val.valid) &&
          iscellstr (val.valid))
        ## Add double quotes around string radio properties
        val.valid = cellfun (@(s) ["@qcode{\"" s "\"}"], val.valid,
                             "uniformoutput", false);
        val.valid = strjoin (val.valid, ' | ');
      endif
    endif
    
    args{2*(ii-1)+1} = field;
    args{2*ii} = val;
  endfor

  ## Build struct and remove unused fields in base properties
  s = struct (args{:});

  if (strcmp (objname, "base"))
    s = rmfield (s, {"displayname", "erasemode"});
  endif

  if (isfigure (hf))
    close (hf)
  endif
endfunction

function def = getdefault (h, objname, field)         
  ## This function is meant to be run without initialization file so
  ## that the properties we get are the default.
  def = get (h, field);
  
  ## Don't print default values for graphics handles
  if (isscalar (def) && def != 0 && ishandle (def))
    def = "";
  else
    if (ischar (def))
      def = ["@qcode{\"" def "\"}"];
    else
      if ((isvector (def) && numel (def) < 5) ||
          isempty (def))
        ## Use disp to print the default value for short vectors and
        ## empty values
        str = disp (def);
        str(end) = [];          # remove linefeed
        str = strtrim (str);    # remove leading space

        ## Add [] around vector values
        if (ismatrix (def) && numel (def) > 1)
          str = ["[" str "]"];
          ## Add ";" between columns vector values
          if (rows (def) != 1)
            str = strrep (str, "\n", "; ");
          endif
        endif
        
        ## Replace texinfo reserved characters
        def = strrep (str, "{", "@{");
        def = strrep (def, "}", "@}");
        def = strrep (def, "@", "@@");
        
        def = ["@code{" def "}"];
      else
        args = arrayfun (@(x) num2str (x), size (def),
                         "uniformoutput", false); 
        def = [strjoin(args, "-by-") " " class(def)];
      endif
    endif
  endif
endfunction

function str = printdoc (objname, obj)
  ## Sort fields so that they appear in alphabetic order in the manual
  fields = sort (fieldnames (obj));
  nf = numel (fields);

  ## File header and begining of properties table
  str = [warn_autogen() "\n\n@table @asis"];

  
  for ii = 1:nf 
    field = fields{ii};
    str = sprintf ("%s\n\n", str);

    ## @anchor: cross reference using XREFobjnamefield label
    ## Concept index: call info from octave with
    ## 'doc ("objname field")' 
    str = sprintf ("%s@anchor{XREF%s%s}\n@cindex %s %s\n",
                   str, objname, field, objname, field);

    ## Item
    str = sprintf ("%s@item @code{%s}", str, field);

    ## Mark item read-only if needed
    if (obj.(field).isreadonly)
      str = sprintf ("%s (read-only):", str);
    else
      str = sprintf ("%s:", str);
    endif
    
    ## Print valid and default values
    tmp = print_options (obj.(field).valid,
                         obj.(field).default);
    if (! isempty (tmp))
      str = sprintf ("%s %s\n", str, tmp);
    else
      str = sprintf ("%s\n", str);
    endif

    ## Print documentation
    str = sprintf ("%s%s\n", str, obj.(field).doc);
  endfor
  
  ## End of properties table
  str = sprintf ("%s\n@end table", str);
endfunction

function str = warn_autogen ()
  str = "@c DO NOT EDIT!  Generated automatically by genpropdoc.m.\n\
\n\
@c Copyright (C) 2014 Pantxo Diribarne\n\
@c\n\
@c This file is part of Octave.\n\
@c\n\
@c Octave is free software; you can redistribute it and/or modify it\n\
@c under the terms of the GNU General Public License as published by the\n\
@c Free Software Foundation; either version 3 of the License, or (at\n\
@c your option) any later version.\n\
@c\n\
@c Octave is distributed in the hope that it will be useful, but WITHOUT\n\
@c ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or\n\
@c FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License\n\
@c for more details.\n\
@c\n\
@c You should have received a copy of the GNU General Public License\n\
@c along with Octave; see the file COPYING.  If not, see\n\
@c <http://www.gnu.org/licenses/>.";
endfunction

function str = print_options (val, default)
  str = "";
  if (! isempty (val))
    tmp = strrep (val, default, ["@{" default "@}"]);
    if (length (tmp) == length (val) && ! isempty (default))
      str = [tmp ", def. " default];
    else
      str = tmp;
    endif
  elseif (! isempty (default))
    str = ["def. " default];
  endif
endfunction
