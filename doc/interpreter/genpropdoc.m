########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {@var{retval} =} genpropdoc (@var{OBJNAME}, @var{FILENAME})
##
## Print FILENAME texinfo source file associated to OBJNAME objects.  This
## function is meant to be run for generating octave documentation
## (see doc/interpreter/graphics_properties.mk).
##
## All the hard coded documentation is written in getdoc function.  See the
## comments in getdoc bellow for instruction on how to document a graphics
## property.
##
## @seealso{}
## @end deftypefn

function genpropdoc (objname, fname = "", props = {})
  objnames = {"root", "figure", "axes", "legend", ...
              "image", "light", "line", "patch", "scatter", "surface", ...
              "text", "uibuttongroup", "uicontextmenu", "uicontrol", ...
              "uipanel", "uimenu", "uipushtool", "uitable", ...
              "uitoggletool", "uitoolbar"
             };

  ## Base properties
  base = getstructure ("base");

  ## Object properties
  if (any (strcmp (objname, objnames)))
    obj = getstructure (objname, base, props);
  else
    error ("genpropdoc: unknown object %s", objname);
  endif

  ## Docstring
  str = printdoc (objname, obj, ! isempty (props));

  if (! isempty (fname))
    fid = fopen (fname,  "w+");
    if (fid < 0)
      error ("genpropdoc: couldn't open %s.", fname);
    endif
  else
    fid = stdout;
  endif

  fprintf (fid, str);

  if (nargin == 2)
    fclose (fid);
  endif

endfunction

function s = getdoc (objname, field, base)
  ## Properties are represented by a struct with fields :
  ##
  ## -"doc": string to be printed verbatim after being expanded
  ##   through expand_doc function.  Special keywords are:
  ##   "__objname__" : replaced by the current object name;
  ##   "__prop__"    : replaced by the current property name;
  ##   "__modemsg__" : replaced by a message explaining that
  ##                   the propmode will be toggled to "manual".
  ##   You may also cross reference properties using the label format
  ##   OBJNAMEPROPERTY, e.g, "@xref{XREFaxescolor, , axes color property}."
  ##
  ## -"valid": string that describes valid values for the current property.
  ##   Use "packopt" function to join options with " | " separator
  ##   and "markdef" to mark default among valid values between curly braces.
  ##   If not provided, valid values for radio properties are automatically
  ##   retrieved using set function.
  ##
  ## -"default": string.  If not provided the default value is automatically
  ##   retrieved using get function.
  ##
  ## -"printdefault": a boolean (def. true) that specifies whether the
  ##   default value should be printed.  It is useful for properties
  ##   like root "screendepth" that default to screen dependent values.

  packopt = @(c) strjoin (c, " | ");
  markdef = @(s) ["@{" s "@}"];

  ## Some generic templates:
  valid_color = "colorspec";
  valid_handle = "graphics handle";
  valid_string = "string";
  valid_fcn = packopt ({"string", "function handle"});
  valid_cellstring = packopt ({"string", "cell array of strings"});
  valid_2elvec = "two-element vector";
  valid_3elvec = "three-element vector";
  valid_4elvec = "four-element vector";
  valid_vecmat = packopt ({"vector", "matrix"});
  valid_scalmat = packopt ({"scalar", "matrix"});

  doc_notimpl = "%s is not yet implemented for __objname__ objects.  \
__prop__ is unused.";
  doc_unused =  "__prop__ is unused.";

  doc_fontangle = "Control whether the font is italic or normal.";
  doc_fontsize = "Size of the font used for text rendering.  \
@xref{XREF__objname__fontunits, , fontunits property}.";
  doc_fontname = "Name of font used for text rendering.  When setting \
this property, the text rendering engine will search for a matching \
font in your system.  If none is found then text is rendered using a \
default sans serif font (same as the default @qcode{\"*\"} value).\n\n\
Programming Note: On systems that don’t use FontConfig natively \
(all but Linux), the font cache is built when Octave is installed.  \
You will need to run @code{system (\"fc-cache -fv\")} manually after \
installing new fonts.";
  doc_fontunits = "Units used to interpret the @qcode{\"fontsize\"} property.";
  doc_fontweight = "Control the variant of the base font used for \
text rendering.";

  ## Initialize structure
  if (isfield (base, field))
    s = base.(field);
  else
    s = struct ("valid", "", "default", "", "doc", "", "printdefault", true);
  endif

  ## Base properties: Write generic documentation because it will be included
  ## in the list of each graphics object.  If a given graphics object
  ## interprets the property differently than others, then the doc will have
  ## to be rewritten for this object.
  if (strcmp (objname, "base"))
    switch (field)
      case "beingdeleted"
      case "busyaction"
        s.doc = "Define how Octave handles the execution of this object's \
callback properties when it is unable to interrupt another object's \
executing callback.  This is only relevant when the currently executing \
callback object has its @code{interruptible} property set to \
\@qcode{\"off\"}.  The __prop__ property of the interrupting callback object \
indicates whether the interrupting callback is queued (@qcode{\"queue\"} \
(default)) or discarded (@qcode{\"cancel\"}).\n\
@xref{Callbacks, , @w{Callbacks section}}.";

      case "buttondownfcn"
        s.doc = "__fcnmsg__";
        s.valid = valid_fcn;

      case "children"
        s.doc = "Graphics handles of the __objname__'s children.";
        s.valid = "vector of graphics handles";

      case "clipping"
        s.doc = "If __prop__ is @qcode{\"on\"}, the __objname__ is \
clipped in its parent axes limits.";

      case "contextmenu"
        s.doc = "Graphics handle of the uicontextmenu object that is \
currently associated to this __objname__ object.";
        s.valid = valid_handle;

      case "createfcn"
        s.doc = "Callback function executed immediately after __objname__ \
has been created.  Function is set by using default property on root object, \
e.g., @code{set (groot, \"default__objname__createfcn\", \
'disp (\"__objname__ created!\")')}.\n\n__fcnmsg__";
        s.valid = valid_fcn;

      case "deletefcn"
        s.doc = "Callback function executed immediately before __objname__ \
is deleted.\n\n__fcnmsg__";
        s.valid = valid_fcn;

      case "handlevisibility"
        s.doc = "If __prop__ is @qcode{\"off\"}, the __objname__'s \
handle is not visible in its parent's \"children\" property.";

      case "hittest"
        s.doc = "Specify whether __objname__ processes mouse events \
or passes them to ancestors of the object.  When enabled, the object may \
respond to mouse clicks by evaluating the @qcode{\"buttondownfcn\"}, showing \
the uicontextmenu, and eventually becoming the root \
@qcode{\"currentobject\"}.  This property is only relevant when the object \
can accept mouse clicks which is determined by the @qcode{\"pickableparts\"} \
property.  @xref{XREF__objname__pickableparts, , @w{pickableparts property}}.";

      case "interruptible"
        s.doc = "Specify whether this object's callback functions may be \
interrupted by other callbacks.  By default __prop__ is @qcode{\"on\"} \
and callbacks that make use of @code{drawnow}, @code{figure}, @code{waitfor}, \
@code{getframe} or @code{pause} functions are eventually interrupted.\n\
@xref{Callbacks, , @w{Callbacks section}}.";

      case "parent"
        s.doc = "Handle of the parent graphics object.";
        s.valid = valid_handle;

      case "pickableparts"
        s.doc = "Specify whether __objname__ will accept mouse clicks.  \
By default, __prop__ is @qcode{\"visible\"} and only visible parts of the \
__objname__ or its children may react to mouse clicks.  When __prop__ is \
@qcode{\"all\"} both visible and invisible parts (or children) may react to \
mouse clicks.  When __prop__ is @qcode{\"none\"} mouse clicks on the object \
are ignored and transmitted to any objects underneath this one.  When an \
object is configured to accept mouse clicks the @qcode{\"hittest\"} property \
will determine how they are processed.  \
@xref{XREF__objname__hittest, , @w{hittest property}}.";

      case "selected"
      case "selectionhighlight"
      case "tag"
        s.doc = "A user-defined string to label the graphics object.";
        s.valid = valid_string;

      case "type"
        s.doc = "Class name of the graphics object.  __prop__ is \
always @qcode{\"__objname__\"}";
        s.valid = valid_string;
        s.printdefault = false;

      case "userdata"
        s.doc = "User-defined data to associate with the graphics object.";
        s.valid = "Any Octave data";

      case "visible"
        s.doc = "If __prop__ is @qcode{\"off\"}, the __objname__ is \
not rendered on screen.";
    endswitch

  ## Root properties:
  elseif (strcmp (objname, "root"))
    switch (field)
      ## Overridden shared properties
      case {"beingdeleted", "busyaction", "buttondownfcn", ...
            "clipping", "createfcn", "deletefcn", "handlevisibility", ...
            "hittest", "interruptible", "selected", ...
            "selectionhighlight", "uicontextmenu", "visible"}
        s.doc = doc_unused;

      case "parent"
        s.doc = "Root object has no parent graphics object.  __prop__ \
is always empty.";

      case "hittest"
        s.doc = doc_unused;

      case "pickableparts"
        s.doc = doc_unused;

      ## Specific properties
      case "callbackobject"
        s.doc = "Graphics handle of the current object whose callback is executing.";
        s.valid = valid_handle;

      case "currentfigure"
        s.doc = "Graphics handle of the current figure.";
        s.valid = valid_handle;

      case "diary"
        s.doc = "If __prop__ is @qcode{\"on\"}, the Octave command window \
session is saved to file.  @xref{XREFrootdiaryfile, , @w{diaryfile property}}.";

      case "diaryfile"
        s.doc = "The name of the diary file.  \
@xref{XREFdiary, , @w{diary function}}.";
        s.valid = valid_string;

      case "echo"
        s.doc = "Control whether Octave displays commands executed from \
scripts.  @xref{XREFecho, , @w{echo function}}.";

      case "errormessage"
        s.doc = "The last error message raised.  \
@xref{XREFlasterr, , @w{lasterr function}}.";
        s.valid = valid_string;

      case "fixedwidthfontname"
        s.valid = valid_string;

      case "format"
        s.doc = "This property is a wrapper around the @code{format} function.\
  @xref{XREFformat, , @w{format function}}.";

      case "formatspacing"
        s.doc = "This property is a wrapper around the @code{format} function.\
  @xref{XREFformat, , @w{format function}}.";

      case "language"
        s.valid = valid_string;

      case "monitorpositions"
        s.doc = doc_unused;
        s.printdefault = false;

      case "pointerlocation"
        s.doc = doc_unused;
        s.valid = valid_2elvec;

      case "pointerwindow"
        s.doc = doc_unused;
        s.valid = valid_handle;

      case "recursionlimit"
        s.doc = "The maximum number of times a function can be called \
recursively.  \
@xref{XREFmax_recursion_depth, , @w{max_recursion_depth function}}.";
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
        s.doc = "If __prop__ is @qcode{\"on\"}, all graphics objects handles \
are visible in their parents' children list, regardless of the value of their \
@code{handlevisibility} property.";

      case "units"
    endswitch

  ## Figure properties
  elseif (strcmp (objname, "figure"))
    switch (field)
      ## Overridden shared properties
      case "clipping"
        s.doc = doc_unused;

      case "pickableparts"
        s.doc = doc_unused;

      ## Specific properties
      case "alphamap"
        s.doc = sprintf (doc_notimpl, "Transparency");

      case "closerequestfcn"
        s.doc = "Function that is executed when a figure is deleted.  \
@xref{XREFclosereq, , closereq function}.\n\n__fcnmsg__";
        s.valid = valid_fcn;

      case "color"
        s.doc = "Color of the figure background.  \
@xref{Colors, , colorspec}.";
        s.valid = valid_color;

      case "colormap"
        s.doc = "A matrix containing the RGB color map for the current axes.";
        s.valid = "N-by-3 matrix";

      case "currentaxes"
        s.doc = "Handle to the graphics object of the current axes.";
        s.valid = valid_handle;

      case "currentcharacter"
        s.doc = doc_unused;

      case "currentobject"
        s.valid = valid_handle;

      case "currentpoint"
        s.doc = "A 1-by-2 matrix which holds the coordinates of the point \
over which the mouse pointer was when a mouse event occurred.  The X and Y \
coordinates are in units defined by the figure's @code{units} property \
and their origin is the lower left corner of the plotting area.\n\
\n\
Events which set @code{currentpoint} are\n\
@table @asis\n\
@item A mouse button was pressed\n\
always\n\
@item A mouse button was released\n\
only if the figure's callback @code{windowbuttonupfcn} is defined\n\
@item The pointer was moved while pressing the mouse button (drag)\n\
only if the figure's callback @code{windowbuttonmotionfcn} is defined\n\
@end table";
        s.valid = valid_2elvec;

      case "dockcontrols"
        s.doc = doc_unused;

      case "filename"
        s.doc = "The filename used when saving the plot figure.";
        s.valid = valid_string;

      case "graphicssmoothing"
        s.doc = "Use smoothing techniques to reduce the appearance of jagged lines.";

      case "integerhandle"
        s.doc = "Assign the next lowest unused integer as the Figure number.";

      case "inverthardcopy"
        s.doc = "Replace the figure and axes background color with white when printing.";

      case "keypressfcn"
        s.doc = "Callback function executed when a keystroke event \
happens while the figure has focus. The actual key that was pressed \
can be retrieved using the second argument 'evt' of the function.  __fcnmsg__";
        s.valid = valid_fcn;

      case "keyreleasefcn"
        s.doc = "With @code{keypressfcn}, the keyboard callback functions.  \
These callback functions are called when a key is pressed/released \
respectively.  The functions are called with two input arguments.  The first \
argument holds the handle of the calling figure.  The second argument holds \
an event structure which has the following members:\n\
@table @code\n\
@item Character:\n\
The ASCII value of the key\n\
@item Key:\n\
Lowercase value of the key\n\
@item Modifier:\n\
A cell array containing strings representing the modifiers pressed with the \
key.\n\
@end table\
\n\n__fcnmsg__";
        s.valid = valid_fcn;

      case "menubar"
        s.doc = "Control the display of the figure menu bar at the top \
of the figure.";

      case "name"
        s.doc = "Name to be displayed in the figure title bar.  The name is \
displayed to the right of any title determined by the @code{numbertitle} \
property.";
        s.valid = valid_string;

      ## FIXME: Uncomment when support added in graphics.in.h
      #case "number"
      #  s.doc = "Number of current figure (RO).";

      case "nextplot"
        s.doc = "__prop__ is used by high level plotting functions to \
decide what to do with axes already present in the figure.  \
@xref{XREFnewplot, , @w{newplot function}}.";

      case "numbertitle"
        s.doc = "Display \"Figure\" followed by the numerical figure handle \
value in the figure title bar.";

      case "outerposition"
        s.doc = "Specify the position and size of the figure including \
the top menubar and the bottom status bar.  \
The four elements of the vector are the coordinates of the lower left corner \
and width and height of the figure.  \
@xref{XREFfigureunits, , @w{units property}}.";
        s.valid = valid_4elvec;

      case "paperorientation"
        s.doc = "The value for the @code{papersize}, and @code{paperposition} \
properties depends upon __prop__.  The horizontal and vertical values for \
@code{papersize} and @code{paperposition} reverse order \
when __prop__ is switched between @code{\"portrait\"} and \
@code{\"landscape\"}.";

      case "paperposition"
        s.doc = "Vector @code{[left bottom width height]} defining the \
position and size of the figure (in @code{paperunits} units) on the printed \
page.  The position @code{[left bottom]} defines the lower left corner of the \
figure on the page, and the size is defined by @code{[width height]}.  For \
output formats not implicitly rendered on paper, @code{width} and \
@code{height} define the size of the image and the position information is \
ignored.  \
__modemsg__.";
        s.valid = valid_4elvec;

      case "paperpositionmode"
        s.doc = "If __prop__ is set to @qcode{\"auto\"}, the \
@code{paperposition} property is automatically computed: the printed \
figure will have the same size as the on-screen figure and will be centered \
on the output page.  Setting the __prop__ to @code{\"auto\"} does not modify \
the value of the @code{paperposition} property.";

      case "papersize"
        s.doc = "Vector @code{[width height]} defining the size of the \
paper for printing.  Setting the __prop__ property to a value, not associated \
with one of the defined @code{papertypes} and consistent with the setting for \
@code{paperorientation}, forces the @code{papertype} property to the value \
@qcode{\"<custom>\"}.  If __prop__ is set to a value associated with a \
supported @code{papertype} and consistent with the @code{paperorientation}, \
the @code{papertype} value is modified to the associated value.";
        s.valid = valid_2elvec;

      case "papertype"
        s.doc = "Name of the paper used for printed output.  \
Setting __prop__ also changes @code{papersize}, while maintaining consistency \
with the @code{paperorientation} property.";

      case "paperunits"
        s.doc = "The unit used to compute the @code{paperposition} property.  \
The conversion from physical units (e.g., @code{\"inches\"}) is dependent on \
the @code{screenpixelsperinch} property of the root object.";

      case "pointer"
        s.doc = "Name of the mouse pointer shape associated with the canvas \
of the figure.  When __prop__ is \"custom\", the shape is determined by \
the @code{pointershapecdata} property.\n\n\
__prop__ has no effect when the figure is in zoom, pan, or rotate mode. \
In this case, Octave automatically uses a pointer shape appropriate \
to the mode.";

      case "pointershapecdata"
        s.doc ="m-by-m matrix defining a custom pointer.  Each \
element defines a pixel with the element (1,1) representing the \
top-left pixel.  A value of 1 is colored black, a value of 2 is colored white, \
and all other values are rendered as transparent.";
        s.valid = "16-by-16 or 32-by-32 Matrix";

      case "pointershapehotspot"
        s.doc ="For custom pointers only __prop__ defines the row and column \
of the pixel in @code{pointershapecdata} that is used as the pointer location.";
        s.valid = valid_2elvec;

      case "position"
        s.doc = "Specify the position and size of the figure canvas.  \
The four elements of the vector are the coordinates of the lower left corner \
and width and height of the figure.  \
@xref{XREFfigureunits, , @w{units property}}.";
        s.valid = valid_4elvec;

      case "renderer"
        s.doc = "Rendering engine used for printing when @code{renderermode} \
is \"manual\".  __modemsg__.";

      case "renderermode"
        s.doc = "Control whether the rendering engine used for printing is \
chosen automatically or specified by the @code{renderer} property.  \
@xref{XREFprint, , @w{print function}}.";

      case "resize"
        s.doc = "Control whether the figure can be resized by dragging the \
window borders and corners using a mouse.  When __prop__ is @qcode{\"off\"} \
mouse interactions are disabled but the figure can still be resized by \
changing its @qcode{\"position\"} property.";

      case "resizefcn"
        s.doc = "__prop__ is deprecated.  Use @code{sizechangedfcn} instead.";
        s.valid = valid_fcn;

      case "selectiontype"
        s.doc = "Selection type of the latest mouse click.\n\n\
__prop__ may take different values depending on the combination of mouse \
button and keyboard modifier that were used:\n\
@table @code\n\
@item normal:\n\
Left-click.\n\
@item alt:\n\
Right-click or Ctrl+Left-click.\n\
@item extend:\n\
Shitf+Left-click, Middle click, or combined Left-click and Right-click.\n\
@item open:\n\
Double Left-click.\n\
@end table";

      case "sizechangedfcn"
        s.doc = "Callback triggered when the figure window size is changed.\
\n\n__fcnmsg__";
        s.valid = valid_fcn;

      case "toolbar"
        s.doc = "Control the display of the toolbar (along the bottom of the \
menubar) and the status bar.  When set to @qcode{\"auto\"}, the display is based on the value of the @code{menubar} property.";

      case "units"
        s.doc = "The unit used to compute the @code{position} and \
@code{outerposition} properties.";

      case "windowbuttondownfcn"
        s.doc = "@xref{XREFfigurewindowbuttonupfcn, , \
@w{windowbuttonupfcn property}}.";
        s.valid = valid_fcn;

      case "windowbuttonmotionfcn"
        s.doc = "@xref{XREFfigurewindowbuttonupfcn, , \
@w{windowbuttonupfcn property}}.";
        s.valid = valid_fcn;

      case "windowbuttonupfcn"
        s.doc = "With @code{windowbuttondownfcn} and \
@code{windowbuttonmotionfcn}, the mouse callback functions.  These \
callback functions are called when a mouse button is pressed, dragged, or \
released respectively.  When these callback functions are executed, the \
@code{currentpoint} property holds the current coordinates of the cursor.\
\n\n__fcnmsg__";
        s.valid = valid_fcn;

      case "windowkeypressfcn"
        s.doc = "Function that is executed when a key is pressed and \
the figure has focus.\n\n__fcnmsg__";
        s.valid = valid_fcn;

      case "windowkeyreleasefcn"
        s.doc = "Function that is executed when a key is released and \
the figure has focus.\n\n__fcnmsg__";
        s.valid = valid_fcn;

      case "windowscrollwheelfcn"
        s.doc = "Function that is executed when a user manipulates \
the mouse wheel over this figure.  \
The function is called with two input arguments.  The first \
argument holds the handle of the calling figure.  The second argument holds \
an event structure which has the following members:\n\
@table @code\n\
@item VerticalScrollCount:\n\
The number of wheel steps, typically 1 when scrolling down and -1 when \
scrolling up.\n\
@item VerticalScrollAmount:\n\
The number of lines a wheel step should scroll.  This value is always 3.\n\
@item EventName:\n\
The event name which is \"WindowScrollWheel\".\n\
@end table\
\n\n__fcnmsg__";
        s.valid = valid_fcn;

      case "windowstyle"
        s.doc = "The window style of a figure.  One of the following values:\n\
@table @code\n\
@item normal\n\
Set the window style as non modal.\n\
@item modal\n\
Set the window as modal so that it will stay on top of all normal figures.\n\
@item docked\n\
Setting the window style as docked currently does not dock the window.\n\
@end table\n\
\n\
Changing modes of a visible figure may cause the figure to close and reopen.";

    endswitch

  ## Axes properties
  elseif (strcmp (objname, "axes") || strcmp (objname, "legend"))
    switch (field)
      ## Overridden shared properties
      case "clipping"
        s.doc = doc_unused;

      ## Specific properties
      case "alim"
        s.doc = sprintf (doc_notimpl, "Transparency");

      case "alimmode"
      case "ambientlightcolor"
        s.doc = doc_unused;

      case "box"
        s.doc = "Control whether the __objname__ has a surrounding box.";

      case "boxstyle"
        s.doc = "For 3-D axes, control whether the @qcode{\"full\"} \
box is drawn or only the 3 @qcode{\"back\"} axes";

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
        s.doc = "Define the limits for the color axis of image children.  \
__modemsg__.  @xref{XREFpcolor, , @w{pcolor function}}.";
        s.valid = valid_2elvec;

      case "climmode"

      case "clippingstyle"
        s.doc = doc_unused;

      case "color"
        s.doc = "Color of the __objname__ background.  \
@xref{Colors, , colorspec}.";
        s.valid = valid_color;

      case "colororder"
        s.doc = "RGB values used by plot function for automatic line \
coloring.";
        s.valid = "N-by-3 RGB matrix";

      case "colororderindex"
        s.doc = doc_unused;

      case "currentpoint"
        s.doc = "Matrix @code{[xf, yf, zf; xb, yb, zb]} which holds the \
coordinates (in axes data units) of the point over which the mouse pointer \
was when the mouse button was pressed.  If a mouse callback function is \
defined, @code{currentpoint} holds the pointer coordinates at the time \
the mouse button was pressed.  For 3-D plots, the first row of the returned \
matrix specifies the point nearest to the current camera position and the \
second row the furthest point.  The two points forms a line which is \
perpendicular to the screen.";
        s.valid = "2-by-3 matrix";

      case "dataaspectratio"
        s.doc = "Specify the relative height and width of the data \
displayed in the axes.  Setting @code{dataaspectratio} to \
@w{@code{[1, 2]}} causes the length of one unit as displayed on the x-axis \
to be the same as the length of 2 units on the y-axis.  \
@xref{XREFdaspect, , daspect function}.  __modemsg__.";
        s.valid = valid_3elvec;

      case "dataaspectratiomode"
      case "fontangle"
        s.doc = doc_fontangle;

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;

      case "fontsize"
        s.doc = doc_fontsize;
        s.valid = "scalar";

      case "fontsmoothing"
        s.doc = "Control whether any text associated with __objname__ is anti-aliased.";

      case "fontunits"
        s.doc = doc_fontunits;

      case "fontweight"
        s.doc = doc_fontweight;

      case "gridalpha"
        s.doc = sprintf (doc_notimpl, "Transparency");

      case "gridalphamode"
        s.doc = doc_unused;

      case "gridcolor"
        s.doc = doc_unused;

      case "gridcolormode"
        s.doc = doc_unused;

      case "gridlinestyle"

      case "innerposition"
        s.doc = "The @qcode{\"innerposition\"} property is the same as the \
@ref{XREFaxesposition, , @w{@qcode{\"position\"} property}}.";
        s.valid = valid_4elvec;

      case "labelfontsizemultiplier"
        s.doc = "Ratio between the x/y/zlabel fontsize and the tick \
label fontsize";

      case "layer"
        s.doc = "Control whether the axes is drawn below child graphics \
objects (ticks, labels, etc.@: covered by plotted objects) or above.";

      case "linestyleorder"
        s.doc = doc_unused;

      case "linestyleorderindex"
        s.doc = doc_unused;

      case "linewidth"
        s.doc = "Width of the main axes lines";

      case "minorgridalpha"
        s.doc = sprintf (doc_notimpl, "Transparency");

      case "minorgridalphamode"
        s.doc = doc_unused;

      case "minorgridcolor"
        s.doc = doc_unused;

      case "minorgridcolormode"
        s.doc = doc_unused;

      case "minorgridlinestyle"
      case "mousewheelzoom"
        s.doc = "Fraction of axes limits to zoom for each wheel movement.";
        s.valid = "scalar in the range (0, 1)";

      case "nextplot"
        s.doc = "__prop__ is used by high level plotting functions to \
decide what to do with graphics objects already present in the axes.  \
@xref{XREFnewplot, , @w{newplot function}}.  The state of __prop__ \
is typically controlled using the @code{hold} function.  \
@xref{XREFhold, , @w{hold function}}.";

      case "outerposition"
        s.doc = "Specify the position of the plot including titles, \
axes, and legend.  The four elements of the vector are the \
coordinates of the lower left corner and width and height of the \
plot, in units normalized to the width and height of the plot \
window.  For example, @code{[0.2, 0.3, 0.4, 0.5]} sets the lower \
left corner of the axes at @math{(0.2, 0.3)} and the width and \
height to be 0.4 and 0.5 respectively.  \
@xref{XREFaxesposition, , @w{position property}}.";
        s.valid = valid_4elvec;
      case "plotboxaspectratio"
        s.doc = "@xref{XREFpbaspect, , pbaspect function}.  \
__modemsg__.";

      case "plotboxaspectratiomode"
      case "position"
        if (strcmp (objname, "legend"))
          s.doc = "Specify the position of the legend excluding its title. \
The four elements of the vector are the coordinates of the lower left corner \
and width and height of the legend.  Changing this property also \
switches the @qcode{\"location\"} to @qcode{\"none\"}.";
          s.printdefault = false;
        else
          s.doc = "Specify the position of the plot excluding titles, \
axes, and legend.  The four elements of the vector are the \
coordinates of the lower left corner and width and height of the \
plot, in units normalized to the width and height of the plot \
window.  For example, @code{[0.2, 0.3, 0.4, 0.5]} sets the lower \
left corner of the axes at @math{(0.2, 0.3)} and the width and \
height to be 0.4 and 0.5 respectively.  \
@xref{XREFaxesouterposition, , @w{outerposition property}}.";
        endif
        s.valid = valid_4elvec;

      case "positionconstraint"
        s.doc = "Specify which of @qcode{\"innerposition\"} or \
@qcode{\"outerposition\"} properties takes precedence when axes \
annotations extent changes.  \
@xref{XREFaxesinnerposition, , @w{@qcode{\"innerposition\"} property}}, \
and @ref{XREFaxesouterposition, , @w{@qcode{\"outerposition\"} property}}.";

      case "projection"
        s.doc = doc_unused;

      case "sortmethod"
        s.doc = doc_unused;

      case "tickdir"
        s.doc = "Control whether axes tick marks project \"in\" to the plot \
box or \"out\".  __modemsg__.";

      case "tickdirmode"

      case "ticklabelinterpreter"
        s.doc = "Control the way x/y/zticklabel properties are interpreted.\n\
@xref{Use of the \"interpreter\" Property, , @w{Use of the \"interpreter\" Property}}.";

      case "ticklength"
        s.doc = "Two-element vector @code{[2Dlen 3Dlen]} specifying the \
length of the tickmarks relative to the longest visible axis.";
        s.valid = valid_2elvec;

      case "tightinset"
        s.doc = "Size of the @code{[left bottom right top]} margins \
around the axes that enclose labels and title annotations.";
        s.valid = valid_4elvec;
        s.printdefault = false;

      case "title"
        s.doc = "Graphics handle of the title text object.";
        s.valid = valid_handle;

      case "titlefontsizemultiplier"
        s.doc = "Ratio between the title fontsize and the tick \
label fontsize";
        s.valid = "positive scalar";

      case "titlefontweight"
        s.doc = "Control variant of base font used for the axes title.";

      case "units"
        if (strcmp (objname, "legend"))
          s.doc = "Units used to interpret the @qcode{\"position\"}, \
 property.";
        else
          s.doc = "Units used to interpret the @qcode{\"position\"}, \
@qcode{\"outerposition\"}, and @qcode{\"tightinset\"} properties.";
        endif

      case "view"
        s.doc = "Two-element vector @code{[azimuth elevation]} specifying \
the viewpoint for three-dimensional plots";
        s.valid = valid_2elvec;

      case "xaxislocation"
        s.doc = "Control the x axis location.";

      case "xcolor"
        s.doc = "Color of the x-axis.  @xref{Colors, , colorspec}.  \
__modemsg__.";
        s.valid = packopt ({markdef(valid_color), "@qcode{\"none\"}"});

      case "xcolormode"

      case "xdir"
        s.doc = "Direction of the x axis: @qcode{\"normal\"} is left \
to right.";

      case "xgrid"
        s.doc = "Control whether major x grid lines are displayed.";

      case "xlabel"
        s.doc = "Graphics handle of the x label text object.";
        s.valid = valid_handle;

      case "xlim"
        s.doc = "Two-element vector @code{[xmin xmax]} specifying the limits \
for the x-axis.  __modemsg__.   @xref{XREFxlim, , @w{xlim function}}.";
        s.valid = valid_2elvec;

      case "xlimitmethod"
        s.doc = "Method used to determine the x axis limits when the \
@code{xlimmode} property is @qcode{\"auto\"}.  The default value, \
@qcode{\"tickaligned\"} makes limits align with the closest ticks.  With \
value @qcode{\"tight\"} the limits are adjusted to enclose all the graphics \
objects in the axes, while with value @qcode{\"padded\"}, an additionnal \
margin of about 7%% of the data extent is added around the objects. \
@xref{XREFaxis, , @w{axis function}}.";

      case "xlimmode"
      case "xminorgrid"
        s.doc = "Control whether minor x grid lines are displayed.";

      case "xminortick"
      case "xscale"
      case "xtick"
        s.doc = "Position of x tick marks.  __modemsg__.";
        s.valid = "vector";
        s.printdefault = false;

      case "xticklabel"
        s.doc = "Labels of x tick marks.  __modemsg__.";
        s.valid = valid_cellstring;

      case "xticklabelmode"
      case "xticklabelrotation"
        s.doc = doc_unused;

      case "xtickmode"
      case "yaxislocation"
        s.doc = "Control the y-axis location.";

      case "ycolor"
        s.doc = "Color of the y-axis.  @xref{Colors, , colorspec}.";
        s.valid = packopt ({markdef(valid_color), "@qcode{\"none\"}"});

      case "ycolormode"

      case "ydir"
        s.doc = "Direction of the y-axis: @qcode{\"normal\"} is bottom \
to top.";

      case "ygrid"
        s.doc = "Control whether major y grid lines are displayed.";

      case "ylabel"
        s.doc = "Graphics handle of the y label text object.";
        s.valid = valid_handle;

      case "ylim"
        s.doc = "Two-element vector @code{[ymin ymax]} specifying the limits \
for the y-axis.  __modemsg__.  @xref{XREFylim, , @w{ylim function}}.";
        s.valid = valid_2elvec;

      case "ylimitmethod"
        s.doc = "Method used to determine the y axis limits when the \
@code{xlimmode} property is @qcode{\"auto\"}.  The default value, \
@qcode{\"tickaligned\"} makes limits align with the closest ticks.  With \
value @qcode{\"tight\"} the limits are adjusted to enclose all the graphics \
objects in the axes, while with value @qcode{\"padded\"}, an additionnal \
margin of about 7%% of the data extent is added around the objects. \
@xref{XREFaxis, , @w{axis function}}.";

      case "ylimmode"
      case "yminorgrid"
        s.doc = "Control whether minor y grid lines are displayed.";

      case "yminortick"
      case "yscale"
      case "ytick"
        s.doc = "Position of y tick marks.  __modemsg__.";
        s.valid = "vector";
        s.printdefault = false;

      case "yticklabel"
        s.doc = "Labels of y tick marks.  __modemsg__.";
        s.valid = valid_cellstring;

      case "yticklabelmode"
      case "yticklabelrotation"
        s.doc = doc_unused;

      case "ytickmode"
      case "zcolor"
        s.doc = "Color of the z-axis.  @xref{Colors, , colorspec}.";
        s.valid = packopt ({markdef(valid_color), "@qcode{\"none\"}"});

      case "zcolormode"
      case "zdir"
      case "zgrid"
        s.doc = "Control whether major z grid lines are displayed.";

      case "zlabel"
        s.doc = "Graphics handle of the z label text object.";
        s.valid = valid_handle;

      case "zlim"
        s.doc = "Two-element vector @code{[zmin zmaz]} specifying the limits \
for the z-axis.  __modemsg__.  @xref{XREFzlim, , @w{zlim function}}.";
        s.valid = valid_2elvec;

      case "zlimitmethod"
        s.doc = "Method used to determine the z axis limits when the \
@code{xlimmode} property is @qcode{\"auto\"}.  The default value, \
@qcode{\"tickaligned\"} makes limits align with the closest ticks.  With \
value @qcode{\"tight\"} the limits are adjusted to enclose all the graphics \
objects in the axes, while with value @qcode{\"padded\"}, an additionnal \
margin of about 7%% of the data extent is added around the objects. \
@xref{XREFaxis, , @w{axis function}}.";

      case "zlimmode"
      case "zminorgrid"
        s.doc = "Control whether minor z grid lines are displayed.";

      case "zminortick"
      case "zscale"
      case "ztick"
        s.doc = "Position of z tick marks.  __modemsg__.";
        s.valid = "vector";
        s.printdefault = false;

      case "zticklabel"
        s.doc = "Labels of z tick marks.  __modemsg__.";
        s.valid = valid_cellstring;

      case "zticklabelmode"
      case "zticklabelrotation"
        s.doc = doc_unused;

      case "ztickmode"

      ## Legend specific properties
      case "autoupdate"
        s.doc = "Control whether the number of legend items is updated \
automatically when objects are added to (or deleted from) the peer axes.\n\
For example:\n\
@example\n\
@group\n\
## Create a single plot with its legend.\n\
figure ();\n\
plot (1:10);\n\
legend (\"Slope 1\");\n\
## Add another plot and specify its displayname so that\n\
## the legend is correctly updated.\n\
hold on;\n\
plot ((1:10) * 2, \"displayname\", \"Slope 2\");\n\
## Stop automatic updates for further plots.\n\
legend (\"autoupdate\", \"off\");\n\
plot ((1:10) * 3);\n\
@end group\n\
@end example";

      case "edgecolor"
        s.doc = "Control the color of the legend outline.";
        s.valid = valid_color;

      case "interpreter"
        s.doc = "Control if and eventually how labels strings are interpreted \
before rendering.\n\
@xref{Use of the \"interpreter\" Property, , @w{Use of the \"interpreter\" Property}}.";

      case "itemhitfcn"
        s.doc = "Callback function which is executed when a legend item \
is clicked.  @xref{Callbacks, , @w{Callbacks section}}.\n\
\n\
The callback function must have the following prototype \
@code{fcn (hlegend, evnt)}, where @code{hlegend} is the legend object handle \
and @code{evnt} is a structure with the following fields:\n\
@table @code\n\
@item Peer\n\
Handle of the plot object to which the clicked item is associated.\n\
@item Region\n\
May be @qcode{\"icon\"} or @qcode{\"label\"} depending on which part of \
the item is clicked.\n\
@item SelectionType\n\
One of @qcode{\"normal\"}, @qcode{\"extend\"}, @qcode{\"open\"}, or \
@qcode{\"alt\"}. \
@xref{XREFfigureselectiontype, , @w{Figure @qcode{\"selectiontype\"}}}.\n\
@item Source\n\
Handle of the legend object.\n\
@item EventName\n\
Name is @qcode{\"ItemHit\"}.\n\
@end table";

      case "location"
        s.doc = "Control the location of the legend.";

      case "numcolumns"
        s.doc = "Control the number of columns used in the layout of the legend items. \
 For example:\n\
@example\n\
@group\n\
figure ();\n\
plot (rand (30));\n\
legend (\"numcolumns\", 3);\n\
@end group\n\
@end example\n\
__modemsg__.";
        s.valid = "scalar interger";

      case "orientation"
        s.doc = "Control whether the legend items are arranged vertically \
(column-wise) or horizontally (row-wise).";

      case "string"
        s.doc = "List of labels for the legend items.  For example:\n\
@example\n\
@group\n\
figure ();\n\
plot (rand (20));\n\
## Let legend choose names automatically\n\
hl = legend ();\n\
## Selectively change some names\n\
str = get (hl, \"string\");\n\
str(1:5:end) = \"Garbage\";\n\
set (hl, \"string\", str);\n\
@end group\n\
@end example";
        s.valid = valid_cellstring;
        s.printdefault = false;

      case "textcolor"
        s.doc = "Control the color of the text strings for legend items.";
        s.valid = valid_color;

      case "textposition"
        s.doc = "Control whether text strings are displayed on the left or \
right of their corresponding icon.";

    endswitch

  ## Line properties
  elseif (strcmp (objname, "line"))
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;

      ## Specific properties
      case "color"
        s.doc = "Color of the line object.  @xref{Colors, , colorspec}.";
        s.valid = valid_color;

      case "displayname"
        s.doc = "Text for the legend entry corresponding to this line.";
        s.valid = valid_cellstring;

      case "interpreter"

      case "linestyle"
        s.doc = "@xref{Line Styles}.";

      case "linewidth"
        s.doc = "Width of the line object measured in points.";

      case "linejoin"
        s.doc = "Control the shape of the junction of line segments. \
This property currently only affects the printed output.";

      case "marker"
        s.doc = "Shape of the marker for each data point.  \
@xref{Marker Styles}.";

      case "markeredgecolor"
        s.doc = "Color of the edge of the markers.  When set to \
@qcode{\"auto\"}, the marker edges have the same color as the line.  If set \
to @qcode{\"none\"}, no marker edges are displayed.  This property can also \
be set to any color.  @xref{Colors, , colorspec}.";

      case "markerfacecolor"
        s.doc = "Color of the face of the markers.  When set to \
@qcode{\"auto\"}, the marker faces have the same color as the line.  If set \
to @qcode{\"none\"}, the marker faces are not displayed.  This property \
can also be set to any color.  @xref{Colors, , colorspec}.";

      case "markersize"
        s.doc = "Size of the markers measured in points.";
        s.valid = "scalar";

      case "xdata"
        s.doc = "Vector of x data to be plotted.";
        s.valid = "vector";

      case "xdatasource"
        s.doc = "Name of a vector in the current base workspace to use as \
x data.";
        s.valid = valid_string;

      case "ydata"
        s.doc = "Vector of y data to be plotted.";
        s.valid = "vector";

      case "ydatasource"
        s.doc = "Name of a vector in the current base workspace to use as \
y data.";
        s.valid = valid_string;

      case "zdata"
        s.doc = "Vector of z data to be plotted.";
        s.valid = "vector";

      case "zdatasource"
        s.doc = "Name of a vector in the current base workspace to use as \
z data.";
        s.valid = valid_string;

    endswitch

  ## Text properties
  elseif (strcmp (objname, "text"))
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;

      ## Specific properties
      case "backgroundcolor"
        s.doc = "Color of the background area.  \
@xref{Colors, , colorspec}.";
        s.valid = valid_color;

      case "color"
        s.doc = "Color of the text.  @xref{Colors, ,colorspec}.";
        s.valid = valid_color;

      case "displayname"
      case "edgecolor"
        s.doc = "Color of the outline of the background area.  \
@xref{Colors, , colorspec}.";
        s.valid = valid_color;

      case "editing"
        s.doc = doc_unused;

      case "extent"
        s.doc = "Vector @code{[x0 y0 width height]} indicating the size \
and location of the text string.";
        s.valid = valid_4elvec;
        s.printdefault = false;

      case "fontangle"
        s.doc = doc_fontangle;

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;

      case "fontsmoothing"
        s.doc = "Control whether anti-aliasing is used when rendering text.";

      case "fontsize"
        s.doc = doc_fontsize;
        s.valid = "scalar";

      case "fontunits"
        s.doc = doc_fontunits;

      case "fontweight"
        s.doc = doc_fontweight;

      case "horizontalalignment"
      case "interpreter"
        s.doc = "Control the way the @qcode{\"string\"} property is \
interpreted.\n\
@xref{Use of the \"interpreter\" Property, , @w{Use of the \"interpreter\" Property}}.";

      case "linestyle"
        s.doc = "Style of the outline.  @xref{Line Styles}.";

      case "linewidth"
        s.doc = "Width of the outline.";
        s.valid = "scalar";

      case "margin"
        s.doc = "Margins between the borders of the background area \
and the texts.  The value is currently interpreted as pixels, regardless \
of the @qcode{\"fontunits\"} property.";
        s.valid = "scalar";

      case "position"
        s.doc = "Vector @code{[X0 Y0 Z0]} where X0, Y0, and Z0 indicate the \
position of the text anchor as defined by @code{verticalalignment} and \
@code{horizontalalignment}.";
        s.valid = valid_3elvec;

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
    switch (field)
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
        s.doc = "Text for the legend entry corresponding to this image.";
        s.valid = valid_cellstring;

      case "xdata"
        s.doc = "Two-element vector @code{[xfirst xlast]} specifying the x \
coordinates of the centers of the first and last columns of the image.\n\
\n\
Setting @code{xdata} to the empty matrix ([]) will restore the default value \
of @code{[1 columns(image)]}.";
        s.valid = valid_2elvec;

      case "ydata"
        s.doc = "Two-element vector @code{[yfirst ylast]} specifying the y \
coordinates of the centers of the first and last rows of the image.\n\
\n\
Setting @code{ydata} to the empty matrix ([]) will restore the default value \
of @code{[1 rows(image)]}.";
        s.valid = valid_2elvec;

    endswitch

  ## Surface properties
  elseif (strcmp (objname, "surface"))
    switch (field)
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
        s.doc = "Strength of the ambient light. Value between 0.0 and 1.0";
        s.valid = "scalar";

      case "backfacelighting"
        s.doc = "@qcode{\"lit\"}: The normals are used as is for lighting. \
@qcode{\"reverselit\"}: The normals are always oriented towards the point of view. \
@qcode{\"unlit\"}: Faces with normals pointing away from the point of view are unlit.";

      case "cdata"
        s.valid = "matrix";

      case "cdatamapping"
      case "cdatasource"
      case "diffusestrength"
        s.doc = "Strength of the diffuse reflex. Value between 0.0 (no \
diffuse reflex) and 1.0 (full diffuse reflex).";
        s.valid = "scalar";

      case "displayname"
        s.doc = "Text for the legend entry corresponding to this surface.";

      case "edgealpha"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = "scalar";

      case "edgecolor"
      case "edgelighting"
        s.doc = "When set to a value other than @qcode{\"none\"}, the edges \
of the object are drawn with light and shadow effects.  Supported values are \
@qcode{\"none\"} (no lighting effects), @qcode{\"flat\"} (facetted look) and \
@qcode{\"gouraud\"} (linear interpolation of the lighting effects between \
the vertices). @qcode{\"phong\"} is deprecated and has the same effect as \
@qcode{\"gouraud\"}.";

      case "facealpha"
        s.doc = "Transparency level of the faces of the surface object.  Only \
double values are supported at present where a value of 0 means complete \
transparency and a value of 1 means solid faces without transparency.  Setting \
the property to @qcode{\"flat\"}, @qcode{\"interp\"} or @qcode{\"texturemap\"} \
causes the faces to not being rendered.  Additionally, the faces are not sorted \
from back to front which might lead to unexpected results when rendering \
layered transparent faces.";
        s.valid = packopt ({"scalar", ...
                            "@qcode{\"flat\"}", ...
                            "@qcode{\"interp\"}", ...
                            "@qcode{\"texturemap\"}"});

      case "facecolor"
      case "facelighting"
        s.doc = "When set to a value other than @qcode{\"none\"}, the faces \
of the object are drawn with light and shadow effects.  Supported values are \
@qcode{\"none\"} (no lighting effects), @qcode{\"flat\"} (facetted look) and \
@qcode{\"gouraud\"} (linear interpolation of the lighting effects between \
the vertices). @qcode{\"phong\"} is deprecated and has the same effect as \
@qcode{\"gouraud\"}.";

      case "facenormals"
        s.doc = "Face normals are used for lighting the edges or faces if the \
@code{edgelighting} or @code{facelighting} properties are set to \
@qcode{\"flat\"}.  __modemsg__";

      case "facenormalsmode"
        s.doc = "If this property is set to @qcode{\"auto\"}, \
@code{facenormals} are automatically calculated if the @code{edgelighting} or \
@code{facelighting} property are set to @qcode{\"flat\"} and at least one \
@code{light} object is present and visible in the same axes.";

      case "interpreter"
      case "linestyle"
        s.doc = "@xref{Line Styles}.";

      case "linewidth"
        s.doc = "@xref{XREFlinelinewidth, , @w{line linewidth property}}.";

      case "marker"
        s.doc = "@xref{Marker Styles}.";

      case "markeredgecolor"
        s.doc = "@xref{XREFlinemarkeredgecolor, , \
@w{line markeredgecolor property}}.";

      case "markerfacecolor"
        s.doc = "@xref{XREFlinemarkerfacecolor, , \
@w{line markerfacecolor property}}.";

      case "markersize"
        s.doc = "@xref{XREFlinemarkersize, , \
@w{line markersize property}}.";
        s.valid = "scalar";

      case "meshstyle"
      case "specularcolorreflectance"
        s.doc = "Reflectance for specular color. Value between 0.0 (color \
of underlying face) and 1.0 (color of light source).";
        s.valid = "scalar";

      case "specularexponent"
        s.doc = "Exponent for the specular reflex. The lower the value, \
the more the reflex is spread out.";
        s.valid = "scalar";

      case "specularstrength"
        s.doc = "Strength of the specular reflex. Value between 0.0 (no \
specular reflex) and 1.0 (full specular reflex).";
        s.valid = "scalar";

      case "vertexnormals"
        s.doc = "Vertex normals are used for lighting the edges or faces if \
the @code{edgelighting} or @code{facelighting} properties are set to \
@qcode{\"gouraud\"}.  __modemsg__";

      case "vertexnormalsmode"
        s.doc = "If this property is set to @qcode{\"auto\"}, \
@code{vertexnormals} are automatically calculated if the @code{edgelighting} \
or @code{facelighting} property are set to @qcode{\"gouraud\"} and at least \
one @code{light} object is present and visible in the same axes.";

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
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;

      ## Specific properties
      case "alphadatamapping"
        s.doc = sprintf (doc_notimpl, "Transparency");

      case "ambientstrength"
        s.doc = "Strength of the ambient light. Value between 0.0 and 1.0";
        s.valid = "scalar";

      case "backfacelighting"
        s.doc =  "@qcode{\"lit\"}: The normals are used as is for lighting. \
@qcode{\"reverselit\"}: The normals are always oriented towards the point of view. \
@qcode{\"unlit\"}: Faces with normals pointing away from the point of view are unlit.";

      case "cdata"
        s.doc = "Data defining the patch object color.\n\
Patch color can be defined for faces or for vertices.\n\
\n\
If @code{cdata} is a scalar index into the current colormap or a RGB triplet, \
it defines the color of all faces.\n\
\n\
If @code{cdata} is an N-by-1 vector of indices or an N-by-3 (RGB) matrix, \
it defines the color of each one of the N faces.\n\
\n\
If @code{cdata} is an N-by-M or an N-by-M-by-3 (RGB) matrix, \
it defines the color at each vertex.";
        s.valid = valid_scalmat;

      case "diffusestrength"
        s.doc = "Strength of the diffuse reflex. Value between 0.0 (no \
diffuse reflex) and 1.0 (full diffuse reflex).";
        s.valid = "scalar";

      case "displayname"
        s.doc = "Text of the legend entry corresponding to this patch.";

      case "edgealpha"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = valid_scalmat;

      case "edgecolor"
      case "edgelighting"
        s.doc = "When set to a value other than @qcode{\"none\"}, the edges \
of the object are drawn with light and shadow effects.  Supported values are \
@qcode{\"none\"} (no lighting effects), @qcode{\"flat\"} (facetted look) and \
@qcode{\"gouraud\"} (linear interpolation of the lighting effects between \
the vertices). @qcode{\"phong\"} is deprecated and has the same effect as \
@qcode{\"gouraud\"}.";

      case "facealpha"
        s.doc = "Transparency level of the faces of the patch object.  Only \
double values are supported at present where a value of 0 means complete \
transparency and a value of 1 means solid faces without transparency.  Setting \
the property to @qcode{\"flat\"} or @qcode{\"interp\"} causes the faces to not \
being rendered.  Additionally, the faces are not sorted from back to front \
which might lead to unexpected results when rendering layered transparent \
faces.";
        s.valid = packopt ({"scalar", ...
                            "@qcode{\"flat\"}", ...
                            "@qcode{\"interp\"}"});

      case "facecolor"
        ## Don't provide a default value, and mark colorspec with
        ## braces, this forces the default rgb triplet to be displayed
        s.valid = packopt ({markdef(valid_color), ...
                            "@qcode{\"none\"}", ...
                            "@qcode{\"flat\"}", ...
                            "@qcode{\"interp\"}"});

      case "facelighting"
        s.doc = "When set to a value other than @qcode{\"none\"}, the faces \
of the object are drawn with light and shadow effects. Supported values are \
@qcode{\"none\"} (no lighting effects), @qcode{\"flat\"} (facetted look) and \
@qcode{\"gouraud\"} (linear interpolation of the lighting effects between \
the vertices). @qcode{\"phong\"} is deprecated and has the same effect as \
@qcode{\"gouraud\"}.";

      case "facenormals"
        s.doc = "Face normals are used for lighting the edges or faces if the \
@code{edgelighting} or @code{facelighting} properties are set to \
@qcode{\"flat\"}.  __modemsg__";

      case "facenormalsmode"
        s.doc = "If this property is set to @qcode{\"auto\"}, \
@code{facenormals} are automatically calculated if the @code{edgelighting} or \
@code{facelighting} property are set to @qcode{\"flat\"} and at least one \
@code{light} object is present and visible in the same axes.";

      case "faces"
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
        s.doc = "@xref{XREFlinemarker, , @w{line marker property}}.";

      case "markeredgecolor"
        s.doc = "@xref{XREFlinemarkeredgecolor, , \
@w{line markeredgecolor property}}.";

      case "markerfacecolor"
        s.doc = "@xref{XREFlinemarkerfacecolor, , \
@w{line markerfacecolor property}}.";

      case "markersize"
        s.doc = "@xref{XREFlinemarkersize, , @w{line markersize property}}.";
        s.valid = "scalar";

      case "specularcolorreflectance"
        s.doc = "Reflectance for specular color.  Value between 0.0 (color \
of underlying face) and 1.0 (color of light source).";
        s.valid = "scalar";

      case "specularexponent"
        s.doc = "Exponent for the specular reflex.  The lower the value, \
the more the reflex is spread out.";
        s.valid = "scalar";

      case "specularstrength"
        s.doc = "Strength of the specular reflex.  Value between 0.0 (no \
specular reflex) and 1.0 (full specular reflex).";
        s.valid = "scalar";

      case "vertexnormals"
        s.doc = "Vertex normals are used for lighting the edges or faces if \
the @code{edgelighting} or @code{facelighting} properties are set to \
@qcode{\"gouraud\"}.  __modemsg__";

      case "vertexnormalsmode"
        s.doc = "If this property is set to @qcode{\"auto\"}, \
@code{vertexnormals} are automatically calculated if the @code{edgelighting} \
or @code{facelighting} property are set to @qcode{\"gouraud\"} and at least \
one @code{light} object is present and visible in the same axes.";

      case "vertices"
        s.valid = valid_vecmat;

      case "xdata"
        s.valid = valid_vecmat;

      case "ydata"
        s.valid = valid_vecmat;

      case "zdata"
        s.valid = valid_vecmat;

    endswitch

  ## Scatter properties
  elseif (strcmp (objname, "scatter"))
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;

      ## Specific properties
      case "cdatamode"
        s.doc = "If @code{cdatamode} is @qcode{\"auto\"}, @code{cdata} is set \
to the color from the @code{colororder} of the ancestor axes corresponding to \
the @code{seriesindex}.";

      case "cdatasource"
        s.doc = sprintf (doc_notimpl, "Data from workspace variables");

      case "cdata"
        s.doc = "Data defining the scatter object color.\n\
\n\
If @code{cdata} is a scalar index into the current colormap or a RGB triplet, \
it defines the color of all scatter markers.\n\
\n\
If @code{cdata} is an N-by-1 vector of indices or an N-by-3 (RGB) matrix, \
it defines the color of each one of the N scatter markers.";
        s.valid = valid_scalmat;


      case "displayname"
        s.doc = "Text of the legend entry corresponding to this scatter object.";

      case "linewidth"
        s.doc = "Line width of the edge of the markers.";

      case "marker"
        s.doc = "@xref{XREFlinemarker, , @w{line marker property}}.";

      case "markeredgealpha"
        s.doc = "Transparency level of the faces of the markers where a \
value of 0 means complete transparency and a value of 1 means solid faces \
without transparency.  Note that the markers are not sorted from back to \
front which might lead to unexpected results when rendering layered \
transparent markers or in combination with other transparent objects.";
        s.valid = "scalar";

      case "markeredgecolor"
        s.doc = "Color of the edge of the markers.  @qcode{\"none\"} means \
that the edges are transparent and @qcode{\"flat\"} means that the value \
from @code{cdata} is used.  @xref{XREFlinemarkeredgecolor, , \
@w{line markeredgecolor property}}.";
        s.valid = packopt ({markdef("@qcode{\"none\"}"), ...
                            "@qcode{\"flat\"}", ...
                            valid_color});

      case "markerfacealpha"
        s.doc = "Transparency level of the faces of the markers where a \
value of 0 means complete transparency and a value of 1 means solid faces \
without transparency.  Note that the markers are not sorted from back to \
front which might lead to unexpected results when rendering layered \
transparent markers or in combination with other transparent objects.";
        s.valid = "scalar";

      case "markerfacecolor"
        s.doc = "Color of the face of the markers.  @qcode{\"none\"} means \
that the faces are transparent, @qcode{\"flat\"} means that the value from \
@code{cdata} is used, and @qcode{\"auto\"} uses the @code{color} property of \
the ancestor axes. @xref{XREFlinemarkerfacecolor, , \
@w{line markerfacecolor property}}.";
        s.valid = packopt ({markdef("@qcode{\"none\"}"), ...
                            "@qcode{\"flat\"}", ...
                            "@qcode{\"auto\"}", ...
                            valid_color});

      case "seriesindex"
        s.doc = "Each scatter object in the same axes is asigned an \
incrementing integer.  This corresponds to the index into the \
@code{colororder} of the ancestor axes that is used if @code{cdatamode} is \
set to @qcode{\"auto\"}.";

      case "sizedatasource"
        s.doc = sprintf (doc_notimpl, "Data from workspace variables");

      case "sizedata"
        s.doc = "Size of the area of the marker. A scalar value applies to \
all markers.  If @code{cdata} is an N-by-1 vector, it defines the color of \
each one of the N scatter markers.";
        s.valid =  packopt ({"[]", "scalar", "vector"});

      case "xdatasource"
        s.doc = sprintf (doc_notimpl, "Data from workspace variables");

      case "xdata"
        s.doc = "Vector with the x coordinates of the scatter object.";
        s.valid = "vector";

      case "ydatasource"
        s.doc = sprintf (doc_notimpl, "Data from workspace variables");

      case "ydata"
        s.doc = "Vector with the y coordinates of the scatter object.";
        s.valid = "vector";

      case "zdatasource"
        s.doc = sprintf (doc_notimpl, "Data from workspace variables");

      case "zdata"
        s.doc = "For 3D data, vector with the y coordinates of the scatter \
object.";
        s.valid = packopt ({"[]", "vector"});

    endswitch

  ## Light properties
  elseif (strcmp (objname, "light"))
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;

      ## Specific properties
      case "color"
        s.doc = "Color of the light source.  @xref{Colors, ,colorspec}.";
        s.valid = valid_color;

      case "position"
        s.doc = "Position of the light source.";

      case "style"
        s.doc = "This string defines whether the light emanates from a \
light source at infinite distance (@qcode{\"infinite\"}) or from a local \
point source (@qcode{\"local\"}).";

    endswitch

  ## uimenu properties
  elseif (strcmp (objname, "uimenu"))
    switch (field)
      ## Overridden shared properties
      case "buttondownfcn"
        s.doc = doc_unused;

      ## Specific properties
      case "accelerator"
      case "callback"
      case "checked"
      case "enable"
      case "foregroundcolor"
      case "label"
      case "position"
      case "separator"

    endswitch

  ## uicontextmenu properties
  elseif (strcmp (objname, "uicontextmenu"))
    switch (field)
      ## Overridden shared properties
      case "buttondownfcn"
        s.doc = doc_unused;

      ## Specific properties
      case "callback"
      case "position"

    endswitch

  ## uipanel properties
  elseif (strcmp (objname, "uipanel"))
    switch (field)
      ## Overridden shared properties

      ## Specific properties
      case "backgroundcolor"
      case "bordertype"
      case "borderwidth"
      case "fontangle"
        s.doc = doc_fontangle;

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;

      case "fontsize"
        s.doc = doc_fontsize;
        s.valid = "scalar";

      case "fontunits"
        s.doc = doc_fontunits;

      case "fontweight"
        s.doc = doc_fontweight;

      case "foregroundcolor"
      case "highlightcolor"
      case "position"

      case "resizefcn"
        s.doc = "__prop__ is deprecated.  Use @code{sizechangedfcn} instead.";
        s.valid = valid_fcn;

      case "shadowcolor"

      case "sizechangedfcn"
        s.doc = "Callback triggered when the uipanel size is changed.\
\n\n__fcnmsg__";
        s.valid = valid_fcn;

      case "title"
      case "titleposition"
      case "units"

    endswitch

  ## uibuttongroup properties
  elseif (strcmp (objname, "uibuttongroup"))
    switch (field)
      ## Overridden shared properties

      ## Specific properties
      case "backgroundcolor"
      case "bordertype"
      case "borderwidth"
      case "fontangle"
        s.doc = doc_fontangle;

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;

      case "fontsize"
        s.doc = doc_fontsize;
        s.valid = "scalar";

      case "fontunits"
        s.doc = doc_fontunits;

      case "fontweight"
        s.doc = doc_fontweight;

      case "foregroundcolor"
      case "highlightcolor"
      case "position"

      case "resizefcn"
        s.doc = "__prop__ is deprecated.  Use @code{sizechangedfcn} instead.";
        s.valid = valid_fcn;

      case "selectedobject"
      case "selectionchangedfcn"
      case "shadowcolor"

      case "sizechangedfcn"
        s.doc = "Callback triggered when the uibuttongroup size is changed.\
\n\n__fcnmsg__";
        s.valid = valid_fcn;

      case "title"
      case "titleposition"
      case "units"

    endswitch

  ## uicontrol properties
  elseif (strcmp (objname, "uicontrol"))
    switch (field)
      ## Overridden shared properties

      ## Specific properties
      case "backgroundcolor"
      case "callback"
      case "cdata"
      case "enable"
      case "extent"
        s.doc = "Size of the text string associated to the uicontrol \
 returned in the form @code{[0 0 width height]} (the two first elements \
are always zero).\n\n\
For multi-line strings the returned @code{width} and @code{height} \
indicate the size of the rectangle enclosing all lines.";
        s.valid = valid_4elvec;
        s.printdefault = false;

      case "fontangle"
        s.doc = doc_fontangle;

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;

      case "fontsize"
        s.doc = doc_fontsize;
        s.valid = "scalar";

      case "fontunits"
        s.doc = doc_fontunits;

      case "fontweight"
        s.doc = doc_fontweight;

      case "foregroundcolor"
      case "horizontalalignment"
      case "keypressfcn"
      case "listboxtop"
      case "max"
      case "min"
      case "position"
      case "sliderstep"
      case "string"
      case "style"
      case "tooltipstring"
      case "units"
      case "value"
      case "verticalalignment"

    endswitch

  ## uitable Properties
  elseif (strcmp (objname, "uitable"))
    switch (field)
      ## Overridden shared properties

      ## Specific properties
      case "backgroundcolor"
      case "celleditcallback"
      case "cellselectioncallback"
      case "columneditable"
      case "columnformat"
      case "columnname"
      case "columnwidth"
      case "data"
      case "enable"
      case "extent"
        s.valid = valid_4elvec;
        s.printdefault = false;

      case "fontangle"
        s.doc = doc_fontangle;

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;

      case "fontsize"
        s.doc = doc_fontsize;
        s.valid = "scalar";

      case "fontunits"
        s.doc = doc_fontunits;

      case "fontweight"
        s.doc = doc_fontweight;

      case "foregroundcolor"
      case "keypressfcn"
      case "keyreleasefcn"
      case "position"
      case "rearrangeablecolumns"
      case "rowname"
      case "rowstriping"
      case "tooltipstring"
      case "units"

    endswitch

  ## uitoolbar properties
  elseif (strcmp (objname, "uitoolbar"))
    switch (field)
      ## Overridden shared properties
      case "buttondownfcn"
        s.doc = doc_unused;

    endswitch

  ## uipushtool properties
  elseif (strcmp (objname, "uipushtool"))
    switch (field)
      ## Overridden shared properties
      case "buttondownfcn"
        s.doc = doc_unused;

      ## Specific properties
      case "cdata"
      case "clickedcallback"
      case "enable"
      case "separator"
      case "tooltipstring"

    endswitch

  ## uitoggletool properties
  elseif (strcmp (objname, "uitoggletool"))
    switch (field)
      ## Overridden shared properties
      case "buttondownfcn"
        s.doc = doc_unused;

      ## Specific properties
      case "cdata"
      case "clickedcallback"
      case "enable"
      case "offcallback"
      case "oncallback"
      case "separator"
      case "state"
      case "tooltipstring"

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
  fcnmsg = "For information on how to write graphics listener \
functions see @ref{Callbacks, , @w{Callbacks section}}.";
  strout = strrep (strout, "__fcnmsg__", fcnmsg);
endfunction

function s = getstructure (objname, base = [], props = {})
  hf = [];
  if (! strcmp (objname, "root"))
    ## Use an improbable number to avoid ishghandle to return true for 1
    hf = figure (2265465, "visible", "off");
  endif

  ## Build a default object to extract its properties list and default values.
  if (strcmp (objname, "base"))
    ## Base properties are extracted from hggroup that only have 1 additional
    ## regular (non-hidden) property, "displayname".
    h = hggroup ();
  elseif (strcmp (objname, "root"))
    h = 0;
  elseif (strcmp (objname, "figure"))
    h = hf;
  elseif (strcmp (objname, "legend"))
    line ();
    h = legend ();
    if (isempty (props))
        props = {"autoupdate", "box", "color", "edgecolor", "fontangle", ...
                 "fontname", "fontsize", "fontunits", "fontweight", ...
                 "itemhitfcn", "location", "numcolumns", "orientation", ...
                 "position", "string", "textcolor", "title", "units"};
    endif
  elseif (strcmp (objname, "scatter"))
    ## Make sure to get a scatter object independent of graphics toolkit
    hax = axes (hf);
    h = __go_scatter__ (hax);
  else
    eval (["h = " objname " ();"]);
  endif

  gprop = get (h);
  sprop = set (h);

  if (! isempty (props))
    flds = fieldnames (gprop);
    idx = cellfun (@(s) ! any (strcmp (props, s)), flds);
    gprop = rmfield (gprop, flds(idx));
    flds = fieldnames (sprop);
    idx = cellfun (@(s) ! any (strcmp (props, s)), flds);
    sprop = rmfield (sprop, flds(idx));
  endif

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
      if (! isempty (val.valid) && iscellstr (val.valid))
        ## Add double quotes around string radio properties
        val.valid = cellfun (@(s) ["@qcode{\"" s "\"}"], val.valid,
                             "uniformoutput", false);
        val.valid = strjoin (val.valid, " | ");
      endif
    endif

    args{2*(ii-1)+1} = field;
    args{2*ii} = val;
  endfor

  ## Build struct and remove unused fields in base properties
  s = struct (args{:});

  if (strcmp (objname, "base"))
    s = rmfield (s, "displayname");
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
  if (ishghandle (def) && isscalar (def) && def != 0)
    def = "";
  else
    if (ischar (def))
      def = ['@qcode{"' def '"}'];
    else
      if ((isvector (def) && numel (def) < 5) || isempty (def))
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
        def = strrep (str, "@", "@@");  # must occur first
        def = strrep (def, "{", "@{");
        def = strrep (def, "}", "@}");

        def = ["@code{" def "}"];
      else
        args = arrayfun (@(x) num2str (x), size (def), "uniformoutput", false);
        def = [strjoin(args, "-by-") " " class(def)];
      endif
    endif
  endif

endfunction

function str = printdoc (objname, obj, is_prop_subset)
  ## Sort fields so that they appear in alphabetic order in the manual
  fields = sort (fieldnames (obj));
  nf = numel (fields);

  ## File header and beginning of properties table
  str = warn_autogen ();
  if (strcmp (objname, "root"))
    str = sprintf ("%s\n\nProperties of the root graphics object:", str);
  elseif (is_prop_subset)
    ## Do nothing
  else
    str = sprintf ("%s\n\nProperties of @code{%s} objects (@pxref{XREF%s,,%s}):",
                   str, objname, objname, objname);
  endif
  str = sprintf ("%s\n\n@table @asis", str);

  for ii = 1:nf
    field = fields{ii};
    str = sprintf ("%s\n\n", str);

    if (! is_prop_subset)
      ## @anchor: cross reference using XREFobjnamefield label
      ## Concept index: call info from octave with 'doc ("objname field")'
      str = sprintf ("%s@anchor{XREF%s%s}\n@prindex %s %s\n",
                     str, objname, field, objname, field);
    endif

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
@c Copyright (C) 2014-2023 The Octave Project Developers\n\
@c\n\
@c This file is part of Octave.\n\
@c\n\
@c Octave is free software: you can redistribute it and/or modify it\n\
@c under the terms of the GNU General Public License as published by\n\
@c the Free Software Foundation, either version 3 of the License, or\n\
@c (at your option) any later version.\n\
@c\n\
@c Octave is distributed in the hope that it will be useful, but\n\
@c WITHOUT ANY WARRANTY; without even the implied warranty of\n\
@c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
@c GNU General Public License for more details.\n\
@c\n\
@c You should have received a copy of the GNU General Public License\n\
@c along with Octave; see the file COPYING.  If not, see\n\
@c <https://www.gnu.org/licenses/>.";
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
