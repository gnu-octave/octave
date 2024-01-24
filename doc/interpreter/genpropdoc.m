########################################################################
##
## Copyright (C) 2014-2024 The Octave Project Developers
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
  ##   "__fcnmsg__"  : replaced by a message explaining where to find
  ##                   documentation on the form of a callback function.
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
  ##
  ## -"category": a string that is used to group properties.  Properties
  ##   without category designations will be defaulted into a "Miscellaneous"
  ##   category.  The "Miscellaneous" and (if it is used) the "Unused"
  ##   categories will be sorted to the end of each document.

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
    s = struct ("valid", "", "default", "", "doc", "", ...
                "printdefault", true, "category", "Miscellaneous");
  endif

  ## Base properties: Write generic documentation because it will be included
  ## in the list of each graphics object.  If a given graphics object
  ## interprets the property differently than others, then the doc will have
  ## to be rewritten for this object.
  if (strcmp (objname, "base"))
    switch (field)

      case "beingdeleted"
        s.doc  = "Property indicating that a function has initiated deletion \
of the object.  __prop__ is set to true until the object no longer exists.";
        s.category = "Creation/Deletion";

      case "busyaction"
        s.doc = "Define how Octave handles the execution of this object's \
callback properties when it is unable to interrupt another object's \
executing callback.  This is only relevant when the currently executing \
callback object has its @code{interruptible} property set to \
\@qcode{\"off\"}.  The __prop__ property of the interrupting callback object \
indicates whether the interrupting callback is queued (@qcode{\"queue\"} \
(default)) or discarded (@qcode{\"cancel\"}).\n\
@xref{Callbacks, , @w{Callbacks section}}.";
        s.category = "Callback Execution";

      case "buttondownfcn"
        s.doc = "__fcnmsg__";
        s.valid = valid_fcn;
        s.category = "Mouse Interaction";

      case "children"
        s.doc = "Graphics handles of the __objname__'s children.";
        s.valid = "vector of graphics handles";
        s.category = "Parent/Children";

      case "clipping"
        s.doc = "If __prop__ is @qcode{\"on\"}, the __objname__ is \
clipped in its parent axes limits.";
        s.category = "Display";

      case "contextmenu"
        s.doc = "Graphics handle of the uicontextmenu object that is \
currently associated to this __objname__ object.";
        s.valid = valid_handle;
        s.category = "Mouse Interaction";

      case "createfcn"
        s.doc = "Callback function executed immediately after __objname__ \
has been created.  Function is set by using default property on root object, \
e.g., @code{set (groot, \"default__objname__createfcn\", \
'disp (\"__objname__ created!\")')}.\n\n__fcnmsg__";
        s.valid = valid_fcn;
        s.category = "Creation/Deletion";

      case "deletefcn"
        s.doc = "Callback function executed immediately before __objname__ \
is deleted.\n\n__fcnmsg__";
        s.valid = valid_fcn;
        s.category = "Creation/Deletion";

      case "handlevisibility"
        s.doc = "If __prop__ is @qcode{\"off\"}, the __objname__'s \
handle is not visible in its parent's \"children\" property.";
        s.category = "Parent/Children";

      case "hittest"
        s.doc = "Specify whether __objname__ processes mouse events \
or passes them to ancestors of the object.  When enabled, the object may \
respond to mouse clicks by evaluating the @qcode{\"buttondownfcn\"}, showing \
the uicontextmenu, and eventually becoming the root \
@qcode{\"currentobject\"}.  This property is only relevant when the object \
can accept mouse clicks which is determined by the @qcode{\"pickableparts\"} \
property.  @xref{XREF__objname__pickableparts, , @w{pickableparts property}}.";
        s.category = "Mouse Interaction";

      case "interruptible"
        s.doc = "Specify whether this object's callback functions may be \
interrupted by other callbacks.  By default __prop__ is @qcode{\"on\"} \
and callbacks that make use of @code{drawnow}, @code{figure}, @code{waitfor}, \
@code{getframe} or @code{pause} functions are eventually interrupted.\n\
@xref{Callbacks, , @w{Callbacks section}}.";
        s.category = "Callback Execution";

      case "parent"
        s.doc = "Handle of the parent graphics object.";
        s.valid = valid_handle;
        s.category = "Parent/Children";

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
        s.category = "Mouse Interaction";

      case "selected"
        s.doc = "Property indicates whether this __objname__ is selected.";
        s.category = "Mouse Interaction";

      case "selectionhighlight"
        s.doc = "If __prop__ is @qcode{\"on\"}, then the __objname__'s \
selection state is visually highlighted.";
        s.category = "Mouse Interaction";

      case "tag"
        s.doc = "A user-defined string to label the graphics object.";
        s.valid = valid_string;
        s.category = "Object Identification";

      case "type"
        s.doc = "Class name of the graphics object.  __prop__ is \
always @qcode{\"__objname__\"}.";
        s.valid = valid_string;
        s.printdefault = false;
        s.category = "Object Identification";

      case "userdata"
        s.doc = "User-defined data to associate with the graphics object.";
        s.valid = "Any Octave data";
        s.category = "Object Identification";

      case "visible"
        s.doc = "If __prop__ is @qcode{\"off\"}, the __objname__ is \
not rendered on screen.";
        s.category = "Display";
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
        ## Use base category.

      case "parent"
        s.doc = "Root object has no parent graphics object.  __prop__ \
is always empty.";
        ## Use base category.

      case "hittest"
        s.doc = doc_unused;
        ## Use base category.

      case "pickableparts"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "callbackobject"
        s.doc = "Graphics handle of the current object whose callback is \
executing.";
        s.valid = valid_handle;
        s.category = "Callback Execution";

      case "commandwindowsize"
        s.doc = "The number of columns and rows displayed in a newly created \
command window.";
        s.valid = valid_2elvec;
        s.category = "Command Window Display";

      case "currentfigure"
        s.doc = "Graphics handle of the current figure.";
        s.valid = valid_handle;
        s.category = "Object Identification";

      case "diary"
        s.doc = "If __prop__ is @qcode{\"on\"}, the Octave command window \
session is saved to file.  @xref{XREFrootdiaryfile, , @w{diaryfile property}}.";
        s.category = "Command Logging";

      case "diaryfile"
        s.doc = "The name of the diary file.  \
@xref{XREFdiary, , @w{diary function}}.";
        s.valid = valid_string;
        s.category = "Command Logging";

      case "echo"
        s.doc = "Control whether Octave displays commands executed from \
scripts.  @xref{XREFecho, , @w{echo function}}.";
        s.category = "Command Window Display";

      case "fixedwidthfontname"
        s.doc = "Name of the fixed-width font that will be used for \
graphics objects when the @qcode{fontname} property is set to \"FixedWidth\".";
        s.valid = valid_string;
        s.category = "Command Window Display";

      case "format"
        s.doc = "This property is a wrapper around the @code{format} function.\
  @xref{XREFformat, , @w{format function}}.";
        s.category = "Command Window Display";

      case "formatspacing"
        s.doc = "This property is a wrapper around the @code{format} function.\
  @xref{XREFformat, , @w{format function}}.";
        s.category = "Command Window Display";

      case "monitorpositions"
        s.doc = "Reports the width and height of connected monitors.  Note: \
Octave only partially implements __prop__.  Only information about the primary \
monitor is stored in __prop__ which is the same information stored in the \
@ref{XREFrootscreensize, , @w{@qcode{\"screensize\"} property}}.";
        s.printdefault = false;
        s.valid = valid_4elvec;
        s.category = "Screen Information";

      case "pointerlocation"
        s.doc = doc_unused;
        s.valid = valid_2elvec;
        s.category = "Pointer Information";

      case "pointerwindow"
        s.doc = doc_unused;
        s.valid = valid_handle;
        s.category = "Pointer Information";

      case "screendepth"
        s.doc = "Color depth in bits per pixel of the display.";
        s.valid = "double";
        s.printdefault = false;
        s.category = "Screen Information";

      case "screenpixelsperinch"
        s.doc = "The screen resolution of the primary display in units of \
pixels per inch.";
        s.valid = "double";
        s.printdefault = false;
        s.category = "Screen Information";

      case "screensize"
        s.doc = "Size of the primary display represented as the four-element \
vector [left, bottom, width, height].";
        s.valid = valid_4elvec;
        s.printdefault = false;
        s.category = "Screen Information";

      case "showhiddenhandles"
        s.doc = "If __prop__ is @qcode{\"on\"}, all graphics objects handles \
are visible in their parents' children list, regardless of the value of their \
@code{handlevisibility} property.";
        s.category = "Parent/Children";

      case "units"
        s.doc = "The unit type used for the \
@ref{XREFrootmonitorpositions, , @w{@qcode{\"monitorpositions\"}}}, \
@ref{XREFrootpointerlocation, , @w{@qcode{\"pointerlocation\"}}}, and \
@ref{XREFrootscreensize, , @w{@qcode{\"screensize\"}}} properties.";
        s.category = "Screen Information";

    endswitch

  ## Figure properties
  elseif (strcmp (objname, "figure"))
    switch (field)
      ## Overridden shared properties
      case "clipping"
        s.doc = doc_unused;
        ## Use base category.

      case "pickableparts"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "alphamap"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Appearance";

      case "closerequestfcn"
        s.doc = "Function that is executed when a figure is deleted.  \
@xref{XREFclosereq, , closereq function}.\n\n__fcnmsg__";
        s.valid = valid_fcn;
        s.category = "Creation/Deletion";

      case "color"
        s.doc = "Color of the figure background.  \
@xref{Colors, , colorspec}.";
        s.valid = valid_color;
        s.category = "Appearance";

      case "colormap"
        s.doc = "A matrix containing the RGB color map for the current axes.";
        s.valid = "N-by-3 matrix";
        s.category = "Appearance";

      case "currentaxes"
        s.doc = "Handle to the graphics object of the current axes.";
        s.valid = valid_handle;
        s.category = "Object Identification";

      case "currentcharacter"
        s.doc = sprintf (doc_notimpl, "Tracking of the last key pressed");
        s.category = "Object Identification";

      case "currentobject"
        s.doc = "Handle to the most recently active graphics object in the \
figure.";
        s.valid = valid_handle;
        s.category = "Object Identification";

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
        s.category = "Mouse Interaction";

      case "dockcontrols"
        s.doc = sprintf (doc_notimpl, "Interactive figure docking");
        s.category = "Position";

      case "filename"
        s.doc = "The filename used when saving the plot figure.";
        s.valid = valid_string;
        s.category = "Printing/Saving";

      case "graphicssmoothing"
        s.doc = "Use smoothing techniques to reduce the appearance of jagged \
lines.";
        s.category = "Appearance";

      case "innerposition"
        s.doc = "The @qcode{\"innerposition\"} property is the same as the \
@ref{XREFfigureposition, , @w{@qcode{\"position\"} property}}.";
        s.valid = valid_4elvec;
        s.category = "Position";

      case "integerhandle"
        s.doc = "Assign the next lowest unused integer as the Figure number.";
        s.category = "Object Identification";

      case "inverthardcopy"
        s.doc = "Replace the figure and axes background color with white when \
printing.";
        s.category = "Printing/Saving";

      case "keypressfcn"
        s.doc = "Callback function executed when a keystroke event \
happens while the figure has focus.  The actual key that was pressed \
can be retrieved using the second argument 'evt' of the function.\
\n\n__fcnmsg__";
        s.valid = valid_fcn;
        s.category = "Keyboard Interaction";

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
        s.category = "Keyboard Interaction";

      case "menubar"
        s.doc = "Control the display of the figure menu bar at the top \
of the figure.";
        s.category = "Mouse Interaction";

      case "name"
        s.doc = "Name to be displayed in the figure title bar.  The name is \
displayed to the right of any title determined by the @code{numbertitle} \
property.";
        s.valid = valid_string;
        s.category = "Appearance";

      case "nextplot"
        s.doc = "__prop__ is used by high level plotting functions to \
decide what to do with axes already present in the figure.  \
@xref{XREFnewplot, , @w{newplot function}}.";
        s.category = "Object Identification";

      case "number"
        s.doc = "Number of the current __objname__.";
        s.valid = "double";
        s.category = "Object Identification";

      case "numbertitle"
        s.doc = "Display \"Figure\" followed by the numerical figure handle \
value in the figure title bar.";
        s.category = "Appearance";

      case "outerposition"
        s.doc = "Specify the position and size of the figure including \
the top menubar and the bottom status bar.  \
The four elements of the vector are the coordinates of the lower left corner \
and width and height of the figure.  \
@xref{XREFfigureunits, , @w{units property}}.";
        s.valid = valid_4elvec;
        s.category = "Position";

      case "paperorientation"
        s.doc = "The value for the @code{papersize}, and @code{paperposition} \
properties depends upon __prop__.  The horizontal and vertical values for \
@code{papersize} and @code{paperposition} reverse order \
when __prop__ is switched between @code{\"portrait\"} and \
@code{\"landscape\"}.";
        s.category = "Printing/Saving";

      case "paperposition"
        s.doc = "Vector @code{[left bottom width height]} defining the \
position and size of the figure (in @code{paperunits} units) on the printed \
page.  The position @code{[left bottom]} defines the lower left corner of the \
figure on the page, and the size is defined by @code{[width height]}.  For \
output formats not implicitly rendered on paper, @code{width} and \
@code{height} define the size of the image and the position information is \
ignored.  __modemsg__.";
        s.valid = valid_4elvec;
        s.category = "Printing/Saving";

      case "paperpositionmode"
        s.doc = "If __prop__ is set to @qcode{\"auto\"}, the \
@code{paperposition} property is automatically computed: the printed \
figure will have the same size as the on-screen figure and will be centered \
on the output page.  Setting the __prop__ to @code{\"auto\"} does not modify \
the value of the @code{paperposition} property.";
        s.category = "Printing/Saving";

      case "papersize"
        s.doc = "Vector @code{[width height]} defining the size of the \
paper for printing.  Setting the __prop__ property to a value, not associated \
with one of the defined @code{papertypes} and consistent with the setting for \
@code{paperorientation}, forces the @code{papertype} property to the value \
@qcode{\"<custom>\"}.  If __prop__ is set to a value associated with a \
supported @code{papertype} and consistent with the @code{paperorientation}, \
the @code{papertype} value is modified to the associated value.";
        s.valid = valid_2elvec;
        s.category = "Printing/Saving";

      case "papertype"
        s.doc = "Name of the paper used for printed output.  \
Setting __prop__ also changes @code{papersize}, while maintaining consistency \
with the @code{paperorientation} property.";
        s.category = "Printing/Saving";

      case "paperunits"
        s.doc = "The unit used to compute the @code{paperposition} property.  \
The conversion from physical units (e.g., @code{\"inches\"}) is dependent on \
the @code{screenpixelsperinch} property of the root object.";
        s.category = "Printing/Saving";

      case "pointer"
        s.doc = "Name of the mouse pointer shape associated with the canvas \
of the figure.  When __prop__ is \"custom\", the shape is determined by \
the @code{pointershapecdata} property.\n\n\
__prop__ has no effect when the figure is in zoom, pan, or rotate mode.  \
In this case, Octave automatically uses a pointer shape appropriate \
to the mode.";
        s.category = "Mouse Interaction";

      case "pointershapecdata"
        s.doc ="m-by-m matrix defining a custom pointer.  Each \
element defines a pixel with the element (1,1) representing the \
top-left pixel.  A value of 1 is colored black, a value of 2 is colored white, \
and all other values are rendered as transparent.";
        s.valid = "16-by-16 or 32-by-32 Matrix";
        s.category = "Mouse Interaction";

      case "pointershapehotspot"
        s.doc ="For custom pointers only __prop__ defines the row and column \
of the pixel in @code{pointershapecdata} that is used as the pointer location.";
        s.valid = valid_2elvec;
        s.category = "Mouse Interaction";

      case "position"
        s.doc = "Specify the position and size of the figure canvas.  \
The four elements of the vector are the coordinates of the lower left corner \
and width and height of the figure.  \
@xref{XREFfigureunits, , @w{units property}}.";
        s.valid = valid_4elvec;
        s.category = "Position";

      case "renderer"
        s.doc = "Rendering engine used for printing when @code{renderermode} \
is \"manual\".  __modemsg__.";
        s.category = "Printing/Saving";

      case "renderermode"
        s.doc = "Control whether the rendering engine used for printing is \
chosen automatically or specified by the @code{renderer} property.  \
@xref{XREFprint, , @w{print function}}.";
        s.category = "Printing/Saving";

      case "resize"
        s.doc = "Control whether the figure can be resized by dragging the \
window borders and corners using a mouse.  When __prop__ is @qcode{\"off\"} \
mouse interactions are disabled but the figure can still be resized by \
changing its @qcode{\"position\"} property.";
        s.category = "Mouse Interaction";

      case "resizefcn"
        s.doc = "__prop__ is deprecated.  Use @code{sizechangedfcn} instead.";
        s.valid = valid_fcn;
        s.category = "Mouse Interaction";

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
Shift+Left-click, Middle click, or combined Left-click and Right-click.\n\
@item open:\n\
Double Left-click.\n\
@end table";
        s.category = "Mouse Interaction";

      case "sizechangedfcn"
        s.doc = "Callback triggered when the figure window size is changed.\
\n\n__fcnmsg__";
        s.valid = valid_fcn;
        s.category = "Mouse Interaction";

      case "toolbar"
        s.doc = "Control the display of the toolbar (along the bottom of the \
menubar) and the status bar.  When set to @qcode{\"auto\"}, the display is \
based on the value of the @code{menubar} property.";
        s.category = "Mouse Interaction";

      case "units"
        s.doc = "The unit used to compute the @code{position} and \
@code{outerposition} properties.";
        s.category = "Position";

      case "windowbuttondownfcn"
        s.doc = "@xref{XREFfigurewindowbuttonupfcn, , \
@w{windowbuttonupfcn property}}.";
        s.valid = valid_fcn;
        s.category = "Mouse Interaction";

      case "windowbuttonmotionfcn"
        s.doc = "@xref{XREFfigurewindowbuttonupfcn, , \
@w{windowbuttonupfcn property}}.";
        s.valid = valid_fcn;
        s.category = "Mouse Interaction";

      case "windowbuttonupfcn"
        s.doc = "With @code{windowbuttondownfcn} and \
@code{windowbuttonmotionfcn}, the mouse callback functions.  These \
callback functions are called when a mouse button is pressed, dragged, or \
released respectively.  When these callback functions are executed, the \
@code{currentpoint} property holds the current coordinates of the cursor.\
\n\n__fcnmsg__";
        s.valid = valid_fcn;
        s.category = "Mouse Interaction";

      case "windowkeypressfcn"
        s.doc = "Function that is executed when a key is pressed and \
the figure has focus.\n\n__fcnmsg__";
        s.valid = valid_fcn;
        s.category = "Keyboard Interaction";

      case "windowkeyreleasefcn"
        s.doc = "Function that is executed when a key is released and \
the figure has focus.\n\n__fcnmsg__";
        s.valid = valid_fcn;
        s.category = "Keyboard Interaction";

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
        s.category = "Mouse Interaction";

      case "windowstate"
        s.doc = sprintf (doc_notimpl, "Window state adjustment");
        s.category = "Display";

      case "windowstyle"
        s.doc = "The window style of a figure.  One of the following values:\n\
@table @code\n\
@item normal\n\
The window may be unselected and other windows may be shown in front of the \
window.\n\
@item modal\n\
The window will stay on top of all normal figures until it is dismissed.\n\
@item docked\n\
Unimplemented.\n\
@end table\n\
\n\
Changing modes of a visible figure may cause the figure to close and reopen.";
        s.category = "Display";

    endswitch

  ## Axes properties
  elseif (strcmp (objname, "axes") || strcmp (objname, "legend"))
    switch (field)
      ## Overridden shared properties
      case "clipping"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "alim"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Color and Transparency";

      case "alimmode"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Color and Transparency";

      case "alphamap"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Color and Transparency";

      case "alphascale"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Color and Transparency";

      case "ambientlightcolor"
        s.doc = doc_unused;
        s.category = "Color and Transparency";

      case "box"
        s.doc = "Control whether the __objname__ has a surrounding box.";
        if (strcmp (objname, "legend"))
          s.category = "Legend Outline Appearance";
        else
          s.category = "Axes Box Appearance";
        endif

      case "boxstyle"
        s.doc = "For 3-D axes, control whether the @qcode{\"full\"} \
box is drawn or only the 3 @qcode{\"back\"} axes.";
        s.category = "Axes Box Appearance";

      case "cameraposition"
        s.doc = "Coordinates of the camera position viewing the __objname__.  \
        __modemsg__.";
        s.valid = valid_3elvec;
        s.category = "Camera and View Controls";

      case "camerapositionmode"
        s.doc = "Current state of the camera position property, whether \
automatically set according to the @ref{XREFview, , view function}, or \
manually set with the \
@ref{XREFaxescameraposition, , @w{@qcode{\"cameraposition\"} property}}.";
        s.category = "Camera and View Controls";

      case "cameratarget"
        s.doc = "Coordinates of the point at which the viewing camera is \
aimed.  __modemsg__.";
        s.valid = valid_3elvec;
        s.category = "Camera and View Controls";

      case "cameratargetmode"
        s.doc = "Current state of camera target property, either manually \
set with the \
@ref{XREFaxescameratarget, , @w{@qcode{\"cameratarget\"} property}} or \
automatically positioned at the center of the axes plot area.";
        s.category = "Camera and View Controls";

      case "cameraupvector"
        s.doc = "A 3-element vector defining the upward direction of the \
current view.  Note that the default is [0 1 0] for 2-D plots and [0 0 1] for \
3-D plots.  __modemsg__.";
        s.valid = valid_3elvec;
        s.category = "Camera and View Controls";

      case "cameraupvectormode"
        s.doc = "Current state of camera up vector property, set to manual \
when the \
@ref{XREFaxescameraupvector, , @w{@qcode{\"cameraupvector\"} property}} is \
used to change the vector from the 2-D or 3-D default values.";
        s.category = "Camera and View Controls";

      case "cameraviewangle"
        s.doc = "The camera's field of view defined as an angle between 0 \
and 180 degrees.  __modemsg__.";
        s.valid = "scalar";
        s.category = "Camera and View Controls";

      case "cameraviewanglemode"
        s.doc = "Current state of the camera view angle property, either \
manually set with the \
@ref{XREFaxescameraviewangle, , @w{@qcode{\"cameraviewangle\"} property}} \
or automatically set by Octave to include all visible objects.";
        s.category = "Camera and View Controls";

      case "clim"
        s.doc = "Define limits for the color axis of __objname__ \
children that have the @qcode{cdata} property.  \
__modemsg__.";
        s.valid = valid_2elvec;
        s.category = "Color and Transparency";

      case "climmode"
        s.doc = "Current state of the color limit mode, either \
manually set by the \
@ref{XREFaxesclim, , @w{@qcode{\"clim\"} property}} or automatically set by \
Octave to the minimum and maximum @qcode{cdata} values of __objname__'s \
children.";
        s.category = "Color and Transparency";

      case "clippingstyle"
        s.doc = doc_unused;
        s.category = "Display";

      case "color"
        s.doc = "Color of the __objname__ background.  \
@xref{Colors, , colorspec}.";
        s.valid = valid_color;
        if (strcmp (objname, "legend"))
          s.category = "Legend Item Appearance";
        else
          s.category = "Axes Box Appearance";
        endif
      case "colormap"
        s.doc = "A matrix containing the RGB color map for this __objname__ \
object.";
        s.valid = "N-by-3 matrix";
        s.category = "Color and Transparency";

      case "colororder"
        s.doc = "RGB values used by plot function for automatic line \
coloring.";
        s.valid = "N-by-3 RGB matrix";
        s.category = "Automatic Line Properties";

      case "colororderindex"
        s.doc = doc_unused;
        s.category = "Automatic Line Properties";

      case "colorscale"
        s.doc = sprintf (doc_notimpl, "Automatic linear/log color scaling");
        s.category = "Color and Transparency";

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
        s.category = "Mouse Interaction";

      case "dataaspectratio"
        s.doc = "Specify the relative height and width of the data \
displayed in the axes.  Setting @code{dataaspectratio} to \
@w{@code{[1, 2]}} causes the length of one unit as displayed on the x-axis \
to be the same as the length of 2 units on the y-axis.  \
@xref{XREFdaspect, , daspect function}.  __modemsg__.";
        s.valid = valid_3elvec;
        s.category = "Axes Box Appearance";

      case "dataaspectratiomode"
        s.doc = "Current state of the data aspect ratio mode, either \
manually set by the \
@ref{XREFaxesdataaspectratio, , @w{@qcode{\"dataaspectratio\"} property}} or \
automatically set by Octave in combination with other display properties to \
fit the data in the current view.";
        s.category = "Axes Box Appearance";

      case "fontangle"
        s.doc = doc_fontangle;
        s.category = "Text Appearance";

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;
        s.category = "Text Appearance";

      case "fontsize"
        s.doc = [doc_fontsize, "  __modemsg__."];
        s.valid = "scalar";
        s.category = "Text Appearance";

      case "fontsizemode"
        s.doc = "Current state of the fontsize mode, either manually set by \
the @ref{XREFaxesfontsize, , @w{@qcode{\"fontsize\"} property}} or \
automatically set by Octave to maintain readability.";
        s.category = "Text Appearance";

      case "fontsmoothing"
        s.doc = "Control whether any text associated with __objname__ is \
anti-aliased.";
        s.category = "Text Appearance";

      case "fontunits"
        s.doc = doc_fontunits;
        s.category = "Text Appearance";

      case "fontweight"
        s.doc = doc_fontweight;
        s.category = "Text Appearance";

      case "gridalpha"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Axes Grid Appearance";

      case "gridalphamode"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Axes Grid Appearance";

      case "gridcolor"
        s.doc = "Color of the major grid lines.  \
@xref{Colors, , colorspec}.  __modemsg__.";
        s.valid = packopt ({markdef(valid_color), "@qcode{\"none\"}"});
        s.category = "Axes Grid Appearance";

      case "gridcolormode"
        s.doc = "Current state of the gridcolor mode, either manually set by \
the @ref{XREFaxesgridcolor, , @w{@qcode{\"gridcolor\"} property}} or \
automatically set by Octave to the default value.";
        s.category = "Axes Grid Appearance";

      case "gridlinestyle"
        s.doc = "@xref{Line Styles}.";
        s.category = "Axes Grid Appearance";

      case "innerposition"
        s.doc = "The @qcode{\"innerposition\"} property is the same as the \
@ref{XREFaxesposition, , @w{@qcode{\"position\"} property}}.";
        s.valid = valid_4elvec;
        s.category = "Axes Grid Appearance";

      case "interactions"
        s.doc = sprintf (doc_notimpl, "Interaction objects");
        s.category = "Callback Execution";

      case "labelfontsizemultiplier"
        s.doc = "Ratio between the x/y/zlabel fontsize and the tick \
label fontsize.";
        s.category = "Text Appearance";

      case "layer"
        s.doc = "Control whether the axes is drawn below child graphics \
objects (ticks, labels, etc.@: covered by plotted objects) or above.";
        s.category = "Axes Box Appearance";

      case "layout"
        s.doc = sprintf (doc_notimpl, "Tiled and gridded chart layout");
        s.category = "Axes Box Appearance";

      case "legend"
        s.doc = [sprintf(doc_notimpl, "Legend property control"), "  Use \
the @ref{XREFlegend, , legend function} to set legend properties."];
        s.category = "Text Appearance";

      case "linestyleorder"
        s.doc = [sprintf(doc_notimpl, "Linestyle order specification"), "  \
The first linestyle specified in the __prop__ vector will be the style used \
for all subsequent lines."];
        s.category = "Automatic Line Properties";

      case "linestyleorderindex"
        s.doc = sprintf (doc_notimpl, "Linestyle order selection");
        s.category = "Automatic Line Properties";

      case "linewidth"
        s.doc = "Width of the main axes lines.";
        s.category = "Axes Box Appearance";

      case "minorgridalpha"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Axes Grid Appearance";

      case "minorgridalphamode"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Axes Grid Appearance";

      case "minorgridcolor"
        s.doc = "Color of the minor grid lines.  \
@xref{Colors, , colorspec}.  __modemsg__.";
        s.valid = packopt ({markdef(valid_color), "@qcode{\"none\"}"});
        s.category = "Axes Grid Appearance";

      case "minorgridcolormode"
        s.doc = "Current state of the minorgridcolor mode, either manually \
set by the \
@ref{XREFaxesminorgridcolor, , @w{@qcode{\"minorgridcolor\"} property}} or \
automatically set by Octave to the default value.";
        s.category = "Axes Grid Appearance";

      case "minorgridlinestyle"
        s.doc = "@xref{Line Styles}.";
        s.category = "Axes Grid Appearance";

      case "mousewheelzoom"
        s.doc = "Fraction of axes limits to zoom for each wheel movement.";
        s.valid = "scalar in the range (0, 1)";
        s.category = "Mouse Interaction";

      case "nextplot"
        s.doc = "__prop__ is used by high level plotting functions to \
decide what to do with graphics objects already present in the axes.  \
@xref{XREFnewplot, , @w{newplot function}}.  The state of __prop__ \
is typically controlled using the @code{hold} function.  \
@xref{XREFhold, , @w{hold function}}.";
        s.category = "Object Identification";

      case "nextseriesindex"
        s.doc = sprintf (doc_notimpl, "LineStyleOrder and ColorOrder index \
selection");
        s.category = "Automatic Line Properties";

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
        s.category = "Position";

      case "plotboxaspectratio"
        s.doc = "@xref{XREFpbaspect, , pbaspect function}.  \
__modemsg__.";
        s.category = "Position";

      case "plotboxaspectratiomode"
        s.doc = "Current state of the plot box aspect ratio mode, either \
manually set by the \
@ref{XREFaxesdataaspectratio, , @w{@qcode{\"dataaspectratio\"} property}} or \
automatically set by Octave in combination with other display properties to \
fit the data in the current view.";
        s.category = "Position";

      case "position"
        if (strcmp (objname, "legend"))
          s.doc = "Specify the position of the legend excluding its title.  \
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
        s.category = "Position";

      case "positionconstraint"
        s.doc = "Specify which of @qcode{\"innerposition\"} or \
@qcode{\"outerposition\"} properties takes precedence when axes \
annotations extent changes.  \
@xref{XREFaxesinnerposition, , @w{@qcode{\"innerposition\"} property}}, \
and @ref{XREFaxesouterposition, , @w{@qcode{\"outerposition\"} property}}.";
        s.category = "Position";

      case "projection"
        s.doc = sprintf (doc_notimpl, "Orthographic/perspective projection \
adjustment");
        s.category = "Camera and View Controls";

      case "sortmethod"
        s.doc = sprintf (doc_notimpl, "Child display order control");
        s.category = "Parent/Children";

      case "tickdir"
        s.doc = "Control whether axes tick marks project @qcode{\"in\"} to \
the plot box or @qcode{\"out\"}.  The value @qcode{\"both\"} will draw tick \
marks both in and out.  The value @qcode{\"none\"} means no tick marks will be \
drawn, although tick labels will still be rendered.  __modemsg__.  Note that \
the default is @qcode{\"in\"} for 2-D and @qcode{\"out\"} for 3-D plots.";
        s.category = "Axes Box Appearance";

      case "tickdirmode"
        s.doc = "Current state of the tickdir mode, either manually set by \
the @ref{XREFaxestickdir, , @w{@qcode{\"tickdir\"} property}} or \
automatically set to the default for the current view.";
        s.category = "Axes Box Appearance";

      case "ticklabelinterpreter"
        s.doc = "Control the way x/y/zticklabel properties are interpreted.\n\
@xref{Use of the \"interpreter\" Property, , @w{Use of the \"interpreter\" Property}}.";
        s.category = "Text Appearance";

      case "ticklength"
        s.doc = "Two-element vector @code{[2Dlen 3Dlen]} specifying the \
length of the tickmarks relative to the longest visible axis.";
        s.valid = valid_2elvec;
        s.category = "Axes Box Appearance";

      case "tightinset"
        s.doc = "Size of the @code{[left bottom right top]} margins \
around the axes that enclose labels and title annotations.";
        s.valid = valid_4elvec;
        s.printdefault = false;
        s.category = "Text Appearance";

      case "title"
        s.doc = "Graphics handle of the title text object.";
        s.valid = valid_handle;
        s.category = "Object Identification";

      case "titlefontsizemultiplier"
        s.doc = "Ratio between the title fontsize and the tick label fontsize.";
        s.valid = "positive scalar";
        s.category = "Text Appearance";

      case "titlefontweight"
        s.doc = "Control variant of base font used for the axes title.";
        s.category = "Text Appearance";

      case "toolbar"
        s.doc = [sprintf(doc_notimpl, "AxesToolbar objects")];
        s.category = "Axes Box Appearance";

      case "units"
        if (strcmp (objname, "legend"))
          s.doc = "Units used to interpret the @qcode{\"position\"}, \
 property.";
        else
          s.doc = "Units used to interpret the @qcode{\"position\"}, \
@qcode{\"outerposition\"}, and @qcode{\"tightinset\"} properties.";
        endif
        s.category = "Position";

      case "view"
        s.doc = "Two-element vector @code{[azimuth elevation]} specifying \
the viewpoint for three-dimensional plots.";
        s.valid = valid_2elvec;
        s.category = "Camera and View Controls";

      case "xaxis"
        s.doc = [sprintf(doc_notimpl, "Axes Ruler objects")];
        s.category = "Axes Box Appearance";

      case "xaxislocation"
        s.doc = "Control the x-axis location.";
        s.category = "Axes Box Appearance";

      case "xcolor"
        s.doc = "Color of the x-axis.  @xref{Colors, , colorspec}.  \
__modemsg__.";
        s.valid = packopt ({markdef(valid_color), "@qcode{\"none\"}"});
        s.category = "Axes Box Appearance";

      case "xcolormode"
        s.doc = "Current state of the setting determining the color that is \
 applied to the x-axis grid lines.  If set to \"auto\" and/or the \
@ref{XREFaxesgridcolormode, , @w{@qcode{\"gridcolormode\"} property}} is set \
to \"manual\", the x-axis grid color will be defined by the \
@ref{XREFaxesgridcolor, , @w{@qcode{\"gridcolor\"} property}}.  Otherwise \
the x-axis grid color will be defined by the \
@ref{XREFaxesxcolor, , @w{@qcode{\"xcolor\"} property}}.";
        s.category = "Axes Box Appearance";

      case "xdir"
        s.doc = "Direction of the x axis: @qcode{\"normal\"} is left \
to right in default 2-D and 3-D views.";
        s.category = "Axes Box Appearance";

      case "xgrid"
        s.doc = "Control whether major x grid lines are displayed.";
        s.category = "Axes Grid Appearance";

      case "xlabel"
        s.doc = "Graphics handle of the x label text object.";
        s.valid = valid_handle;
        s.category = "Text Appearance";

      case "xlim"
        s.doc = "Two-element vector @code{[xmin xmax]} specifying the limits \
for the x-axis.  __modemsg__.   @xref{XREFxlim, , @w{xlim function}}.";
        s.valid = valid_2elvec;
        s.category = "Axes Box Appearance";

      case "xlimitmethod"
        s.doc = "Method used to determine the x-axis limits when the \
@code{xlimmode} property is @qcode{\"auto\"}.  The default value, \
@qcode{\"tickaligned\"} makes limits align with the closest ticks.  With \
value @qcode{\"tight\"} the limits are adjusted to enclose all the graphics \
objects in the axes, while with value @qcode{\"padded\"}, an additional \
margin of about 7%% of the data extent is added around the objects.  \
@xref{XREFaxis, , @w{axis function}}.";
        s.category = "Axes Box Appearance";

      case "xlimmode"
        s.doc = "Current state of the x-axis limit selection method, either \
manually set with the @ref{XREFaxesxlim, , @w{@qcode{\"xlim\"} property}} \
or automatically set to span the plotted data according to the \
@ref{XREFaxesxlimitmethod, , @w{@qcode{\"xlimitmethod\"} property}}.";
        s.category = "Axes Box Appearance";

      case "xminorgrid"
        s.doc = "Control whether minor x grid lines are displayed.";
        s.category = "Axes Grid Appearance";

      case "xminortick"
        s.doc = "Control whether minor x tick marks are displayed.";
        s.category = "Axes Grid Appearance";

      case "xscale"
        s.doc = "Set the x-axis to a linear or logarithmic scale.";
        s.category = "Axes Grid Appearance";

      case "xtick"
        s.doc = "Position of x tick marks.  __modemsg__.";
        s.valid = "vector";
        s.printdefault = false;
        s.category = "Axes Grid Appearance";

      case "xticklabel"
        s.doc = "Labels of x tick marks.  __modemsg__.";
        s.valid = valid_cellstring;
        s.category = "Text Appearance";

      case "xticklabelmode"
        s.doc = "Setting to determine whether the xtick labels are set \
automatically by Octave or manually using the \
@ref{XREFaxesxticklabel, , @w{@qcode{\"xticklabel\"} property}}.";
        s.category = "Text Appearance";

      case "xticklabelrotation"
        s.doc = [sprintf(doc_notimpl, "Axis label rotation")];
        s.category = "Text Appearance";

      case "xtickmode"
        s.doc = "Setting to determine whether the xtick locations and \
spacing are set automatically by Octave or manually using the \
@ref{XREFaxesxtick, , @w{@qcode{\"xtick\"} property}}.";
        s.category = "Axes Grid Appearance";

      case "yaxis"
        s.doc = [sprintf(doc_notimpl, "Axes Ruler objects")];
        s.category = "Axes Box Appearance";

      case "yaxislocation"
        s.doc = "Control the y-axis location.";
        s.category = "Axes Box Appearance";

      case "ycolor"
        s.doc = "Color of the y-axis.  @xref{Colors, , colorspec}.";
        s.valid = packopt ({markdef(valid_color), "@qcode{\"none\"}"});
        s.category = "Axes Box Appearance";

      case "ycolormode"
        s.doc = "Current state of the setting determining the color that is \
 applied to the y-axis grid lines.  If set to \"auto\" and/or the \
@ref{XREFaxesgridcolormode, , @w{@qcode{\"gridcolormode\"} property}} is set \
to \"manual\", the y-axis grid color will be defined by the \
@ref{XREFaxesgridcolor, , @w{@qcode{\"gridcolor\"} property}}.  Otherwise \
the y-axis grid color will be defined by the \
@ref{XREFaxesycolor, , @w{@qcode{\"ycolor\"} property}}.";
        s.category = "Axes Box Appearance";

      case "ydir"
        s.doc = "Direction of the y-axis: @qcode{\"normal\"} is bottom \
to top in 2-D and front to back in 3-D default views.";
        s.category = "Axes Box Appearance";

      case "ygrid"
        s.doc = "Control whether major y grid lines are displayed.";
        s.category = "Axes Grid Appearance";

      case "ylabel"
        s.doc = "Graphics handle of the y label text object.";
        s.valid = valid_handle;
        s.category = "Text Appearance";

      case "ylim"
        s.doc = "Two-element vector @code{[ymin ymax]} specifying the limits \
for the y-axis.  __modemsg__.  @xref{XREFylim, , @w{ylim function}}.";
        s.valid = valid_2elvec;
        s.category = "Axes Box Appearance";

      case "ylimitmethod"
        s.doc = "Method used to determine the y-axis limits when the \
@code{xlimmode} property is @qcode{\"auto\"}.  The default value, \
@qcode{\"tickaligned\"} makes limits align with the closest ticks.  With \
value @qcode{\"tight\"} the limits are adjusted to enclose all the graphics \
objects in the axes, while with value @qcode{\"padded\"}, an additional \
margin of about 7%% of the data extent is added around the objects.  \
@xref{XREFaxis, , @w{axis function}}.";
        s.category = "Axes Box Appearance";

      case "ylimmode"
        s.doc = "Current state of the y-axis limit selection method, either \
manually set with the @ref{XREFaxesylim, , @w{@qcode{\"ylim\"} property}} \
or automatically set to span the plotted data according to the \
@ref{XREFaxesylimitmethod, , @w{@qcode{\"ylimitmethod\"} property}}.";
        s.category = "Axes Box Appearance";

      case "yminorgrid"
        s.doc = "Control whether minor y grid lines are displayed.";
        s.category = "Axes Grid Appearance";

      case "yminortick"
        s.doc = "Control whether minor y tick marks are displayed.";
        s.category = "Axes Grid Appearance";

      case "yscale"
        s.doc = "Set the y-axis to a linear or logarithmic scale.";
        s.category = "Axes Grid Appearance";

      case "ytick"
        s.doc = "Position of y tick marks.  __modemsg__.";
        s.valid = "vector";
        s.printdefault = false;
        s.category = "Axes Grid Appearance";

      case "yticklabel"
        s.doc = "Labels of y tick marks.  __modemsg__.";
        s.valid = valid_cellstring;
        s.category = "Text Appearance";

      case "yticklabelmode"
        s.doc = "Setting to determine whether the ytick labels are set \
automatically by Octave or manually using the \
@ref{XREFaxesyticklabel, , @w{@qcode{\"yticklabel\"} property}}.";
        s.category = "Text Appearance";

      case "yticklabelrotation"
        s.doc = [sprintf(doc_notimpl, "Axis label rotation")];
        s.category = "Text Appearance";

      case "ytickmode"
        s.doc = "Setting to determine whether the ytick locations and \
spacing are set automatically by Octave or manually using the \
@ref{XREFaxesytick, , @w{@qcode{\"ytick\"} property}}.";
        s.category = "Axes Grid Appearance";

      case "zaxis"
        s.doc = [sprintf(doc_notimpl, "Axes Ruler objects")];
        s.category = "Axes Box Appearance";

      case "zcolor"
        s.doc = "Color of the z-axis.  @xref{Colors, , colorspec}.";
        s.valid = packopt ({markdef(valid_color), "@qcode{\"none\"}"});
        s.category = "Axes Box Appearance";

      case "zcolormode"
        s.doc = "Current state of the setting determining the color that is \
 applied to the z-axis grid lines.  If set to \"auto\" and/or the \
@ref{XREFaxesgridcolormode, , @w{@qcode{\"gridcolormode\"} property}} is set \
to \"manual\", the z-axis grid color will be defined by the \
@ref{XREFaxesgridcolor, , @w{@qcode{\"gridcolor\"} property}}.  Otherwise \
the z-axis grid color will be defined by the \
@ref{XREFaxeszcolor, , @w{@qcode{\"zcolor\"} property}}.";
        s.category = "Axes Box Appearance";

      case "zdir"
        s.doc = "Direction of the y-axis: @qcode{\"normal\"} is bottom \
to top in default 3-D views.";
        s.category = "Axes Box Appearance";

      case "zgrid"
        s.doc = "Control whether major z grid lines are displayed.";
        s.category = "Axes Grid Appearance";

      case "zlabel"
        s.doc = "Graphics handle of the z label text object.";
        s.valid = valid_handle;
        s.category = "Text Appearance";

      case "zlim"
        s.doc = "Two-element vector @code{[zmin zmaz]} specifying the limits \
for the z-axis.  __modemsg__.  @xref{XREFzlim, , @w{zlim function}}.";
        s.valid = valid_2elvec;
        s.category = "Axes Box Appearance";

      case "zlimitmethod"
        s.doc = "Method used to determine the z-axis limits when the \
@code{xlimmode} property is @qcode{\"auto\"}.  The default value, \
@qcode{\"tickaligned\"} makes limits align with the closest ticks.  With \
value @qcode{\"tight\"} the limits are adjusted to enclose all the graphics \
objects in the axes, while with value @qcode{\"padded\"}, an additional \
margin of about 7%% of the data extent is added around the objects.  \
@xref{XREFaxis, , @w{axis function}}.";
        s.category = "Axes Box Appearance";

      case "zlimmode"
        s.doc = "Current state of the z-axis limit selection method, either \
manually set with the @ref{XREFaxeszlim, , @w{@qcode{\"zlim\"} property}} \
or automatically set to span the plotted data according to the \
@ref{XREFaxeszlimitmethod, , @w{@qcode{\"zlimitmethod\"} property}}.";
        s.category = "Axes Box Appearance";

      case "zminorgrid"
        s.doc = "Control whether minor z grid lines are displayed.";
        s.category = "Axes Grid Appearance";

      case "zminortick"
        s.doc = "Control whether minor z tick marks are displayed.";
        s.category = "Axes Grid Appearance";

      case "zscale"
        s.doc = "Set the z-axis to a linear or logarithmic scale.";
        s.category = "Axes Grid Appearance";

      case "ztick"
        s.doc = "Position of z tick marks.  __modemsg__.";
        s.valid = "vector";
        s.printdefault = false;
        s.category = "Axes Grid Appearance";

      case "zticklabel"
        s.doc = "Labels of z tick marks.  __modemsg__.";
        s.valid = valid_cellstring;
        s.category = "Text Appearance";

      case "zticklabelmode"
        s.doc = "Setting to determine whether the ztick labels are set \
automatically by Octave or manually using the \
@ref{XREFaxeszticklabel, , @w{@qcode{\"zticklabel\"} property}}.";
        s.category = "Text Appearance";

      case "zticklabelrotation"
        s.doc = [sprintf(doc_notimpl, "Axis label rotation")];
        s.category = "Text Appearance";

      case "ztickmode"
        s.doc = "Setting to determine whether the ztick locations and \
spacing are set automatically by Octave or manually using the \
@ref{XREFaxesztick, , @w{@qcode{\"ztick\"} property}}.";
        s.category = "Axes Grid Appearance";

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
        s.category = "Legend Item Appearance";

      case "edgecolor"
        s.doc = "Control the color of the legend outline.";
        s.valid = valid_color;
        s.category = "Legend Outline Appearance";

      case "interpreter"
        s.doc = "Control if and eventually how labels strings are interpreted \
before rendering.\n\
@xref{Use of the \"interpreter\" Property, , @w{Use of the \"interpreter\" Property}}.";
        s.category = "Text Appearance";

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
@qcode{\"alt\"}.  \
@xref{XREFfigureselectiontype, , @w{Figure @qcode{\"selectiontype\"}}}.\n\
@item Source\n\
Handle of the legend object.\n\
@item EventName\n\
Name is @qcode{\"ItemHit\"}.\n\
@end table";
        s.category = "Callback Execution";

      case "location"
        s.doc = "Control the location of the legend.";
        s.category = "Position";

      case "numcolumns"
        s.doc = "Control the number of columns used in the layout of the \
legend items.  \
For example:\n\
@example\n\
@group\n\
figure ();\n\
plot (rand (30));\n\
legend (\"numcolumns\", 3);\n\
@end group\n\
@end example\n\
__modemsg__.";
        s.valid = "scalar integer";
        s.category = "Legend Item Appearance";

      case "orientation"
        s.doc = "Control whether the legend items are arranged vertically \
(column-wise) or horizontally (row-wise).";
        s.category = "Legend Item Appearance";

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
        s.category = "Text Appearance";

      case "textcolor"
        s.doc = "Control the color of the text strings for legend items.";
        s.valid = valid_color;
        s.category = "Text Appearance";

      case "textposition"
        s.doc = "Control whether text strings are displayed on the left or \
right of their corresponding icon.";
        s.category = "Text Appearance";

    endswitch

  ## Line properties
  elseif (strcmp (objname, "line"))
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "color"
        s.doc = "Color of the line object.  @xref{Colors, , colorspec}.";
        s.valid = valid_color;
        s.category = "Line Appearance";

      case "displayname"
        s.doc = "Text for the legend entry corresponding to this line.";
        s.valid = valid_cellstring;
        s.category = "Line Appearance";

      case "linestyle"
        s.doc = "@xref{Line Styles}.";
        s.category = "Line Appearance";

      case "linewidth"
        s.doc = "Width of the line object measured in points.";
        s.category = "Line Appearance";

      case "linejoin"
        s.doc = "Control the shape of the junction of line segments.  \
This property currently only affects the printed output.";
        s.category = "Line Appearance";

      case "marker"
        s.doc = "Shape of the marker for each data point.  \
@xref{Marker Styles}.";
        s.category = "Marker Appearance";

      case "markeredgecolor"
        s.doc = "Color of the edge of the markers.  When set to \
@qcode{\"auto\"}, the marker edges have the same color as the line.  If set \
to @qcode{\"none\"}, no marker edges are displayed.  This property can also \
be set to any color.  @xref{Colors, , colorspec}.";
        s.category = "Marker Appearance";

      case "markerfacecolor"
        s.doc = "Color of the face of the markers.  When set to \
@qcode{\"auto\"}, the marker faces have the same color as the line.  If set \
to @qcode{\"none\"}, the marker faces are not displayed.  This property \
can also be set to any color.  @xref{Colors, , colorspec}.";
        s.category = "Marker Appearance";

      case "markersize"
        s.doc = "Size of the markers measured in points.";
        s.valid = "scalar";
        s.category = "Marker Appearance";

      case "xdata"
        s.doc = "Vector of x data to be plotted.";
        s.valid = "vector";
        s.category = "Line Data";

      case "xdatasource"
        s.doc = "Name of a vector in the current base workspace to use as \
x data.";
        s.valid = valid_string;
        s.category = "Line Data";

      case "ydata"
        s.doc = "Vector of y data to be plotted.";
        s.valid = "vector";
        s.category = "Line Data";

      case "ydatasource"
        s.doc = "Name of a vector in the current base workspace to use as \
y data.";
        s.valid = valid_string;
        s.category = "Line Data";

      case "zdata"
        s.doc = "Vector of z data to be plotted.";
        s.valid = "vector";
        s.category = "Line Data";

      case "zdatasource"
        s.doc = "Name of a vector in the current base workspace to use as \
z data.";
        s.valid = valid_string;
        s.category = "Line Data";

    endswitch

  ## Text properties
  elseif (strcmp (objname, "text"))
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "backgroundcolor"
        s.doc = "Color of the background area.  \
@xref{Colors, , colorspec}.";
        s.valid = valid_color;
        s.category = "Text Box Appearance";

      case "color"
        s.doc = "Color of the text.  @xref{Colors, ,colorspec}.";
        s.valid = valid_color;
        s.category = "Text Appearance";

      case "edgecolor"
        s.doc = "Color of the outline of the background area.  \
@xref{Colors, , colorspec}.";
        s.valid = valid_color;
        s.category = "Text Box Appearance";

      case "editing"
        s.doc = sprintf (doc_notimpl, "Interactive text editing");
        s.category = "Text Appearance";

      case "extent"
        s.doc = "Vector @code{[x0 y0 width height]} indicating the size \
and location of the text string.";
        s.valid = valid_4elvec;
        s.printdefault = false;
        s.category = "Position";

      case "fontangle"
        s.doc = doc_fontangle;
        s.category = "Text Appearance";

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;
        s.category = "Text Appearance";

      case "fontsmoothing"
        s.doc = "Control whether anti-aliasing is used when rendering text.";
        s.category = "Text Appearance";

      case "fontsize"
        s.doc = doc_fontsize;
        s.valid = "scalar";
        s.category = "Text Appearance";

      case "fontunits"
        s.doc = doc_fontunits;
        s.category = "Text Appearance";

      case "fontweight"
        s.doc = doc_fontweight;
        s.category = "Text Appearance";

      case "horizontalalignment"
        s.doc = "Specifies the horizontal location of the point set by the \
@ref{XREFtextposition, , @w{@qcode{\"position\"} property}} relative to the \
text.";
        s.category = "Position";

      case "interpreter"
        s.doc = "Control the way the @qcode{\"string\"} property is \
interpreted.\n\
@xref{Use of the \"interpreter\" Property, , @w{Use of the \"interpreter\" Property}}.";
        s.category = "Text Appearance";

      case "linestyle"
        s.doc = "Style of the outline.  @xref{Line Styles}.";
        s.category = "Text Box Appearance";

      case "linewidth"
        s.doc = "Width of the outline.";
        s.valid = "scalar";
        s.category = "Text Box Appearance";

      case "margin"
        s.doc = "Margins between the borders of the background area \
and the texts.  The value is currently interpreted as pixels, regardless \
of the @qcode{\"fontunits\"} property.";
        s.valid = "scalar";
        s.category = "Text Box Appearance";

      case "position"
        s.doc = "Vector @code{[X0 Y0 Z0]} where X0, Y0, and Z0 indicate the \
position of the text anchor as defined by @code{verticalalignment} and \
@code{horizontalalignment}.";
        s.valid = valid_3elvec;
        s.category = "Position";

      case "rotation"
        s.doc = "The angle of rotation for the displayed text, \
measured in degrees.";
        s.valid = "scalar";
        s.category = "Position";

      case "string"
        s.doc = "The text object string content.";
        s.valid = valid_string;
        s.category = "Text Appearance";

      case "units"
        s.doc = "Sets the measurement unit or method applied to the \
@ref{XREFtextposition, , @w{@qcode{\"position\"}}} and \
@ref{XREFtextextent, , @w{@qcode{\"extent\"}}} properties.  The default \
option \"data\" uses the same units and limits as the data plotted in the \
figure.  The \"normalized\" option applies a unitless 0 to 1 scale to the \
limits along each axis of the displayed data.";
        s.category = "Position";

      case "verticalalignment"
        s.doc = "Specifies the vertical location of the point set by the \
@ref{XREFtextposition, , @w{@qcode{\"position\"} property}} relative to the \
text.  Note that \"top\" and \"bottom\" align to the edge of the text \
box while \"cap\" and \"baseline\" refer to the edges of the text itself.";
        s.category = "Position";

    endswitch

  ## Image properties
  elseif (strcmp (objname, "image"))
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "alphadata"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = valid_scalmat;
        s.category = "Image Data";

      case "alphadatamapping"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Image Data";

      case "cdata"
        s.doc = "Color data for the image object.  Data is either stored as \
a 2-D matrix where each element's value determines that pixel's color \
according to the current colormap, or as a 3-D array where the third \
dimension contains separate red, blue, and green components for each pixel.";
        s.valid = "array";
        s.category = "Image Data";

      case "cdatamapping"
        s.doc = "Sets the method for mapping data from the \
@ref{XREFimagecdata, , @w{@qcode{\"cdata\"} property}} to the current \
colormap.  \"Direct\" mapping selects the color using the \"cdata\" value \
as an index to the current colormap.  \"Scaled\" mapping scales the \
\"cdata\" values to the range specified in the \
@ref{XREFaxesclim, , @w{@qcode{\"clim\"} axes property}}.";
        s.category = "Image Data";

      case "displayname"
        s.doc = "Text for the legend entry corresponding to this image.";
        s.valid = valid_cellstring;
        s.category = "Text Appearance";

      case "xdata"
        s.doc = "Two-element vector @code{[xfirst xlast]} specifying the x \
coordinates of the centers of the first and last columns of the image.\n\
\n\
Setting @code{xdata} to the empty matrix ([]) will restore the default value \
of @code{[1 columns(image)]}.";
        s.valid = valid_2elvec;
        s.category = "Image Data";

      case "ydata"
        s.doc = "Two-element vector @code{[yfirst ylast]} specifying the y \
coordinates of the centers of the first and last rows of the image.\n\
\n\
Setting @code{ydata} to the empty matrix ([]) will restore the default value \
of @code{[1 rows(image)]}.";
        s.valid = valid_2elvec;
        s.category = "Image Data";

    endswitch

  ## Surface properties
  elseif (strcmp (objname, "surface"))
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "alphadata"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = valid_scalmat;
        s.category = "Color and Transparency";

      case "alphadatamapping"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Color and Transparency";

      case "ambientstrength"
        s.doc = "Strength of the ambient light.  Value between 0.0 and 1.0.";
        s.valid = "scalar";
        s.category = "Lighting";

      case "backfacelighting"
        s.doc = "@qcode{\"lit\"}: The normals are used as is for lighting.  \
@qcode{\"reverselit\"}: The normals are always oriented towards the point of \
view.  @qcode{\"unlit\"}: Faces with normals pointing away from the point of \
view are unlit.";
        s.category = "Lighting";

      case "cdata"
        s.doc = "Color data values for __objname__ vertices. Data is stored \
either as a 2-D matrix the same size as \
@ref{XREFsurfacezdata, , @w{@qcode{\"zdata\"}}} where each element's value \
determines that vertex's color according to the current colormap, or as a \
3-D array where the third dimension contains separate red, blue, and green \
components for each vertex.";
        s.valid = "array";
        s.category = "Color and Transparency";

      case "cdatamapping"
        s.doc = "Sets the method for mapping data from the \
@ref{XREFsurfacecdata, , @w{@qcode{\"cdata\"} property}} to the current \
colormap.  \"Direct\" mapping selects the color using the \"cdata\" value \
as an index to the current colormap.  \"Scaled\" mapping scales the \
\"cdata\" values to the range specified in the \
@ref{XREFaxesclim, , @w{@qcode{\"clim\"} axes property}}.";
        s.category = "Color and Transparency";

      case "cdatasource"
        s.doc = "The name of a workspace variable that contains data that \
will be used for the \
@ref{XREFsurfacecdata, , @w{@qcode{\"cdata\"} property}}.  Data is \
transferred into \"cdata\" using the \
@xref{XREFrefreshdata, , @w{refreshdata function}}.";
        s.valid = valid_string;
        s.category = "Color and Transparency";

      case "diffusestrength"
        s.doc = "Strength of the diffuse reflection.  Value between 0.0 (no \
diffuse reflection) and 1.0 (full diffuse reflection).";
        s.valid = "scalar";
        s.category = "Lighting";

      case "displayname"
        s.doc = "Text for the legend entry corresponding to this surface.";
        s.category = "Text Appearance";

      case "edgealpha"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = "scalar";
        s.category = "Outline Appearance";

      case "edgecolor"
        s.doc = "Color of the edges of the __objname__ object, specified as \
either a valid color specification or one of \"none\", \"flat\", or \
\"interp\".  \"flat\" and \"interp\" will set either a single color for \
each edge or a color interpolated between two adjacent vertices using the \
color value data stored in \
@ref{XREFsurfacecdata, , @w{@qcode{\"cdata\"}}}.  \
@xref{Colors, , colorspec}.";
        s.valid = packopt ({valid_color, ...
                            "@qcode{\"none\"}", ...
                            "@qcode{\"flat\"}", ...
                            "@qcode{\"interp\"}"});
        s.category = "Outline Appearance";

      case "edgelighting"
        s.doc = "When set to a value other than @qcode{\"none\"}, the edges \
of the object are drawn with light and shadow effects.  Supported values are \
@qcode{\"none\"} (no lighting effects), @qcode{\"flat\"} (faceted look), and \
@qcode{\"gouraud\"} (linear interpolation of the lighting effects between \
the vertices).  @qcode{\"phong\"} is deprecated and has the same effect as \
@qcode{\"gouraud\"}.";
        s.category = "Outline Appearance";

      case "facealpha"
        s.doc = "Transparency level of the faces of the surface object.  Only \
double values are supported at present where a value of 0 means complete \
transparency and a value of 1 means solid faces without transparency.  Setting \
the property to @qcode{\"flat\"}, @qcode{\"interp\"} or @qcode{\"texturemap\"} \
causes the faces to not being rendered.  Additionally, the faces are not \
sorted from back to front which might lead to unexpected results when \
rendering layered transparent faces.";
        s.valid = packopt ({"scalar", ...
                            "@qcode{\"flat\"}", ...
                            "@qcode{\"interp\"}", ...
                            "@qcode{\"texturemap\"}"});
        s.category = "Faces Appearance";

      case "facecolor"
        s.doc = "Color of the faces of the __objname__ object, specified as \
either a valid color specification or one of \"none\", \"flat\", or \
\"interp\".  \"flat\" and \"interp\" will set either a single color for \
each face or a color interpolated across the face's vertices using the \
color value data stored in \
@ref{XREFsurfacecdata, , @w{@qcode{\"cdata\"}}}.  \
@xref{Colors, , colorspec}.";
        s.valid = packopt ({valid_color, ...
                            "@qcode{\"none\"}", ...
                            "@qcode{\"flat\"}", ...
                            "@qcode{\"interp\"}"});
        s.category = "Faces Appearance";

      case "facelighting"
        s.doc = "When set to a value other than @qcode{\"none\"}, the faces \
of the object are drawn with light and shadow effects.  Supported values are \
@qcode{\"none\"} (no lighting effects), @qcode{\"flat\"} (faceted look), and \
@qcode{\"gouraud\"} (linear interpolation of the lighting effects between \
the vertices).  @qcode{\"phong\"} is deprecated and has the same effect as \
@qcode{\"gouraud\"}.";
        s.category = "Faces Appearance";

      case "facenormals"
        s.doc = "Face normals are used for lighting the edges or faces if the \
@code{edgelighting} or @code{facelighting} properties are set to \
@qcode{\"flat\"}.  __modemsg__";
        s.category = "Faces Appearance";

      case "facenormalsmode"
        s.doc = "If this property is set to @qcode{\"auto\"}, \
@code{facenormals} are automatically calculated if the @code{edgelighting} or \
@code{facelighting} property are set to @qcode{\"flat\"} and at least one \
@code{light} object is present and visible in the same axes.";
        s.category = "Faces Appearance";

      case "linestyle"
        s.doc = "@xref{Line Styles}.";
        s.category = "Outline Appearance";

      case "linewidth"
        s.doc = "@xref{XREFlinelinewidth, , @w{line linewidth property}}.";
        s.category = "Outline Appearance";

      case "marker"
        s.doc = "@xref{Marker Styles}.";
        s.category = "Marker Appearance";

      case "markeredgecolor"
        s.doc = "@xref{XREFlinemarkeredgecolor, , \
@w{line markeredgecolor property}}.";
        s.category = "Marker Appearance";

      case "markerfacecolor"
        s.doc = "@xref{XREFlinemarkerfacecolor, , \
@w{line markerfacecolor property}}.";
        s.category = "Marker Appearance";

      case "markersize"
        s.doc = "@xref{XREFlinemarkersize, , \
@w{line markersize property}}.";
        s.valid = "scalar";
        s.category = "Marker Appearance";

      case "meshstyle"
        s.doc = "Specifies whether to display the edges associated with the \
        surface data's rows, columns, or both.";
        s.category = "Outline Appearance";

      case "specularcolorreflectance"
        s.doc = "Reflectance for specular color.  Value between 0.0 (color \
of underlying face) and 1.0 (color of light source).";
        s.valid = "scalar";
        s.category = "Lighting";

      case "specularexponent"
        s.doc = "Exponent for the specular reflection.  The lower the value, \
the more the reflection is spread out.";
        s.valid = "scalar";
        s.category = "Lighting";

      case "specularstrength"
        s.doc = "Strength of the specular reflection.  Value between 0.0 (no \
specular reflection) and 1.0 (full specular reflection).";
        s.valid = "scalar";
        s.category = "Lighting";

      case "vertexnormals"
        s.doc = "Vertex normals are used for lighting the edges or faces if \
the @code{edgelighting} or @code{facelighting} properties are set to \
@qcode{\"gouraud\"}.  __modemsg__";
        s.category = "Lighting";

      case "vertexnormalsmode"
        s.doc = "If this property is set to @qcode{\"auto\"}, \
@code{vertexnormals} are automatically calculated if the @code{edgelighting} \
or @code{facelighting} property are set to @qcode{\"gouraud\"} and at least \
one @code{light} object is present and visible in the same axes.";
        s.category = "Lighting";

      case "xdata"
        s.doc = "Data for the x-coordinate.";
        s.valid = "matrix";
        s.category = "Surface Data";

      case "xdatasource"
        s.doc = "The name of a workspace variable that contains data that \
will be used for the \
@ref{XREFsurfacexdata, , @w{@qcode{\"xdata\"} property}}.  Data is \
transferred into \"xdata\" using the \
@xref{XREFrefreshdata, , @w{refreshdata function}}.";
        s.valid = valid_string;
        s.category = "Surface Data";

      case "ydata"
        s.doc = "Data for the y-coordinate.";
        s.valid = "matrix";
        s.category = "Surface Data";

      case "ydatasource"
        s.doc = "The name of a workspace variable that contains data that \
will be used for the \
@ref{XREFsurfaceydata, , @w{@qcode{\"ydata\"} property}}.  Data is \
transferred into \"ydata\" using the \
@xref{XREFrefreshdata, , @w{refreshdata function}}.";
        s.valid = valid_string;
        s.category = "Surface Data";

      case "zdata"
        s.doc = "Data for the z-coordinate.";
        s.valid = "matrix";
        s.category = "Surface Data";

      case "zdatasource"
        s.doc = "The name of a workspace variable that contains data that \
will be used for the \
@ref{XREFsurfacezdata, , @w{@qcode{\"zdata\"} property}}.  Data is \
transferred into \"zdata\" using the \
@xref{XREFrefreshdata, , @w{refreshdata function}}.";
        s.valid = valid_string;
        s.category = "Surface Data";

    endswitch

  ## Patch properties
  elseif (strcmp (objname, "patch"))
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "alphadatamapping"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.category = "Patch Appearance";

      case "ambientstrength"
        s.doc = "Strength of the ambient light.  Value between 0.0 and 1.0.";
        s.valid = "scalar";
        s.category = "Lighting";

      case "backfacelighting"
        s.doc =  "@qcode{\"lit\"}: The normals are used as is for lighting.  \
@qcode{\"reverselit\"}: The normals are always oriented towards the point of \
view.  @qcode{\"unlit\"}: Faces with normals pointing away from the point of \
view are unlit.";
        s.category = "Lighting";

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
        s.category = "Patch Appearance";

      case "cdatamapping"
        s.category = "Patch Appearance";

      case "diffusestrength"
        s.doc = "Strength of the diffuse reflection.  Value between 0.0 (no \
diffuse reflection) and 1.0 (full diffuse reflection).";
        s.valid = "scalar";
        s.category = "Lighting";

      case "displayname"
        s.doc = "Text of the legend entry corresponding to this patch.";
        s.category = "Text Appearance";

      case "edgealpha"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = valid_scalmat;
        s.category = "Outline Appearance";

      case "edgecolor"
        s.category = "Outline Appearance";

      case "edgelighting"
        s.doc = "When set to a value other than @qcode{\"none\"}, the edges \
of the object are drawn with light and shadow effects.  Supported values are \
@qcode{\"none\"} (no lighting effects), @qcode{\"flat\"} (faceted look), and \
@qcode{\"gouraud\"} (linear interpolation of the lighting effects between \
the vertices).  @qcode{\"phong\"} is deprecated and has the same effect as \
@qcode{\"gouraud\"}.";
        s.category = "Outline Appearance";

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
        s.category = "Face Appearance";

      case "facecolor"
        ## Don't provide a default value, and mark colorspec with
        ## braces, this forces the default RGB triplet to be displayed
        s.valid = packopt ({markdef(valid_color), ...
                            "@qcode{\"none\"}", ...
                            "@qcode{\"flat\"}", ...
                            "@qcode{\"interp\"}"});
        s.category = "Face Appearance";

      case "facelighting"
        s.doc = "When set to a value other than @qcode{\"none\"}, the faces \
of the object are drawn with light and shadow effects.  Supported values are \
@qcode{\"none\"} (no lighting effects), @qcode{\"flat\"} (faceted look), and \
@qcode{\"gouraud\"} (linear interpolation of the lighting effects between \
the vertices).  @qcode{\"phong\"} is deprecated and has the same effect as \
@qcode{\"gouraud\"}.";
        s.category = "Face Appearance";

      case "facenormals"
        s.doc = "Face normals are used for lighting the edges or faces if the \
@code{edgelighting} or @code{facelighting} properties are set to \
@qcode{\"flat\"}.  __modemsg__";
        s.category = "Face Appearance";

      case "facenormalsmode"
        s.doc = "If this property is set to @qcode{\"auto\"}, \
@code{facenormals} are automatically calculated if the @code{edgelighting} or \
@code{facelighting} property are set to @qcode{\"flat\"} and at least one \
@code{light} object is present and visible in the same axes.";
        s.category = "Face Appearance";

      case "faces"
        s.valid = valid_vecmat;
        s.category = "Patch Data";

      case "facevertexalphadata"
        s.doc = sprintf (doc_notimpl, "Transparency");
        s.valid = valid_scalmat;
        s.category = "Patch Appearance";

      case "facevertexcdata"
        s.category = "Patch Data";

      case "linestyle"
        s.category = "Outline Appearance";

      case "linewidth"
        s.category = "Outline Appearance";

      case "marker"
        s.doc = "@xref{XREFlinemarker, , @w{line marker property}}.";
        s.category = "Marker Appearance";

      case "markeredgecolor"
        s.doc = "@xref{XREFlinemarkeredgecolor, , \
@w{line markeredgecolor property}}.";
        s.category = "Marker Appearance";

      case "markerfacecolor"
        s.doc = "@xref{XREFlinemarkerfacecolor, , \
@w{line markerfacecolor property}}.";
        s.category = "Marker Appearance";

      case "markersize"
        s.doc = "@xref{XREFlinemarkersize, , @w{line markersize property}}.";
        s.valid = "scalar";
        s.category = "Marker Appearance";

      case "specularcolorreflectance"
        s.doc = "Reflectance for specular color.  Value between 0.0 (color \
of underlying face) and 1.0 (color of light source).";
        s.valid = "scalar";
        s.category = "Lighting";

      case "specularexponent"
        s.doc = "Exponent for the specular reflection.  The lower the value, \
the more the reflection is spread out.";
        s.valid = "scalar";
        s.category = "Lighting";

      case "specularstrength"
        s.doc = "Strength of the specular reflection.  Value between 0.0 (no \
specular reflection) and 1.0 (full specular reflection).";
        s.valid = "scalar";
        s.category = "Lighting";

      case "vertexnormals"
        s.doc = "Vertex normals are used for lighting the edges or faces if \
the @code{edgelighting} or @code{facelighting} properties are set to \
@qcode{\"gouraud\"}.  __modemsg__";
        s.category = "Lighting";

      case "vertexnormalsmode"
        s.doc = "If this property is set to @qcode{\"auto\"}, \
@code{vertexnormals} are automatically calculated if the @code{edgelighting} \
or @code{facelighting} property are set to @qcode{\"gouraud\"} and at least \
one @code{light} object is present and visible in the same axes.";
        s.category = "Lighting";

      case "vertices"
        s.valid = valid_vecmat;
        s.category = "Patch Data";

      case "xdata"
        s.valid = valid_vecmat;
        s.category = "Patch Data";

      case "ydata"
        s.valid = valid_vecmat;
        s.category = "Patch Data";

      case "zdata"
        s.valid = valid_vecmat;
        s.category = "Patch Data";

    endswitch

  ## Scatter properties
  elseif (strcmp (objname, "scatter"))
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "annotation"
        s.category = "Text Appearance";

      case "cdatamode"
        s.doc = "If @code{cdatamode} is @qcode{\"auto\"}, @code{cdata} is set \
to the color from the @code{colororder} of the ancestor axes corresponding to \
the @code{seriesindex}.";
        s.category = "Scatter Color Data";

      case "cdatasource"
        s.doc = sprintf (doc_notimpl, "Data from workspace variables");
        s.category = "Scatter Color Data";

      case "cdata"
        s.doc = "Data defining the scatter object color.\n\
\n\
If @code{cdata} is a scalar index into the current colormap or a RGB triplet, \
it defines the color of all scatter markers.\n\
\n\
If @code{cdata} is an N-by-1 vector of indices or an N-by-3 (RGB) matrix, \
it defines the color of each one of the N scatter markers.";
        s.valid = valid_scalmat;
        s.category = "Scatter Color Data";

      case "datatiptemplate"
        s.category = "Mouse Interaction";

      case "displayname"
        s.doc = "Text of the legend entry corresponding to this scatter \
object.";
        s.category = "Text Appearance";

      case "latitudedata"
        s.category = "Scatter Plot Data";

      case "latitudedatasource"
        s.category = "Scatter Plot Data";

      case "linewidth"
        s.doc = "Line width of the edge of the markers.";
        s.category = "Marker Appearance";

      case "longitudedata"
        s.category = "Scatter Plot Data";

      case "longitudedatasource"
        s.category = "Scatter Plot Data";

      case "marker"
        s.doc = "@xref{XREFlinemarker, , @w{line marker property}}.";
        s.category = "Marker Appearance";

      case "markeredgealpha"
        s.doc = "Transparency level of the faces of the markers where a \
value of 0 means complete transparency and a value of 1 means solid faces \
without transparency.  Note that the markers are not sorted from back to \
front which might lead to unexpected results when rendering layered \
transparent markers or in combination with other transparent objects.";
        s.valid = "scalar";
        s.category = "Marker Appearance";

      case "markeredgecolor"
        s.doc = "Color of the edge of the markers.  @qcode{\"none\"} means \
that the edges are transparent and @qcode{\"flat\"} means that the value \
from @code{cdata} is used.  @xref{XREFlinemarkeredgecolor, , \
@w{line markeredgecolor property}}.";
        s.valid = packopt ({markdef("@qcode{\"none\"}"), ...
                            "@qcode{\"flat\"}", ...
                            valid_color});
        s.category = "Marker Appearance";

      case "markerfacealpha"
        s.doc = "Transparency level of the faces of the markers where a \
value of 0 means complete transparency and a value of 1 means solid faces \
without transparency.  Note that the markers are not sorted from back to \
front which might lead to unexpected results when rendering layered \
transparent markers or in combination with other transparent objects.";
        s.valid = "scalar";
        s.category = "Marker Appearance";

      case "markerfacecolor"
        s.doc = "Color of the face of the markers.  @qcode{\"none\"} means \
that the faces are transparent, @qcode{\"flat\"} means that the value from \
@code{cdata} is used, and @qcode{\"auto\"} uses the @code{color} property of \
the ancestor axes.  @xref{XREFlinemarkerfacecolor, , \
@w{line markerfacecolor property}}.";
        s.valid = packopt ({markdef("@qcode{\"none\"}"), ...
                            "@qcode{\"flat\"}", ...
                            "@qcode{\"auto\"}", ...
                            valid_color});
        s.category = "Marker Appearance";

      case "rdata"
        s.category = "Scatter Plot Data";

      case "rdatasource"
        s.category = "Scatter Plot Data";

      case "seriesindex"
        s.doc = "Each scatter object in the same axes is assigned an \
incrementing integer.  This corresponds to the index into the \
@code{colororder} of the ancestor axes that is used if @code{cdatamode} is \
set to @qcode{\"auto\"}.";
        s.category = "Scatter Color Data";

      case "sizedata"
        s.doc = "Size of the area of the marker.  A scalar value applies to \
all markers.  If @code{cdata} is an N-by-1 vector, it defines the color of \
each one of the N scatter markers.";
        s.valid =  packopt ({"[]", "scalar", "vector"});
        s.category = "Marker Appearance";

      case "sizedatasource"
        s.doc = sprintf (doc_notimpl, "Data from workspace variables");
        s.category = "Marker Appearance";

      case "thetadata"
        s.category = "Scatter Plot Data";

      case "thetadatasource"
        s.category = "Scatter Plot Data";

      case "xdata"
        s.doc = "Vector with the x coordinates of the scatter object.";
        s.valid = "vector";
        s.category = "Scatter Plot Data";

        case "xdatasource"
        s.doc = sprintf (doc_notimpl, "Data from workspace variables");
        s.category = "Scatter Plot Data";

      case "ydata"
        s.doc = "Vector with the y coordinates of the scatter object.";
        s.valid = "vector";
        s.category = "Scatter Plot Data";

      case "ydatasource"
        s.doc = sprintf (doc_notimpl, "Data from workspace variables");
        s.category = "Scatter Plot Data";

      case "zdata"
        s.doc = "For 3D data, vector with the y coordinates of the scatter \
object.";
        s.valid = packopt ({"[]", "vector"});
        s.category = "Scatter Plot Data";

      case "zdatasource"
        s.doc = sprintf (doc_notimpl, "Data from workspace variables");
        s.category = "Scatter Plot Data";

    endswitch

  ## Light properties
  elseif (strcmp (objname, "light"))
    switch (field)
      ## Overridden shared properties
      case "children"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "color"
        s.doc = "Color of the light source.  @xref{Colors, ,colorspec}.";
        s.valid = valid_color;
        s.category = "Lighting";

      case "position"
        s.doc = "Position of the light source.";
        s.category = "Lighting";

      case "style"
        s.doc = "This string defines whether the light emanates from a \
light source at infinite distance (@qcode{\"infinite\"}) or from a local \
point source (@qcode{\"local\"}).";
        s.category = "Lighting";

    endswitch

  ## uimenu properties
  elseif (strcmp (objname, "uimenu"))
    switch (field)
      ## Overridden shared properties
      case "buttondownfcn"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "accelerator"
        s.category = "Keyboard Interaction";

      case "callback"
        s.category = "Callback Execution";

      case "checked"
        s.category = "Menu Options";

      case "enable"
        s.category = "Menu Options";

      case "foregroundcolor"
        s.category = "Appearance";

      case "menuselectedfcn"
        s.category = "Callback Execution";

      case "position"
        s.category = "Position";

      case "separator"
        s.category = "Appearance";

      case "text"
        s.category = "Menu Options";

    endswitch

  ## uicontextmenu properties
  elseif (strcmp (objname, "uicontextmenu"))
    switch (field)
      ## Overridden shared properties
      case "buttondownfcn"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "callback"
        s.category = "Callback Execution";

      case "position"
        s.category = "Position";

    endswitch

  ## uipanel properties
  elseif (strcmp (objname, "uipanel"))
    switch (field)
      ## Overridden shared properties

      ## Specific properties
      case "backgroundcolor"
        s.category = "Appearance";

      case "bordertype"
        s.category = "Appearance";

      case "borderwidth"
        s.category = "Appearance";

      case "fontangle"
        s.doc = doc_fontangle;
        s.category = "Annotation";

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;
        s.category = "Annotation";

      case "fontsize"
        s.doc = doc_fontsize;
        s.valid = "scalar";
        s.category = "Annotation";

      case "fontunits"
        s.doc = doc_fontunits;
        s.category = "Annotation";

      case "fontweight"
        s.doc = doc_fontweight;
        s.category = "Annotation";

      case "foregroundcolor"
        s.category = "Appearance";

      case "highlightcolor"
        s.category = "Appearance";

      case "position"
        s.category = "Position";

      case "resizefcn"
        s.doc = "__prop__ is deprecated.  Use @code{sizechangedfcn} instead.";
        s.valid = valid_fcn;
        s.category = "Callback Execution";

      case "shadowcolor"
        s.category = "Appearance";

      case "sizechangedfcn"
        s.doc = "Callback triggered when the uipanel size is changed.\
\n\n__fcnmsg__";
        s.valid = valid_fcn;
        s.category = "Callback Execution";

      case "title"
        s.category = "Annotation";

      case "titleposition"
        s.category = "Annotation";

      case "units"
        s.category = "Position";

    endswitch

  ## uibuttongroup properties
  elseif (strcmp (objname, "uibuttongroup"))
    switch (field)
      ## Overridden shared properties

      ## Specific properties
      case "backgroundcolor"
        s.category = "Appearance";

      case "bordertype"
        s.category = "Appearance";

      case "borderwidth"
        s.category = "Appearance";

      case "fontangle"
        s.doc = doc_fontangle;
        s.category = "Annotation";

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;
        s.category = "Annotation";

      case "fontsize"
        s.doc = doc_fontsize;
        s.valid = "scalar";
        s.category = "Annotation";

      case "fontunits"
        s.doc = doc_fontunits;
        s.category = "Annotation";

      case "fontweight"
        s.doc = doc_fontweight;
        s.category = "Annotation";

      case "foregroundcolor"
        s.category = "Appearance";

      case "highlightcolor"
        s.category = "Appearance";

      case "position"
        s.category = "Position";

      case "resizefcn"
        s.doc = "__prop__ is deprecated.  Use @code{sizechangedfcn} instead.";
        s.valid = valid_fcn;
        s.category = "Callback Execution";

      case "selectedobject"
        s.category = "Button Group Operation";

      case "selectionchangedfcn"
        s.category = "Callback Execution";

      case "shadowcolor"
        s.category = "Appearance";

      case "sizechangedfcn"
        s.doc = "Callback triggered when the uibuttongroup size is changed.\
\n\n__fcnmsg__";
        s.valid = valid_fcn;
        s.category = "Appearance";

      case "title"
        s.category = "Annotation";

      case "titleposition"
        s.category = "Annotation";

      case "units"
        s.category = "Position";

    endswitch

  ## uicontrol properties
  elseif (strcmp (objname, "uicontrol"))
    switch (field)
      ## Overridden shared properties

      ## Specific properties
      case "backgroundcolor"
        s.category = "Appearance";

      case "callback"
        s.category = "Callback Execution";

      case "cdata"
        s.category = "Appearance";

      case "enable"
        s.category = "Control Options";

      case "extent"
        s.doc = "Size of the text string associated to the uicontrol \
 returned in the form @code{[0 0 width height]} (the two first elements \
are always zero).\n\n\
For multi-line strings the returned @code{width} and @code{height} \
indicate the size of the rectangle enclosing all lines.";
        s.valid = valid_4elvec;
        s.printdefault = false;
        s.category = "Appearance";

      case "fontangle"
        s.doc = doc_fontangle;
        s.category = "Annotation";

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;
        s.category = "Annotation";

      case "fontsize"
        s.doc = doc_fontsize;
        s.valid = "scalar";
        s.category = "Annotation";

      case "fontunits"
        s.doc = doc_fontunits;
        s.category = "Annotation";

      case "fontweight"
        s.doc = doc_fontweight;
        s.category = "Annotation";

      case "foregroundcolor"
        s.category = "Appearance";

      case "horizontalalignment"
        s.category = "Annotation";

      case "keypressfcn"
        s.category = "Callback Execution";

      case "listboxtop"
        s.category = "Control Options";

      case "max"
        s.category = "Control Options";

      case "min"
        s.category = "Control Options";

      case "position"
        s.category = "Position";

      case "sliderstep"
        s.category = "Control Options";

      case "string"
        s.category = "Annotation";

      case "style"
        s.category = "Appearance";

      case "tooltipstring"
        s.category = "Mouse Interaction";

      case "units"
        s.category = "Position";

      case "value"
        s.category = "Control Options";

      case "verticalalignment"
        s.category = "Annotation";

    endswitch

  ## uitable Properties
  elseif (strcmp (objname, "uitable"))
    switch (field)
      ## Overridden shared properties

      ## Specific properties
      case "backgroundcolor"
        s.category = "Appearance";

      case "celleditcallback"
        s.category = "Callback Execution";

      case "cellselectioncallback"
        s.category = "Callback Execution";

      case "columneditable"
        s.category = "Table Operation";

      case "columnformat"
        s.category = "Table Data";

      case "columnname"
        s.category = "Table Data";

        case "columnwidth"
        s.category = "Table Data";

      case "data"
        s.category = "Table Data";

      case "enable"
        s.category = "Table Operation";

      case "extent"
        s.valid = valid_4elvec;
        s.printdefault = false;
        s.category = "Position";

      case "fontangle"
        s.doc = doc_fontangle;
        s.category = "Annotation";

      case "fontname"
        s.doc = doc_fontname;
        s.valid = valid_string;
        s.category = "Annotation";

      case "fontsize"
        s.doc = doc_fontsize;
        s.valid = "scalar";
        s.category = "Annotation";

      case "fontunits"
        s.doc = doc_fontunits;
        s.category = "Annotation";

      case "fontweight"
        s.doc = doc_fontweight;
        s.category = "Annotation";

      case "foregroundcolor"
        s.category = "Appearance";

      case "keypressfcn"
        s.category = "Callback Execution";

      case "keyreleasefcn"
        s.category = "Callback Execution";

      case "position"
        s.category = "Position";

      case "rearrangeablecolumns"
        s.category = "Table Operation";

      case "rowname"
        s.category = "Annotation";

      case "rowstriping"
        s.category = "Appearance";

      case "tooltipstring"
        s.category = "Mouse Interaction";

      case "units"
        s.category = "Position";

    endswitch

  ## uitoolbar properties
  elseif (strcmp (objname, "uitoolbar"))
    switch (field)
      ## Overridden shared properties
      case "buttondownfcn"
        s.doc = doc_unused;
        ## Use base category.

    endswitch

  ## uipushtool properties
  elseif (strcmp (objname, "uipushtool"))
    switch (field)
      ## Overridden shared properties
      case "buttondownfcn"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case "__named_icon__"
        s.category = "Appearance";

      case "cdata"
        s.category = "Appearance";

      case "clickedcallback"
        s.category = "Callback Execution";

      case "enable"
        s.category = "Operation";

      case "separator"
        s.category = "Appearance";

      case "tooltipstring"
        s.category = "Annotation";

    endswitch

  ## uitoggletool properties
  elseif (strcmp (objname, "uitoggletool"))
    switch (field)
      ## Overridden shared properties
      case "buttondownfcn"
        s.doc = doc_unused;
        ## Use base category.

      ## Specific properties
      case ""
        s.category = "Appearance";

      case "cdata"
        s.category = "Appearance";

      case "clickedcallback"
        s.category = "Callback Execution";

      case "enable"
        s.category = "Toggle Operation";

      case "offcallback"
        s.category = "Callback Execution";

      case "oncallback"
        s.category = "Callback Execution";

      case "separator"
        s.category = "Appearance";

      case "state"
        s.category = "Toggle Operation";

      case "tooltipstring"
        s.category = "Annotation";

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
property to be set to @qcode{\"manual\"}.";
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

  ## Sort fields by category, put the special categories "Miscellaneous" and
  ## "Unused" last if they are used.
  allfields = fieldnames (obj);
  allcategories = cellfun (@(s) obj.(s).category, allfields, "uni", false);
  categories = unique (allcategories);
  idx = strcmp (categories, "Miscellaneous");
  categories = [categories(!idx); categories(idx)];
  idx = strcmp (categories, "Unused");
  categories = [categories(!idx); categories(idx)];

  idx = [];
  for ii = 1:numel (categories)
    fields = sort (allfields(strcmp (allcategories, categories{ii})));
    nf = numel (fields);
    str = sprintf ("%s\n\n@subsubheading %s", str, categories{ii});
    str = sprintf ("%s\n\n@table @asis", str);

    for jj = 1:nf
      field = fields{jj};
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
  endfor

endfunction

function str = warn_autogen ()
  str = "@c DO NOT EDIT!  Generated automatically by genpropdoc.m.\n\
\n\
@c Copyright (C) 2014-2024 The Octave Project Developers\n\
@c\n\
@c See the file COPYRIGHT.md in the top-level directory of this\n\
@c distribution or <https://octave.org/copyright/>.\n\
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
