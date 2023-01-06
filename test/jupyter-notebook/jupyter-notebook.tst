########################################################################
##
## Copyright (C) 2021-2023 The Octave Project Developers
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

## Test running a single cell
%!testif HAVE_RAPIDJSON
%! visibility = get (0, "defaultfigurevisible");
%! toolkit = graphics_toolkit ();
%! unwind_protect
%!   if (! __have_feature__ ("QT_OFFSCREEN")
%!       || ! strcmp (graphics_toolkit (), "qt"))
%!     try
%!       graphics_toolkit ("gnuplot");
%!     catch
%!       ## The system doesn't support gnuplot for drawing hidden
%!       ## figures.  Just return and have test marked as passing.
%!       return;
%!     end_try_catch
%!   endif
%!   set (0, "defaultfigurevisible", "off");
%!
%!   n = jupyter_notebook (fullfile ("octave_kernel.ipynb"));
%!
%!   ## Test embedding images
%!   n.run (2);
%!   assert (n.notebook.cells{2}.outputs{1}.output_type, "display_data")
%!   assert (isfield (n.notebook.cells{2}.outputs{1}.data, "image/png"));
%!   assert (getfield (n.notebook.cells{2}.outputs{1}.data, "text/plain"),
%!           {"<IPython.core.display.Image object>"});
%!
%!   ## Test running non-code cells
%!   markdown_cell = n.notebook.cells{1};
%!   n.run (1);
%!   assert (markdown_cell, n.notebook.cells{1});
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

## Test running all cells
%!testif HAVE_RAPIDJSON
%! visibility = get (0, "defaultfigurevisible");
%! toolkit = graphics_toolkit ();
%! unwind_protect
%!   if (! __have_feature__ ("QT_OFFSCREEN")
%!       || ! strcmp (graphics_toolkit (), "qt"))
%!     try
%!       graphics_toolkit ("gnuplot");
%!     catch
%!       ## The system doesn't support gnuplot for drawing hidden
%!       ## figures.  Just return and have test marked as passing.
%!       return;
%!     end_try_catch
%!   endif
%!   set (0, "defaultfigurevisible", "off");
%!
%!   n = jupyter_notebook (fullfile ("octave_kernel.ipynb"));
%!   n.run_all ();
%!
%!   ## Test embedding images
%!   assert (n.notebook.cells{3}.outputs{1}.output_type, "display_data")
%!   assert (isfield (n.notebook.cells{3}.outputs{1}.data, "image/png"));
%!   assert (getfield (n.notebook.cells{3}.outputs{1}.data, "text/plain"),
%!           {"<IPython.core.display.Image object>"});
%!
%!   ## Test running non-code cells
%!   markdown_cell = n.notebook.cells{1};
%!   n.run (1);
%!   assert (markdown_cell, n.notebook.cells{1});
%!
%!   ## Test embedding textual output
%!   assert (n.notebook.cells{6}.outputs{1}.output_type, "stream")
%!   assert (n.notebook.cells{6}.outputs{1}.name, "stdout");
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

## Test plot magic
%!testif HAVE_RAPIDJSON
%! visibility = get (0, "defaultfigurevisible");
%! toolkit = graphics_toolkit ();
%! unwind_protect
%!   if (! __have_feature__ ("QT_OFFSCREEN")
%!       || ! strcmp (graphics_toolkit (), "qt"))
%!     try
%!       graphics_toolkit ("gnuplot");
%!     catch
%!       ## The system doesn't support gnuplot for drawing hidden
%!       ## figures.  Just return and have test marked as passing.
%!       return;
%!     end_try_catch
%!   endif
%!   set (0, "defaultfigurevisible", "off");
%!
%!   n = jupyter_notebook (fullfile ("plot_magic_and_errors.ipynb"));
%!
%!   ## PNG format
%!   n.run (1);
%!   assert (n.notebook.cells{1}.outputs{1}.output_type, "display_data")
%!   assert (isfield (n.notebook.cells{1}.outputs{1}.data, "image/png"));
%!   assert (getfield (n.notebook.cells{1}.outputs{1}.data, "text/plain"),
%!           {"<IPython.core.display.Image object>"});
%!
%!   ## SVG format
%!   n.run (2);
%!   assert (n.notebook.cells{2}.outputs{1}.output_type, "display_data")
%!   assert (isfield (n.notebook.cells{2}.outputs{1}.data, "image/svg+xml"));
%!   assert (getfield (n.notebook.cells{2}.outputs{1}.data, "text/plain"),
%!           {"<IPython.core.display.SVG object>"});
%!
%!   ## JPG format
%!   n.run (3);
%!   assert (n.notebook.cells{3}.outputs{1}.output_type, "display_data")
%!   assert (isfield (n.notebook.cells{3}.outputs{1}.data, "image/jpeg"));
%!   assert (getfield (n.notebook.cells{3}.outputs{1}.data, "text/plain"),
%!           {"<IPython.core.display.Image object>"});
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

## Test errors
%!testif HAVE_RAPIDJSON
%! visibility = get (0, "defaultfigurevisible");
%! toolkit = graphics_toolkit ();
%! unwind_protect
%!   if (! __have_feature__ ("QT_OFFSCREEN")
%!       || ! strcmp (graphics_toolkit (), "qt"))
%!     try
%!       graphics_toolkit ("gnuplot");
%!     catch
%!       ## The system doesn't support gnuplot for drawing hidden
%!       ## figures.  Just return and have test marked as passing.
%!       return;
%!     end_try_catch
%!   endif
%!   set (0, "defaultfigurevisible", "off");
%!
%!   n = jupyter_notebook (fullfile ("plot_magic_and_errors.ipynb"));
%!
%!   ## Wrong resolution
%!   n.run (4);
%!   assert (n.notebook.cells{4}.outputs{1}.output_type, "stream")
%!   assert (n.notebook.cells{4}.outputs{1}.name, "stderr");
%!   assert (n.notebook.cells{4}.outputs{1}.text,
%!           {"A number is required for resolution, not a string"});
%!
%!   ## Wrong width
%!   n.run (5);
%!   assert (n.notebook.cells{5}.outputs{1}.output_type, "stream")
%!   assert (n.notebook.cells{5}.outputs{1}.name, "stderr");
%!   assert (n.notebook.cells{5}.outputs{1}.text,
%!           {"A number is required for width, not a string"});
%!
%!   ## Wrong height
%!   n.run (6);
%!   assert (n.notebook.cells{6}.outputs{1}.output_type, "stream")
%!   assert (n.notebook.cells{6}.outputs{1}.name, "stderr");
%!   assert (n.notebook.cells{6}.outputs{1}.text,
%!           {"A number is required for height, not a string"});
%!
%!   ## Empty figure
%!   n.run (7);
%!   assert (n.notebook.cells{7}.outputs{1}.output_type, "stream")
%!   assert (n.notebook.cells{7}.outputs{1}.name, "stderr");
%!   assert (n.notebook.cells{7}.outputs{1}.text,
%!           {"The figure is empty!"});
%!
%!   ## Wrong format
%!   n.run (8);
%!   assert (n.notebook.cells{8}.outputs{1}.output_type, "stream")
%!   assert (n.notebook.cells{8}.outputs{1}.name, "stderr");
%!   assert (n.notebook.cells{8}.outputs{1}.text,
%!           {"Cannot embed the 'pdf' image format\n"});
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect
