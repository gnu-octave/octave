########################################################################
##
## Copyright (C) 2021-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
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

classdef jupyter_notebook < handle

  ## -*- texinfo -*-
  ## @deftypefn  {} {@var{notebook} =} jupyter_notebook (@var{notebook_filename})
  ## @deftypefnx {} {@var{notebook} =} jupyter_notebook (@var{notebook_filename}, @var{options})
  ##
  ## Run and fill the Jupyter Notebook in file @var{notebook_filename} from
  ## within GNU Octave.
  ##
  ## Both text and graphical Octave outputs are supported.
  ##
  ## This class has a public property @code{notebook} which is a structure
  ## representing the JSON-decoded Jupyter Notebook.  This property is
  ## intentionally public to enable advanced notebook manipulations.
  ##
  ## Note: Jupyter Notebook versions (@code{nbformat}) lower than 4.0 are not
  ## supported.
  ##
  ## The optional second argument @var{options} is a struct with fields:
  ##
  ## @itemize @bullet
  ## @item
  ## @code{tmpdir} to set the temporary working directory.
  ## @end itemize
  ##
  ## @code{%plot} magic is supported with the following settings:
  ##
  ## @itemize @bullet
  ## @item
  ## "%plot -f <format>" or "%plot --format <format>": specifies the
  ## image storage format.  Supported formats are:
  ##
  ## @itemize @minus
  ## @item
  ## PNG (default)
  ##
  ## @item SVG
  ## (Note: If SVG images do not appear in the notebook, it is most likely
  ## related to Jupyter Notebook security mechanisms and explicitly "trusting"
  ## them will be necessary).
  ##
  ## @item
  ## JPG
  ## @end itemize
  ##
  ## @item
  ## "%plot -r <number>" or "%plot --resolution <number>": specifies the
  ## image resolution.
  ##
  ## @item
  ## "%plot -w <number>" or "%plot --width <number>": specifies the
  ## image width.
  ##
  ## @item
  ## "%plot -h <number>" or "%plot --height <number>": specifies the
  ## image height.
  ## @end itemize
  ##
  ## Examples:
  ##
  ## @example
  ## @group
  ## ## Run all cells and generate the filled notebook
  ##
  ## ## Instantiate an object from a notebook file
  ## notebook = jupyter_notebook ("myNotebook.ipynb");
  ## ## Run the code and embed the results in the @code{notebook} property
  ## notebook.run_all ();
  ## ## Generate a new notebook by overwriting the original notebook
  ## notebook.generate_notebook ("myNotebook.ipynb");
  ## @end group
  ##
  ## @group
  ## ## Run just the second cell and generate the filled notebook
  ##
  ## ## Instantiate an object from a notebook file
  ## notebook = jupyter_notebook ("myNotebook.ipynb");
  ## ## Run the code and embed the results in the @code{notebook} property
  ## notebook.run (2)
  ## ## Generate a new notebook in a new file
  ## notebook.generate_notebook ("myNewNotebook.ipynb");
  ## @end group
  ##
  ## @group
  ## ## Generate an Octave script from a notebook
  ##
  ## ## Instantiate an object from a notebook file
  ## notebook = jupyter_notebook ("myNotebook.ipynb");
  ## ## Generate the Octave script
  ## notebook.generate_octave_script ("jup_script.m");
  ## @end group
  ## @end example
  ##
  ## @seealso{jsondecode, jsonencode}
  ## @end deftypefn

  properties

    notebook = struct ();

  endproperties

  properties (Access = "private")

    context = struct ("ans", "");

    ## Note: This name needs to be stored in a property because it is
    ## set in the constructor but used in some other methods.  However,
    ## we want to defer calling tempname() until immediately before
    ## calling mkdir().  The temporary directory currently created and
    ## deleted in the constructor and the name is reset to the empty
    ## string when the directory is deleted.  Another possible
    ## implementation might be to generate the name and create the
    ## temporary directory here, then delete it in the class destructor.

    tmpdir = "";

  endproperties

  methods

    function obj = jupyter_notebook (notebook_filename, options)

      if (nargin < 1)
        print_usage ();
      endif

      if (! (ischar (notebook_filename) && isrow (notebook_filename)))
        error ("jupyter_notebook: NOTEBOOK_FILENAME must be a string");
      endif

      ## Validate options if present.
      if (nargin == 2 && ! isstruct (options))
        error ("jupyter_notebook: OPTIONS must be a struct");
      endif
      if (nargin == 2 && isfield (options, "tmpdir"))
        obj.tmpdir = options.tmpdir;
      endif

      obj.notebook = jsondecode (fileread (notebook_filename),
                                 "makeValidName", false);

      ## Validate the notebook's format according to nbformat: 4.0
      if (! all (isfield (obj.notebook,
                          {"metadata", "nbformat", "nbformat_minor", "cells"})))
        error ("jupyter_notebook: invalid format for Jupyter notebooks");
      endif

      ## Issue a warning if the format is lower than 4.0.
      if (obj.notebook.nbformat < 4)
        warning (["jupyter_notebook: nbformat versions lower than 4.0 are ", ...
                  "not supported"]);
      endif

      ## Handle the case of only one cell.
      ## Make "obj.notebook.cells" a cell of structs to match the format.
      if (numel (obj.notebook.cells) == 1)
        obj.notebook.cells = {obj.notebook.cells};
      endif

      ## Handle the case where the cells have the same keys.
      ## Make "obj.notebook.cells" a cell of structs, instead of struct array,
      ## to unify the indexing method.
      if (isstruct (obj.notebook.cells))
        obj.notebook.cells = num2cell (obj.notebook.cells);
      endif

      for i = 1:numel (obj.notebook.cells)
        nbcell = obj.notebook.cells{i};
        if (! isfield (nbcell, "source"))
          error ('jupyter_notebook: cells must contain a "source" field');
        endif

        if (! isfield (nbcell, "cell_type"))
          error ('jupyter_notebook: cells must contain a "cell_type" field');
        endif

        ## Handle null JSON values which are decoded into empty arrays.
        if (isfield (nbcell, "execution_count")
            && numel (nbcell.execution_count) == 0)
          obj.notebook.cells{i}.execution_count = 1;
        endif

        ## Handle the case of only one output in the cell.
        ## Make the outputs of the cell a cell of structs to match the format.
        if (isfield (nbcell, "outputs") && numel (nbcell.outputs) == 1)
          obj.notebook.cells{i}.outputs = {obj.notebook.cells{i}.outputs};
        endif
      endfor

    endfunction


    function generate_octave_script (obj, script_filename)

      ## -*- texinfo -*-
      ## @deftypefn {} {} generate_octave_script (@var{script_filename})
      ##
      ## Write an Octave script that has the contents of the Jupyter Notebook
      ## stored in the @code{notebook} attribute to @var{script_filename}.
      ##
      ## Non-code cells are generated as block comments.
      ##
      ## See @code{help jupyter_notebook} for examples.
      ##
      ## @seealso{jupyter_notebook}
      ## @end deftypefn

      if (nargin != 2)
        print_usage ();
      endif

      if (! (ischar (script_filename) && isrow (script_filename)))
        error ("jupyter_notebook: SCRIPT_FILENAME must be a string");
      endif

      fid = fopen (script_filename, "w");

      for i = 1:numel (obj.notebook.cells)
        nbcell = obj.notebook.cells{i};
        is_markdown = strcmp (nbcell.cell_type, "markdown");

        if (is_markdown)
          fputs (fid, "\n#{\n");
        endif

        for k = 1:numel (nbcell.source)
          fputs (fid, nbcell.source{k});
        endfor

        if (is_markdown)
          fputs (fid, "\n#}\n");
        endif
        fputs (fid, "\n");
      endfor
      fclose (fid);

    endfunction


    function generate_notebook (obj, notebook_filename)

      ## -*- texinfo -*-
      ## @deftypefn {} {} generate_notebook (@var{notebook_filename})
      ##
      ## Write the Jupyter Notebook stored in the @code{notebook}
      ## attribute to @var{notebook_filename}.
      ##
      ## The @code{notebook} attribute is encoded to JSON text.
      ##
      ## See @code{help jupyter_notebook} for examples.
      ##
      ## @seealso{jupyter_notebook}
      ## @end deftypefn

      if (nargin != 2)
        print_usage ();
      endif

      if (! (ischar (notebook_filename) && isrow (notebook_filename)))
        error ("jupyter_notebook: NOTEBOOK_FILENAME must be a string");
      endif

      fid = fopen (notebook_filename, "w");

      fputs (fid, jsonencode (obj.notebook,
                              "ConvertInfAndNaN", false,
                              "PrettyPrint", true));

      fclose (fid);

    endfunction


    function run (obj, cell_index)

      ## -*- texinfo -*-
      ## @deftypefn {} {} run (@var{cell_index})
      ##
      ## Run the Jupyter Notebook cell with index @var{cell_index}
      ## and eventually replace previous output cells in the object.
      ##
      ## The first Jupyter Notebook cell has the index 1.
      ##
      ## Note: The code evaluation of the Jupyter Notebook cells is done
      ## in a separate Jupyter Notebook context.  Thus, currently open
      ## figures and workspace variables won't be affected by executing
      ## this function.  However, current workspace variables cannot be
      ## accessed either.
      ##
      ## See @code{help jupyter_notebook} for examples.
      ##
      ## @seealso{jupyter_notebook}
      ## @end deftypefn

      if (nargin != 2)
        print_usage ();
      endif

      if (! (isscalar (cell_index) && isindex (cell_index)))
        error ("jupyter_notebook: CELL_INDEX must be a scalar positive integer");
      endif

      if (cell_index > numel (obj.notebook.cells))
        error ("jupyter_notebook: CELL_INDEX is out of bound");
      endif

      nbcell = obj.notebook.cells{cell_index};

      if (! strcmp (nbcell.cell_type, "code"))
        return;
      endif

      ## Remove previous outputs.
      obj.notebook.cells{cell_index}.outputs = {};

      if (isempty (nbcell.source))
        return;
      endif

      ## Default values for printOptions.
      printOptions.imageFormat = "png";
      printOptions.resolution = "0";

      ## The default width and height in Jupyter notebook
      printOptions.width = "640";
      printOptions.height = "480";

      ## Parse "plot magic" commands.
      ## https://github.com/Calysto/metakernel/blob/master/metakernel/magics/README.md#plot
      for j = 1 : numel (nbcell.source)
        if (strncmpi (nbcell.source{j}, "%plot", 5))
          magics = strsplit (strtrim (nbcell.source{j}));
          for i = 1 : numel (magics)
            if (any (strcmp (magics{i}, {"-f", "--format"}))
                && (i < numel (magics)))
              printOptions.imageFormat = magics{i+1};
            endif
            if (any (strcmp (magics{i}, {"-r", "--resolution"}))
                && (i < numel (magics)))
              printOptions.resolution = magics{i+1};
            endif
            if (any (strcmp (magics{i}, {"-w", "--width"}))
                && (i < numel (magics)))
              printOptions.width = magics{i+1};
            endif
            if (any (strcmp (magics{i}, {"-h", "--height"}))
                && (i < numel (magics)))
              printOptions.height = magics{i+1};
            endif
          endfor
        endif
      endfor

      ## Remember previously opened figures.
      fig_ids = findall (groot, "type", "figure");

      ## Create a new figure, if there are existing plots.
      if (! isempty (fig_ids))
        newFig = figure ();
      endif

      stream_output = struct ("name", "stdout", "output_type", "stream");

      output_lines = obj.evalCode (strjoin (nbcell.source));

      if (! isempty (output_lines))
        stream_output.text = {output_lines};
      endif

      if (isfield (stream_output, "text"))
        obj.notebook.cells{cell_index}.outputs{end+1} = stream_output;
      endif

      ## If there are existing plots and newFig is empty, delete it.
      if (exist ("newFig") && isempty (get (newFig, "children")))
        delete (newFig);
      endif

      ## Check for newly created figures.
      fig_ids_new = setdiff (findall (groot, "type", "figure"), fig_ids);

      if (! isempty (fig_ids_new))
        if (! isempty (obj.tmpdir) && exist (obj.tmpdir, "dir"))
          ## Delete open figures before raising the error.
          delete (fig_ids_new);
          error (["JupyterNotebook: temporary directory %s exists.  ", ...
                  "Please remove it manually."], obj.tmpdir);
        endif

        if (isempty (obj.tmpdir))
          obj.tmpdir = tempname ();
          clear_tmpdir_property = true;
        else
          clear_tmpdir_property = false;
        endif
        [status, msg] = mkdir (obj.tmpdir);
        if (status == 0)
          ## Delete open figures before raising the error.
          delete (fig_ids_new);
          error (["jupyter_notebook: cannot create a temporary directory. ", ...
                  msg]);
        endif

        ## FIXME: Maybe it would be better for these cleanup actions to
        ## happen in an onCleanup object or unwind_protect block so that
        ## they will be executed no matter how we exit this function?

        for i = 1:numel (fig_ids_new)
          figure (fig_ids_new(i), "visible", "off");
          obj.embedImage (cell_index, fig_ids_new(i), printOptions);
          delete (fig_ids_new(i));
        endfor

        [status, msg] = rmdir (obj.tmpdir);
        if (status == 0)
          error (["jupyter_notebook: cannot delete the temporary ", ...
                  "directory. ", msg]);
        endif
        if (clear_tmpdir_property)
          obj.tmpdir = "";
        endif
      endif

    endfunction


    function run_all (obj)

      ## -*- texinfo -*-
      ## @deftypefn {} {} run_all ()
      ##
      ## Run all Jupyter Notebook cells and eventually replace previous
      ## output cells in the object.
      ##
      ## Note: The code evaluation of the Jupyter Notebook cells is done
      ## in a separate Jupyter Notebook context.  Thus, currently open
      ## figures and workspace variables won't be affected by executing
      ## this function.  However, current workspace variables cannot be
      ## accessed either.
      ##
      ## See @code{help jupyter_notebook} for examples.
      ##
      ## @seealso{jupyter_notebook}
      ## @end deftypefn

      if (nargin != 1)
        print_usage ();
      endif

      for i = 1:numel (obj.notebook.cells)
        obj.run (i);
      endfor

    endfunction

  endmethods


  methods (Access = "private")

    function retval = evalCode (__obj__, __code__)

      ## Evaluate the code string "__code__" using "evalc".
      ## Before the code is evaluated, the previous notebook context is
      ## loaded from "__obj__" and the new context is saved to that struct.

      if (nargin != 2)
        print_usage ();
      endif

      if (isempty (__code__))
        retval = [];
        return;
      endif

      if (! (ischar (__code__) && isrow (__code__)))
        error ("jupyter_notebook: CODE must be a string");
      endif

      __obj__.loadContext ();

      ## Add a statement to detect the value of the variable "ans"
      __code__ = [__code__, "\nans"];

      retval = strtrim (evalc (__code__, ["printf (\"error: \"); ", ...
                                          "printf (lasterror.message)"]));

      ## Handle the "ans" variable in the context.
      start_index = rindex (retval, "ans =") + 6;
      if (start_index > 6)
        if (start_index <= length (retval))
          end_index = start_index;
          ## FIXME: loops are slow.
          idx = find (retval(start_index+1:end) == "\n", 1);
          if (idx)
            end_index = start_index + idx;
          else
            end_index = length (retval);
          endif
          __obj__.context.ans = retval(start_index:end_index);
        else
          end_index = length (retval);
          __obj__.context.ans = "";
        endif

        ## Delete the output of the additional statement if the execution
        ## is completed with no errors.
        if (end_index == length (retval))
          ## Remove the extra new line if there are other outputs with
          ## the "ans" statement output
          if (start_index == 7)
            start_index = 1;
          else
            start_index -= 7;
          endif
          retval(start_index:end_index) = "";
        endif
      endif

      __obj__.saveContext ();

    endfunction


    function saveContext (obj)

      ## Save the context in private "obj" attribute.

      ## Handle the "ans" variable in the context.
      obj.context = struct ("ans", obj.context.ans);

      forbidden_var_names = {"__code__", "__obj__", "ans"};

      ## Get variable names.
      var_names = {evalin("caller", "whos").name};

      ## Store all variables to context.
      for i = 1:numel (var_names)
        if (! any (strcmp (var_names{i}, forbidden_var_names)))
          obj.context.(var_names{i}) = evalin ("caller", var_names{i});
        endif
      endfor

    endfunction


    function loadContext (obj)

      ## Load the context from private "obj" attribute.
      for [val, key] = obj.context
        assignin ("caller", key, val);
      endfor

    endfunction


    function embedImage (obj, cell_index, figHandle, printOptions)

      ## Embed images in the notebook.
      ##
      ## To support a new format:
      ## 1. Create a new function that embeds the new format
      ##    (e.g. embed_svg_image).
      ## 2. Add a new case to the switch-statement below.

      if (isempty (get (figHandle, "children")))
        error_text = {"The figure is empty!"};
        obj.addErrorOutput (cell_index, "The figure is empty!");
        return;
      endif

      ## Check if the resolution is correct
      if (isempty (str2num (printOptions.resolution)))
        obj.addErrorOutput (cell_index,
                            "A number is required for resolution, not a string");
        return;
      endif

      ## Check if the width is correct
      if (isempty (str2num (printOptions.width)))
        obj.addErrorOutput (cell_index,
                            "A number is required for width, not a string");
        return;
      endif

      ## Check if the height is correct
      if (isempty (str2num (printOptions.height)))
        obj.addErrorOutput (cell_index,
                            "A number is required for height, not a string");
        return;
      endif

      switch (lower (printOptions.imageFormat))
        case "png"
          display_output = obj.embed_png_jpg_image (figHandle,
                                                    printOptions, "png");
        case "jpg"
          display_output = obj.embed_png_jpg_image (figHandle,
                                                    printOptions, "jpg");
        case "svg"
          display_output = obj.embed_svg_image (figHandle, printOptions);
        otherwise
          obj.addErrorOutput (cell_index, ["Cannot embed the \'", ...
                                           printOptions.imageFormat, ...
                                           "\' image format\n"]);
          return;
      endswitch

      obj.notebook.cells{cell_index}.outputs{end+1} = display_output;

    endfunction


    function dstruct = embed_png_jpg_image (obj, figHandle, printOptions, fmt)

      if (strcmp (fmt, "png"))
        mime = "image/png";
      else
        mime = "image/jpeg";
      endif

      image_path = fullfile (obj.tmpdir, ["temp." fmt]);
      print (figHandle, image_path, ["-d" fmt],
             ["-r" printOptions.resolution]);

      dstruct.output_type = "display_data";
      dstruct.metadata.(mime).width  = printOptions.width;
      dstruct.metadata.(mime).height = printOptions.height;
      dstruct.data.("text/plain") = {"<IPython.core.display.Image object>"};
      dstruct.data.(mime) = base64_encode (uint8 (fileread (image_path)));

      delete (image_path);

    endfunction


    function dstruct = embed_svg_image (obj, figHandle, printOptions)

      image_path = fullfile (obj.tmpdir, "temp.svg");
      print (figHandle, image_path, "-dsvg", ["-r" printOptions.resolution]);

      dstruct.output_type = "display_data";
      dstruct.metadata = struct ();
      dstruct.data.("text/plain") = {"<IPython.core.display.SVG object>"};
      dstruct.data.("image/svg+xml") = strsplit (fileread (image_path), "\n");

      ## FIXME: The following is a workaround until we can properly print
      ##        SVG images in the right width and height.
      ## Detect the <svg> tag; it is either the first or the second item.
      if (strncmpi (dstruct.data.("image/svg+xml"){1}, "<svg", 4))
        i = 1;
      else
        i = 2;
      endif

      ## Embed the width and height in the image itself
      svg_tag = dstruct.data.("image/svg+xml"){i};
      svg_tag = regexprep (svg_tag, 'width=".*?"',
                           ['width="' printOptions.width 'px"']);
      svg_tag = regexprep (svg_tag, 'height=".*?"',
                           ['height="' printOptions.height 'px"']);
      dstruct.data.("image/svg+xml"){i} = svg_tag;

      delete (image_path);

    endfunction


    function addErrorOutput (obj, cell_index, error_msg)

      stream_output.name        = "stderr";
      stream_output.output_type = "stream";
      stream_output.text        = {error_msg};
      obj.notebook.cells{cell_index}.outputs{end+1} = stream_output;

    endfunction

  endmethods

endclassdef


## Note: Functional BIST tests are located in the 'test/jupyter-notebook'
##       directory.

## Test input validation
%!error <Invalid call> jupyter_notebook ()
%!error <NOTEBOOK_FILENAME must be a string> jupyter_notebook (1)
%!error <NOTEBOOK_FILENAME must be a string> jupyter_notebook (['a';'b'])
%!error <OPTIONS must be a struct> jupyter_notebook ("fname", 1)
