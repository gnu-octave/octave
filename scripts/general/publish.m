## Copyright (C) 2016-2017 Kai T. Ohlhus <k.ohlhus@gmail.com>
## Copyright (C) 2010 Fotios Kasolis <fotios.kasolis@gmail.com>
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} publish (@var{file})
## @deftypefnx {} {} publish (@var{file}, @var{output_format})
## @deftypefnx {} {} publish (@var{file}, @var{option1}, @var{value1}, @dots{})
## @deftypefnx {} {} publish (@var{file}, @var{options})
## @deftypefnx {} {@var{output_file} =} publish (@var{file}, @dots{})
##
## Generate a report from the Octave script file @var{file} in one of several
## output formats.
##
## The generated reports interpret any Publishing Markup in comments, which is
## explained in detail in the GNU Octave manual.  Assume the following
## example, using some Publishing Markup, to be the contents of the script file
## @file{pub_example.m}:
##
## @example
## @group
## ## Headline title
## #
## # Some *bold*, _italic_, or |monospaced| Text with
## # a <http://www.octave.org link to *GNU Octave*>.
## ##
##
## # "Real" Octave commands to be evaluated
## sombrero ()
##
## %% @sc{matlab} comment style ('%') is supported as well
## %
## % * Bulleted list item 1
## % * Bulleted list item 2
## %
## % # Numbered list item 1
## % # Numbered list item 2
## @end group
## @end example
##
## To publish this script file, type @code{publish ("pub_example.m")}.
##
## With only @var{file} given, a HTML report is generated in a
## subdirectory @file{html} relative to the current working directory.  The
## Octave commands are evaluated in a separate context and any figures
## created while executing the script file are included in the report.  All
## formatting syntax of @var{file} is treated according to the specified
## output format and included in the report.
##
## Using @code{publish (@var{file}, @var{output_format})} is equivalent
## to the function call using a structure
##
## @example
## @group
## @var{options}.format = @var{output_format};
## publish (@var{file}, @var{options})
## @end group
## @end example
##
## @noindent
## which is described below.  The same holds for using option/value pairs
##
## @example
## @group
## @var{options}.@var{option1} = @var{value1};
## publish (@var{file}, @var{options})
## @end group
## @end example
##
## The structure @var{options} can have the following field names.  If a
## field name is not specified, the default value is used:
##
## @itemize @bullet
## @item
## @samp{format} --- Output format of the published script file, one of
##
## @samp{html} (default), @samp{doc}, @samp{latex}, @samp{ppt},
## @samp{pdf}, or @samp{xml}.
##
## The output formats @samp{doc}, @samp{ppt}, and @samp{xml} are not currently
## supported.  To generate a @samp{doc} report, open a generated @samp{html}
## report with your office suite.
##
## In Octave custom formats are supported by implementing all callback
## subfunctions in a function file named
## @samp{__publish_<custom format>_output__.m}.  To obtain a template for the
## HTML format type:
##
## @example
## @group
## edit (fullfile (fileparts (which ("publish")), ...
##       "private", "__publish_html_output__.m"))
## @end group
## @end example
##
## @item
## @samp{outputDir} --- Full path of the directory where the generated report
## will be located.  If no directory is given, the report is generated in a
## subdirectory @file{html} relative to the current working directory.
##
## @item
## @samp{stylesheet} --- Not supported, only for @sc{matlab} compatibility.
##
## @item
## @samp{createThumbnail} --- Not supported, only for @sc{matlab}
## compatibility.
##
## @item
## @samp{figureSnapMethod} --- Not supported, only for @sc{matlab}
## compatibility.
##
## @item
## @samp{imageFormat} --- Desired format for any images produced while
## evaluating the code.  The allowed image formats depend on the output format:
##
## @itemize @bullet
## @item @samp{html}, @samp{xml} --- @samp{png} (default), any image format
## supported by Octave
##
## @item @samp{latex} --- @samp{epsc2} (default), any image format supported by
## Octave
##
## @item @samp{pdf} --- @samp{jpg} (default) or @samp{bmp}, note @sc{matlab}
## uses  @samp{bmp} as default
##
## @item @samp{doc} or @samp{ppt} --- @samp{png} (default), @samp{jpg},
## @samp{bmp}, or @samp{tiff}
## @end itemize
##
## @item
## @samp{maxWidth} and @samp{maxHeight} --- Maximum width (height) of the
## produced images in pixels.  An empty value means no restriction.  Both
## values must be set in order for the option to work properly.
##
## @samp{[]} (default), integer value @geq{} 0
##
## @item
## @samp{useNewFigure} --- Use a new figure window for figures created by the
## evaluated code.  This avoids side effects with already opened figure
## windows.
##
## @samp{true} (default) or @samp{false}
##
## @item
## @samp{evalCode} --- Evaluate code of the Octave source file
##
## @samp{true} (default) or @samp{false}
##
## @item
## @samp{catchError} --- Catch errors while evaluating code and continue
##
## @samp{true} (default) or @samp{false}
##
## @item
## @samp{codeToEvaluate} --- Octave commands that should be evaluated prior to
## publishing the script file.  These Octave commands do not appear in the
## generated report.
##
## @item
## @samp{maxOutputLines} --- Maximum number of output lines from code
## evaluation which are included in output.
##
## @samp{Inf} (default) or integer value > 0
##
## @item
## @samp{showCode} --- Show the evaluated Octave commands in the generated
## report
##
## @samp{true} (default) or @samp{false}
## @end itemize
##
## The option output @var{output_file} is a string with path and file name
## of the generated report.
##
## @seealso{grabcode}
## @end deftypefn

function output_file = publish (file, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (exist (file, "file") != 2)
    error ("publish: FILE does not exist");
  endif

  ## Check file to be in Octave's load path
  [file_path, file_name, file_ext] = fileparts (file);
  if (isempty (file_path))
    file_path = pwd;
  endif
  if (exist ([file_name, file_ext]) != 2)
    error (["publish: " file " is not in the load path"]);
  endif

  ## Check file extension and for an Octave script

  file_info = __which__ (file_name);
  if (! strcmp (file_ext, ".m") || ! strcmp (file_info.type, "script"))
    error ("publish: only script files can be published");
  endif

  ## Check file to be parsable
  __parse_file__ (file);

  ## Get structure with necessary options
  options = struct ();
  if (numel (varargin) == 1)
    ## Call: publish (file, format)
    if (ischar (varargin{1}))
      options.format = varargin{1};
    ## Call: publish (file, options)
    elseif (isstruct (varargin{1}))
      options = varargin{1};
    else
      error ("publish: second argument must be OUTPUT_FORMAT or OPTIONS");
    endif
  ## Call: publish (file, Name1, Value1, Name2, Value2, ...)
  elseif (rem (numel (varargin), 2) == 0
          && all (cellfun (@ischar, varargin(1:2:end))))
    options = cell2struct (varargin(2:2:end), varargin(1:2:end), 2);
  else
    error ("publish: invalid arguments");
  endif

  ## Validate options struct

  ## Options for the output
  if (! isfield (options, "format"))
    options.format = "html";
  else
    ## FIXME: Implement remaining formats
    if (any (strcmpi (options.format, {"doc", "ppt", "xml"})))
      error ('publish: Output format "%s" is not yet supported',
             options.format);
    endif
    ## Supported or custom output format
    supported_formats = {"html", "doc", "latex", "ppt", "xml", "pdf"};
    if (! any (strcmpi (options.format, supported_formats)))
      ## Check existance of custom formatter
      custom_formatter = ["__publish_", options.format, "_output__"];
      if (! exist (custom_formatter, "file"))
        error (['publish: Custom output format "%s" requires the ', ...
                'formatter function:\n\n\t%s\n\n', ...
                '\tSee "help publish" for more information.'],
                options.format, custom_formatter);
      endif
    else
      options.format = validatestring (options.format, supported_formats);
    endif
  endif

  if (! isfield (options, "outputDir"))
    ## Matlab R2016a doc says default is "", but specifies to create a
    ## subdirectory named "html" in the current working directory.
    options.outputDir = fullfile (file_path, "html");
  elseif (! ischar (options.outputDir))
    error ("publish: OUTPUTDIR must be a string");
  endif

  if (! isfield (options, "stylesheet"))
    options.stylesheet = "";
  elseif (! ischar (options.stylesheet))
    error ("publish: STYLESHEET must be a string");
  endif

  ## Options for the figures
  if (! isfield (options, "createThumbnail"))
    options.createThumbnail = true;
  elseif (! isscalar (options.createThumbnail)
          || ! isreal (options.createThumbnail))
    error ("publish: CREATETHUMBNAIL must be TRUE or FALSE");
  endif

  if (! isfield (options, "figureSnapMethod"))
    options.figureSnapMethod = "entireGUIWindow";
  else
    options.figureSnapMethod = validatestring (options.figureSnapMethod, ...
      {"entireGUIWindow", "print", "getframe", "entireFigureWindow"});
    ## FIXME: implement other SnapMethods
    warning ("publish: option FIGURESNAPMETHOD currently not supported");
  endif

  if (! isfield (options, "imageFormat"))
    switch (options.format)
      case "latex"
        options.imageFormat = "epsc2";
      case "pdf"
        ## Note: Matlab R2016a uses bmp as default
        options.imageFormat = "jpg";
      otherwise
        options.imageFormat = "png";
    endswitch
  elseif (! ischar (options.imageFormat))
    error ("publish: IMAGEFORMAT must be a string");
  else
    ## Check valid imageFormat for chosen format
    ##   html, latex, and xml accept any imageFormat
    switch (options.format)
      case {"doc", "ppt"}
        options.imageFormat = validatestring (options.imageFormat,
                                              {"png", "jpg", "bmp", "tiff"});
      case "pdf"
        options.imageFormat = validatestring (options.imageFormat,
                                              {"bmp", "jpg"});
    endswitch
  endif

  if (! isfield (options, "maxHeight"))
    options.maxHeight = [];
  elseif (! isscalar (options.maxHeight) || options.maxHeight < 1)
    error ("publish: MAXHEIGHT must be a positive integer");
  else
    options.maxHeight = uint64 (options.maxHeight);
  endif

  if (! isfield (options, "maxWidth"))
    options.maxWidth = [];
  elseif (! isscalar (options.maxWidth) || options.maxWidth < 1)
    error ("publish: MAXWIDTH must be a positive integer");
  else
    options.maxWidth = uint64 (options.maxWidth);
  endif

  if (! isfield (options, "useNewFigure"))
    options.useNewFigure = true;
  elseif (! isscalar (options.useNewFigure) || ! isreal (options.useNewFigure))
    error ("publish: USENEWFIGURE must be TRUE or FALSE");
  endif

  ## Options for the code
  if (! isfield (options, "evalCode"))
    options.evalCode = true;
  elseif (! isscalar (options.evalCode) || ! isreal (options.evalCode))
    error ("publish: EVALCODE must be TRUE or FALSE");
  endif

  if (! isfield (options, "catchError"))
    options.catchError = true;
  elseif (! isscalar (options.catchError) || ! isreal (options.catchError))
    error ("publish: CATCHERROR must be TRUE or FALSE");
  endif

  if (! isfield (options, "codeToEvaluate"))
    options.codeToEvaluate = "";
  elseif (! ischar (options.codeToEvaluate))
    error ("publish: CODETOEVALUTE must be a string");
  endif

  if (! isfield (options, "maxOutputLines"))
    options.maxOutputLines = Inf;
  elseif (! isscalar (options.maxOutputLines) || options.maxOutputLines < 0)
    error ("publish: MAXOUTPUTLINES must be an integer >= 0");
  else
    options.maxOutputLines = uint64 (options.maxOutputLines);
  endif

  if (! isfield (options, "showCode"))
    options.showCode = true;
  elseif (! isscalar (options.showCode) || ! isreal (options.showCode))
    error ("publish: SHOWCODE must be TRUE or FALSE");
  endif

  doc.title = "";
  doc.intro = "";
  doc.body = cell ();
  doc.m_source = deblank (read_file_to_cellstr (file));
  doc.m_source_file_name = file;

  ## Split code and paragraphs, find formatting
  doc = parse_m_source (doc);

  ## Create output directory
  [status, msg] = mkdir (options.outputDir);
  if (status != 1)
    error ("publish: cannot create output directory: %s", msg);
  endif

  if (options.evalCode)
    doc = eval_code (doc, options);
  endif

  output_file = create_output (doc, options);

endfunction


function doc = parse_m_source (doc)
  ## PARSE_M_SOURCE First parsing level
  ##   This function extracts the overall structure (paragraphs and code
  ##   sections) given in doc.m_source.
  ##
  ##   The result is written to doc.body, which then contains a cell
  ##   vector of structs, either of
  ##
  ##     a) {struct ("type", "code", ...
  ##                 "lines", [a, b], ...
  ##                 "output", [])}
  ##     b) {struct ("type", "section", ...
  ##                 "content", title_str)}
  ##
  ##   Second parsing level is invoked for the paragraph contents, resulting
  ##   in more elements for doc.body.

  if (isempty (doc.m_source))
    return;  # Nothing to parse
  endif

  ## Parsing helper functions
  ##
  ## Checks line to have N "%" or "#" lines
  ## followed either by a space or is end of string
  is_publish_markup = @(cstr, N) ...
    any (strncmp (char (cstr), {"%%%", "##"}, N)) ...
    && ((length (char (cstr)) == N) || ((char (cstr))(N + 1) == " "));
  ## Checks line of cellstring to be a paragraph line
  is_paragraph = @(cstr) is_publish_markup (cstr, 1);
  ## Checks line of cellstring to be a section headline
  is_head = @(cstr) is_publish_markup (cstr, 2);
  ## Checks line of cellstring to be a headline without section break, using
  ## the cell mode in Matlab (for compatibility), just treated as a new head.
  is_no_break_head = @(cstr) is_publish_markup (cstr, 3);

  ## Find the indices of paragraphs starting with "%%", "##", or "%%%"
  par_start_idx = find (cellfun (is_head, doc.m_source)
                        | cellfun (is_no_break_head, doc.m_source));

  ## If the whole document is code
  if (isempty (par_start_idx))
    doc.body{end+1}.type = "code";
    doc.body{end}.content = strtrim (strjoin (doc.m_source, "\n"));
    doc.body{end}.lines = [1, length(doc.m_source)];
    doc.body{end}.output = {};
    return;
  endif

  ## Determine continuous range of paragraphs
  par_end_idx = [par_start_idx(2:end) - 1, length(doc.m_source)];
  for i = 1:numel (par_end_idx)
    idx = find (! cellfun (is_paragraph,
                           doc.m_source(par_start_idx(i) + 1:par_end_idx(i))));
    if (! isempty (idx))
      par_end_idx(i) = par_start_idx(i) + idx(1) - 1;
    endif
  endfor
  ## Code sections between paragraphs
  code_start_idx = par_end_idx(1:end-1) + 1;
  code_end_idx = par_start_idx(2:end) - 1;
  ## Code at the beginning?
  if (par_start_idx(1) > 1)
    code_start_idx = [1, code_start_idx];
    code_end_idx = [par_start_idx(1) - 1, code_end_idx];
  endif
  ## Code at the end?
  if (par_end_idx(end) < length (doc.m_source))
    code_start_idx = [code_start_idx, par_end_idx(end) + 1];
    code_end_idx = [code_end_idx, length(doc.m_source)];
  endif
  ## Remove overlaps
  idx = code_start_idx > code_end_idx;
  code_start_idx(idx) = [];
  code_end_idx(idx) = [];
  ## Remove empty code blocks
  idx = [];
  for i = 1:numel (code_start_idx)
    if (all (cellfun (@(cstr) isempty (char (cstr)),
                      doc.m_source(code_start_idx(i):code_end_idx(i)))))
      idx = [idx, i];
    endif
  endfor
  code_start_idx(idx) = [];
  code_end_idx(idx) = [];

  ## Try to find a document title and introduction text
  ##   1. First paragraph must start in first line
  ##   2. Second paragraph must start before any code
  title_offset = 0;
  if (is_head (doc.m_source{1})
      && ! isempty (par_start_idx)
      && par_start_idx(1) == 1
      && (isempty (code_start_idx)
          || (length (par_start_idx) > 1
              && par_start_idx(2) < code_start_idx(1))))
    doc.title = doc.m_source{1};
    doc.title = doc.title(4:end);
    content = doc.m_source(2:par_end_idx(1));
    ## Strip leading "# "
    content = cellfun (@(c) cellstr (c(3:end)), content);
    doc.intro = parse_paragraph_content (content);
    title_offset = 1;
  endif

  ## Add non-empty paragraphs and code to doc
  j = 1;
  i = (1 + title_offset);
  while (i <= numel (par_start_idx) || j <= numel (code_start_idx))
    ## Add code while there is code left
    ##   and code is before the next paragraph or there are no more paragraphs
    while (j <= numel (code_start_idx)
           && (i > numel (par_start_idx)
               || par_start_idx(i) > code_start_idx(j)))
      doc.body{end+1}.type = "code";
      lines = [code_start_idx(j), code_end_idx(j)];
      doc.body{end}.content = ...
        strtrim (strjoin (doc.m_source(lines(1):lines(2)), "\n"));
      doc.body{end}.lines = lines;
      doc.body{end}.output = {};
      j++;
    endwhile

    if (i <= numel (par_start_idx))
      type_str = "section";
      title_str = doc.m_source{par_start_idx(i)};
      if (is_head (doc.m_source(par_start_idx(i))))
        title_str = title_str(4:end);
      else
        title_str = title_str(5:end);
      endif
      ## Append, if paragraph title is given
      if (! isempty (title_str))
        doc.body{end+1}.type = type_str;
        doc.body{end}.content = title_str;
      endif

      content = doc.m_source(par_start_idx(i) + 1:par_end_idx(i));
      ## Strip leading "# "
      content = cellfun (@(c) cellstr (c(3:end)), content);
      doc.body = [doc.body, parse_paragraph_content(content)];
      i++;
    endif
  endwhile

endfunction


function p_content = parse_paragraph_content (content)
  ## PARSE_PARAGRAPH_CONTENT second parsing level
  ##
  ##   Parses the content of a paragraph (without potential title) and
  ##   returns a cell vector of structs, that can be appended to doc.body,
  ##   either of
  ##
  ##     a) {struct ("type", "preformatted_code", ...
  ##                 "content", code_str)}
  ##     b) {struct ("type", "preformatted_text", ...
  ##                 "content", text_str)}
  ##     c) {struct ("type", "bulleted_list", ...
  ##                 "content", {"item1", "item2", ..})}
  ##     d) {struct ("type", "numbered_list", ...
  ##                 "content", {"item1", "item2", ..})}
  ##     e) {struct ("type", "include", ...
  ##                 "content", file_str)}
  ##     f) {struct ("type", "graphic", ...
  ##                 "content", file_str)}
  ##     g) {struct ("type", "html", ...
  ##                 "content", html_str)}
  ##     h) {struct ("type", "latex", ...
  ##                 "content", latex_str)}
  ##     i) {struct ("type", "text", ...
  ##                 "content", text_str)}
  ##
  ##   Option i) might contain:
  ##
  ##     * Italic "_", bold "*", and monospaced "|" text
  ##     * Inline "$" and block "$$" LaTeX math
  ##     * Links
  ##     * Trademark symbols

  p_content = cell ();

  if (isempty (content))
    return;
  endif

  ## Split into blocks separated by empty lines
  idx = [0, find(cellfun (@isempty, content)), length(content) + 1];
  ## For each block
  for i = find (diff (idx) > 1)
    block = content(idx(i) + 1:idx(i+1) - 1);

    ## Octave code (two leading spaces)
    if (all (cellfun (@(c) strncmp (char (c), "  ", 2), block)))
      p_content{end+1}.type = "preformatted_code";
      block = cellfun (@(c) cellstr (c(3:end)), block);
      p_content{end}.content = strjoin (block, "\n");
      continue;
    endif

    ## Preformatted text (one leading space)
    if (all (cellfun (@(c) strncmp (char (c), " ", 1), block)))
      p_content{end+1}.type = "preformatted_text";
      block = cellfun (@(c) cellstr (c(2:end)), block);
      p_content{end}.content = strjoin (block, "\n");
      continue;
    endif

    ## Bulleted list starts with "* "
    if (strncmp (block{1}, "* ", 2))
      p_content{end+1}.type = "bulleted_list";
      tmpstr = strjoin (block, "\n");
      ## Revove first "* "
      tmpstr = tmpstr(3:end);
      ## Split items
      p_content{end}.content = strsplit (tmpstr, "\n* ");
      continue;
    endif

    ## Numbered list starts with "# "
    if (strncmp (block{1}, "# ", 2))
      p_content{end+1}.type = "numbered_list";
      tmpstr = strjoin (block, "\n");
      ## Revove first "# "
      tmpstr = tmpstr(3:end);
      ## Split items
      p_content{end}.content = strsplit (tmpstr, "\n# ");
      continue;
    endif

    ## Include <include>fname.m</include>
    if (! isempty (fname = regexpi (strjoin (block, ""),
                                    '^<include>(.*)</include>$',
                                    "tokens")))
      ## Includes result in preformatted code
      p_content{end+1}.type = "preformatted_code";
      include_code = read_file_to_cellstr (strtrim ((fname{1}){1}));
      p_content{end}.content = strjoin (include_code, "\n");

      continue;
    endif

    ## Graphic <<myGraphic.png>>
    if (! isempty (fname = regexpi (strjoin (block, ""),
                                    '^<<(.*)>>$',
                                    "tokens")))
      p_content{end+1}.type = "graphic";
      p_content{end}.content = strtrim ((fname{1}){1});
      continue;
    endif

    ## Parse remaining blocks line by line
    j = 1;
    while (j <= numel (block))
      ## HTML markup
      if (strcmpi (block{j}, "<html>"))
        start_html = j + 1;
        while (j < numel (block) && ! strcmpi (block{j}, "</html>"))
          j++;
        endwhile
        if (j == numel (block) && ! strcmpi (block{j}, "</html>"))
          warning ("publish: no closing </html> found");
        else
          j++;  ## Skip closing tag
        endif
        if (j > start_html)
          p_content{end+1}.type = "html";
          p_content{end}.content = strjoin (block(start_html:j-2), "\n");
        endif
      ## LaTeX markup
      elseif (strcmpi (block{j}, "<latex>"))
        start_latex = j + 1;
        while (j < numel (block) && ! strcmpi (block{j}, "</latex>"))
          j++;
        endwhile
        if (j == numel (block) && ! strcmpi (block{j}, "</latex>"))
          warning ("publish: no closing </latex> found");
        else
          j++;  ## Skip closing tag
        endif
        if (j > start_latex)
          p_content{end+1}.type = "latex";
          p_content{end}.content = strjoin (block(start_latex:j-2), "\n");
        endif
      ## Remaining normal text or markups belonging to normal text
      ## that are handled while output generation:
      ##
      ## * Italic "_", bold "*", and monospaced "|" text
      ## * Inline "$" and block "$$" LaTeX math
      ## * Links
      ## * Trademark symbols
      else
        if (j == 1 || isempty (p_content)
            || ! strcmp (p_content{end}.type, "text"))
          p_content{end+1}.type = "text";
          p_content{end}.content = block{j};
        else
          p_content{end}.content = strjoin ({p_content{end}.content, ...
                                             block{j}}, "\n");
        endif
        j++;
      endif
    endwhile
  endfor
endfunction


function m_source = read_file_to_cellstr (file)
  ## READ_FILE_TO_CELLSTR reads a given file line by line to a cellstring

  fid = fopen (file, "r");
  i = 0;
  do
    m_source{++i} = fgetl (fid);
  until (! ischar (m_source{i}))
  fclose (fid);
  m_source = m_source(1:end-1);  # No EOL
endfunction


function ofile = create_output (doc, options)
  ## CREATE_OUTPUT creates the desired output file

  formatter = [];
  switch (options.format)
    case "html"
      formatter = @__publish_html_output__;
    case {"latex", "pdf"}
      formatter = @__publish_latex_output__;
    otherwise
      ## Custom formatter
      formatter = eval (["@__publish_", options.format, "_output__"]);
  endswitch

  ## Use title, or if not given, the m-file name
  title_str = doc.title;
  if (isempty (title_str))
    [~, title_str] = fileparts (doc.m_source_file_name);
  endif

  content = formatter ("header", title_str,
                       format_output (doc.intro, formatter, options),
                       get_toc (doc.body, formatter));
  content = [content, format_output(doc.body, formatter, options)];
  content = [content, formatter("footer", strjoin (doc.m_source, "\n"))];

  ## Write file
  [~, ofile] = fileparts (doc.m_source_file_name);
  ofile_name = [ofile, formatter("output_file_extension")];
  ofile = fullfile (options.outputDir, ofile_name);
  fid = fopen (ofile, "w");
  fputs (fid, content);
  fclose (fid);

  ## Compile LaTeX, if compiler found
  if (strcmp (options.format, "pdf"))
    status = system ("pdflatex --version");
    if (status == 0)
      for i = 1:2
        ## FIXME: This looks very likely to break when switching OS
        system (["cd ", options.outputDir," && pdflatex ", ofile_name]);
      endfor
    endif
  endif
endfunction


function toc_cstr = get_toc (cstr, formatter)
  ## GET_TOC extracts the table of contents from a cellstring (e.g., doc.body)
  ## with each section headline as a cell in a returned cellstring.

  toc_cstr = cell ();
  for i = 1:numel (cstr)
    if (strcmp (cstr{i}.type, "section"))
      toc_cstr{end+1} = format_text (cstr{i}.content, formatter);
    endif
  endfor
endfunction


function str = format_output (cstr, formatter, options)
  ## FORMAT_OUTPUT steps through all blocks (doc.intro or doc.body) in cstr and
  ## produces a single result string with the source code in the desired output
  ## format.
  ##
  ##   formatter has the only knowledge how to enforce the target format
  ##   and produces for each block the necessary target format source string.

  str = "";
  for i = 1:numel (cstr)
    switch (cstr{i}.type)
      case "code"
        if (options.showCode)
          str = [str, formatter(cstr{i}.type, cstr{i}.content)];
        endif
        if ((options.evalCode) && (! isempty (cstr{i}.output)))
          str = [str, formatter("code_output", cstr{i}.output)];
        endif
      case {"text", "section"}
        str = [str, formatter(cstr{i}.type, ...
                              format_text (cstr{i}.content, formatter))];
      case {"bulleted_list", "numbered_list"}
        items = cellfun (@(str) format_text(str, formatter), ...
                         cstr{i}.content, "UniformOutput", false);
        str = [str, formatter(cstr{i}.type, items)];
      otherwise
        str = [str, formatter(cstr{i}.type, cstr{i}.content)];
    endswitch
  endfor

endfunction


function str = format_text (str, formatter)
  ## FORMAT_TEXT formats inline formats in strings.
  ##   These are: links, bold, italic, monospaced, (TM), (R)

  ## Regular expressions for the formats:
  ##
  ## * Links "<http://www.someurl.com>"
  ## * Links "<octave:Function TEXT>"
  ## * Links "<http://www.someurl.com TEXT>"
  ## * inline "$" and block "$$" LaTeX math
  regexes = {'<\S{3,}[^\s<>]*>', ...
             '<octave:[^\s<>]* *[^<>$]*>', ...
             '<\S{3,}[^\s<>]* *[^<>$]*>', ...
             '\${1,2}.*?\${1,2}', ...
             '\*[^*]*\*', ...  # Bold
             '_[^_]*_', ...    # Italic
             '\|[^|]*\|'};     # Monospaced

  ## Function to escape some special characters for the GNU Octave manual,
  ## see https://www.gnu.org/software/texinfo/manual/texinfo/html_node/HTML-Xref-Node-Name-Expansion.html
  texinfo_esc = @(str) strrep (strrep (str, "-", "_002d"), "_", "_005f");

  ## Substitute all occurrences with placeholders
  placeholder_cstr = {};
  plh = 0;
  for i = 1:numel (regexes)
    cstr = regexp (str, regexes{i}, "match");
    for j = 1:numel (cstr)
      plh += 1;
      str = regexprep (str, regexes{i}, ["PUBLISHPLACEHOLDER" num2str(plh)],
                       "once");
      switch (i)
        case 1
          ## Links "<http://www.someurl.com>"
          url = cstr{j};
          cstr{j} = formatter ("link", url(2:end-1), url(2:end-1));
        case 2
          ## Links "<octave:Function TEXT>"
          idx = strfind (cstr{j}, " ");
          url = cstr{j};
          url = texinfo_esc(url(9:idx-1));
          v = version ();
          if (v(end) == '+')
            v = "interpreter";
          endif
          url = sprintf ( ...
            "https://www.gnu.org/software/octave/doc/%s/XREF%s.html", v, url);
          txt = cstr{j};
          txt = format_text (txt(idx+1:end-1), formatter);
          cstr{j} = formatter ("link", url, txt);
        case 3
          ## Links "<http://www.someurl.com TEXT>"
          idx = strfind (cstr{j}, " ");
          url = cstr{j};
          url = url(2:idx-1);
          txt = cstr{j};
          txt = format_text (txt(idx+1:end-1), formatter);
          cstr{j} = formatter ("link", url, txt);
        case 4
          ## inline "$" and block "$$" LaTeX math --> do nothing
        case 5
          ## Bold
          txt = cstr{j};
          cstr{j} = formatter ("bold", format_text (txt(2:end-1), formatter));
        case 6
          ## Italic
          txt = cstr{j};
          cstr{j} = formatter ("italic", format_text (txt(2:end-1), formatter));
        case 7
          ## Monospaced
          txt = cstr{j};
          cstr{j} = formatter ("monospaced", format_text (txt(2:end-1), ...
                               formatter));
      endswitch
    endfor
    placeholder_cstr = [placeholder_cstr, cstr];
  endfor

  ## Replace special symbols
  str = strrep (str, "(TM)", formatter ("TM"));
  str = strrep (str, "(R)", formatter ("R"));

  ## Restore placeholders
  for i = plh:-1:1
    str = strrep (str, ["PUBLISHPLACEHOLDER" sprintf("%d", i)],
                       placeholder_cstr{i});
  endfor

endfunction


function doc = eval_code (doc, options)
  ## EVAL_CODE Third level parsing
  ##
  ##   Generates the output of the script code and takes care of generated
  ##   figures.

  ## Necessary as the code does not run interactively
  page_screen_output (false, "local");

  ## Remember previously opened figures
  fig_ids = findall (0, "type", "figure");
  [~, fig_name] = fileparts (doc.m_source_file_name);
  fig_num = 1;
  fig_list = struct ();

  ## mat-file used as temporary context
  tmp_context = [tempname(), ".mat"];

  ## Evaluate code, that does not appear in the output.
  eval_code_helper (tmp_context, options.codeToEvaluate);

  ## Create a new figure, if there are existing plots
  if (! isempty (fig_ids) && options.useNewFigure)
    figure ();
  endif

  for i = 1:numel (doc.body)
    if (strcmp (doc.body{i}.type, "code"))
      r = doc.body{i}.lines;
      code_str = strjoin (doc.m_source(r(1):r(2)), "\n");
      if (options.catchError)
        try
          doc.body{i}.output = eval_code_helper (tmp_context, code_str);
         catch err
          doc.body{i}.output = cellstr (["error: ", err.message, ...
                                                "\n\tin:\n\n", code_str]);
        end_try_catch
      else
        doc.body{i}.output = eval_code_helper (tmp_context, code_str);
      endif

      ## Check for newly created figures ...
      fig_ids_new = setdiff (findall (0, "type", "figure"), fig_ids);
      ## ... and save them
      for j = 1:numel (fig_ids_new)
        drawnow ();
        if (isempty (get (fig_ids_new(j), "children")))
          continue;
        endif
        fname = [fig_name, "-", sprintf("%d", fig_num)];
        if (strncmp (options.imageFormat, "eps", 3))
          fname = [fname ".eps"];
        else
          fname = [fname "." options.imageFormat];
        endif
        print_opts = {fig_ids_new(j), ...
                      fullfile(options.outputDir, fname), ...
                      ["-d" options.imageFormat], "-color"};
        if (! isempty (options.maxWidth) && ! isempty (options.maxHeight))
          print_opts{end+1} = sprintf("-S%d,%d", options.maxWidth,
                                                 options.maxHeight);
        elseif (! isempty (options.maxWidth) || ! isempty (options.maxWidth))
          warning (["publish: specify both options.maxWidth ", ...
                              "and options.maxWidth"]);
        endif
        print (print_opts{:});
        fig_num++;
        delete (fig_ids_new(j));
        fig_elem = cell ();
        fig_elem{1} = struct ("type", "graphic", "content", fname);
        if (isfield (fig_list, num2str (i)))
          fig_elem = [getfield(fig_list, sprintf ("%d", i)), fig_elem];
        endif
        fig_list = setfield (fig_list, sprintf ("%d", i), fig_elem);
        ## Create a new figure, if there are existing plots
        if (isempty (setdiff (findall (0, "type", "figure"), fig_ids)) ...
            && ! isempty (fig_ids) && options.useNewFigure)
          figure ();
        endif
      endfor

      ## Truncate output to desired length
      if (options.maxOutputLines < length (doc.body{i}.output))
        doc.body{i}.output = doc.body{i}.output(1:options.maxOutputLines);
      endif
      doc.body{i}.output = strjoin (doc.body{i}.output, "\n");
    endif
  endfor

  ## Close any figures opened by publish function
  delete (setdiff (findall (0, "type", "figure"), fig_ids));

  ## Remove temporary context
  unlink (tmp_context);

  ## Insert figures to document
  fig_code_blocks = fieldnames (fig_list);
  body_offset = 0;
  for i = 1:numel (fig_code_blocks)
    elems = getfield (fig_list, fig_code_blocks{i});
    ## Compute index where the figure(s) has to be inserted
    j = str2double (fig_code_blocks{i}) + body_offset;
    doc.body = [doc.body(1:j), elems, doc.body(j+1:end)];
    body_offset = body_offset + numel (elems);
  endfor

endfunction


function ___cstr___ = eval_code_helper (___context___, ___code___)
  ## EVAL_CODE_HELPER evaluates a given string with Octave code in an extra
  ## temporary context and returns a cellstring with the eval output.

  ## FIXME: code may contain potential conflicting variables named ___code___,
  ## ___context___, or ___cstr___.  Is there a better solution?
  if (isempty (___code___))
    return;
  endif

  if (exist (___context___, "file") == 2)
    load (___context___);
  endif

  ___cstr___ = strsplit (evalc (___code___), "\n");

  clear ___code___;
  save ("-binary", ___context___);
endfunction


## FIXME: Missing any functional BIST tests
## FIXME: Need to create a temporary file for use with error testing

## Test input validation
%!error publish ()
%!error publish (1)
%!error <FILE does not exist> publish ("%%_non_existent_file_%%.m")
%!error <only script files can be published> publish ("publish.m")
%!error publish ("test_script.m", "format", "html", "showCode")
