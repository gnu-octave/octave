## Copyright (C) 2016 Kai T. Ohlhus <k.ohlhus@gmail.com>
## Copyright (C) 2010 Fotios Kasolis <fotios.kasolis@gmail.com>
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
## @deftypefn  {} {} publish (@var{filename})
## @deftypefnx {} {} publish (@var{filename}, @var{output_format})
## @deftypefnx {} {} publish (@var{filename}, @var{option1}, @var{value1}, @dots{})
## @deftypefnx {} {} publish (@var{filename}, @var{options})
## @deftypefnx {} {@var{output_file} =} publish (@var{filename}, @dots{})
##
## Generate reports from Octave script files in several output formats.
##
## The generated reports consider Publishing Markup in comments,
## which is explained in detail in the GNU Octave manual.  Assume the
## following example, using some Publishing Markup, to be the content
## of a script file named @samp{example.m}:
##
## @example
## @group
## %% Headline title
## %
## % Some *bold*, _italic_, or |monospaced| Text with
## % a <http://www.octave.org link to *GNU Octave*>.
## %%
##
## # "Real" Octave commands to be evaluated
## sombrero ()
##
## ## Octave comment style supported as well
## #
## # * Bulleted list item 1
## # * Bulleted list item 2
## #
## # # Numbered list item 1
## # # Numbered list item 2
## @end group
## @end example
##
## To publish this script file, type @code{publish ("example.m")}.
##
## With only @var{filename} given, a HTML report is generated in a
## subdirectory @samp{html} relative to the current working directory.
## The Octave commands are evaluated in a seperate context and any
## figures created while executing the script file are included in the
## report.  All formatting syntax of @var{filename} is treated according
## to the specified output format and included in the report.
##
## Using @code{publish (@var{filename}, @var{output_format})} is
## equivalent to the function call using a structure
##
## @example
## @group
## @var{options}.format = @var{output_format};
## publish (@var{filename}, @var{options})
## @end group
## @end example
##
## @noindent
## which is described below.  The same holds for using option-value-pairs
##
## @example
## @group
## @var{options}.@var{option1} = @var{value1};
## publish (@var{filename}, @var{options})
## @end group
## @end example
##
## The structure @var{options} can have the following field names.  If a
## field name is not specified, the default value is considered:
##
## @itemize @bullet
## @item
## @samp{format} --- Output format of the published script file, one of
##
## @samp{html} (default), @samp{doc}, @samp{latex}, @samp{ppt},
## @samp{xml}, or @samp{pdf}.
##
## The output formats @samp{doc}, @samp{ppt}, and @samp{xml} are currently
## not supported.  To generate a @samp{doc} report, open a generated
## @samp{html} report with your office suite.
##
## @item
## @samp{outputDir} --- Full path string of a directory, where the generated
## report will be located.  If no directory is given, the report is generated
## in a subdirectory @samp{html} relative to the current working directory.
##
## @item
## @samp{stylesheet} --- Not supported, only for @sc{matlab} compatibility.
##
## @item
## @samp{createThumbnail} --- Not supported, only for @sc{matlab} compatibility.
##
## @item
## @samp{figureSnapMethod} --- Not supported, only for @sc{matlab} compatibility.
##
## @item
## @samp{imageFormat} --- Desired format for images produced, while
## evaluating the code.  The allowed image formats depend on the output
## format:
##
## @itemize @bullet
## @item @samp{html} and @samp{xml} --- @samp{png} (default), any other
## image format supported by Octave
##
## @item @samp{latex} --- @samp{epsc2} (default), any other image format
## supported by Octave
##
## @item @samp{pdf} --- @samp{jpg} (default) or @samp{bmp}, note @sc{matlab}
## uses  @samp{bmp} as default
##
## @item @samp{doc} or @samp{ppt} --- @samp{png} (default), @samp{jpg},
## @samp{bmp}, or @samp{tiff}
## @end itemize
##
## @item
## @samp{maxHeight} and @samp{maxWidth} --- Maximum height (width) of the
## produced images in pixels.  An empty value means no restriction.  Both
## values have to be set, to work properly.
##
## @samp{[]} (default), integer value @geq{} 0
##
## @item
## @samp{useNewFigure} --- Use a new figure window for figures created by
## the evaluated code.  This avoids side effects with already opened figure
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
## @samp{catchError} --- Catch errors while code evaluation and continue
##
## @samp{true} (default) or @samp{false}
##
## @item
## @samp{codeToEvaluate} --- Octave commands that should be evaluated prior
## to publishing the script file.  These Octave commands do not appear in the
## generated report.
##
## @item
## @samp{maxOutputLines} --- Maximum number of shown output lines of the
## code evaluation
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
## The returned @var{output_file} is a string with the path and file name
## of the generated report.
##
## @seealso{grabcode}
## @end deftypefn

function output_file = publish (file, varargin)
  narginchk (1, Inf);
  nargoutchk (0, 1);

  if (exist (file, "file") != 2)
    error ("publish: FILE does not exist.");
  endif

  ## Check file extension and for an Octave script
  [~, file_name, file_ext] = fileparts (file);
  file_info = __which__ (file_name);

  if ((! strcmp (file_ext, ".m")) || (! strcmp (file_info.type, "script")))
    error ("publish: Only Octave script files can be published.");
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
      error ("publish: Invalid second argument.");
    endif
  ## Call: publish (file, Name1, Value1, Name2, Value2, ...)
  elseif ((rem (numel (varargin), 2) == 0) ...
          && (all (cellfun (@ischar, varargin))))
    for i = 1:2:numel(varargin)
      options = setfield (options, varargin{i}, varargin{i + 1});
    endfor
  else
    error ("publish: Invalid or inappropriate arguments.");
  endif

  ##
  ## Validate options struct
  ##

  ## Options for the output
  if (! isfield (options, "format"))
    options.format = "html";
  else
    options.format = validatestring (options.format, ...
      {"html", "doc", "latex", "ppt", "xml", "pdf"});
    ## TODO: implement remaining formats
    if (any (strcmp (options.format, {"doc", "ppt", "xml"})))
      error ("publish: Output format currently not supported");
    endif
  endif

  if (! isfield (options, "outputDir"))
    ## Matlab R2016a doc says default is "", but specifies to create a sub
    ## directory named "html" in the current working directory.
    options.outputDir = "html";
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
  elseif ((! isscalar (options.createThumbnail)) ...
          || (! isbool (options.createThumbnail)))
    error ("publish: CREATETHUMBNAIL must be TRUE or FALSE");
  endif

  if (! isfield (options, "figureSnapMethod"))
    options.figureSnapMethod = "entireGUIWindow";
  else
    options.figureSnapMethod = validatestring (options.figureSnapMethod, ...
      {"entireGUIWindow", "print", "getframe", "entireFigureWindow"});
    ## TODO: implement
    warning ("publish: option FIGURESNAPMETHOD currently not supported")
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
    ## check valid imageFormat for chosen format
    ##   html, latex, and xml accept any imageFormat
    switch (options.format)
      case {"doc", "ppt"}
        options.imageFormat = validatestring (options.imageFormat, ...
          {"png", "jpg", "bmp", "tiff"});
      case "pdf"
        options.imageFormat = validatestring (options.imageFormat, ...
          {"bmp", "jpg"});
    endswitch
  endif

  if (! isfield (options, "maxHeight"))
    options.maxHeight = [];
  elseif ((! isscalar (options.maxHeight)) ...
          || (uint64 (options.maxHeight) == 0))
    error ("publish: MAXHEIGHT must be a positive integer");
  else
    options.maxHeight = uint64 (options.maxHeight);
  endif

  if (! isfield (options, "maxWidth"))
    options.maxWidth = [];
  elseif ((! isscalar (options.maxWidth)) ...
          || (uint64 (options.maxWidth) == 0))
    error ("publish: MAXWIDTH must be a positive integer");
  else
    options.maxWidth = uint64 (options.maxWidth);
  endif

  if (! isfield (options, "useNewFigure"))
    options.useNewFigure = true;
  elseif (! isbool (options.useNewFigure))
    error ("publish: USENEWFIGURE must be TRUE or FALSE");
  endif

  ## Options for the code
  if (!isfield (options, "evalCode"))
    options.evalCode = true;
  elseif ((! isscalar (options.evalCode)) || (! isbool (options.evalCode)))
    error ("publish: EVALCODE must be TRUE or FALSE");
  endif

  if (!isfield (options, "catchError"))
    options.catchError = true;
  elseif ((! isscalar (options.catchError)) || (! isbool (options.catchError)))
    error ("publish: CATCHERROR must be TRUE or FALSE");
  endif

  if (!isfield (options, "codeToEvaluate"))
    options.codeToEvaluate = "";
  elseif (! ischar (options.codeToEvaluate))
    error ("publish: CODETOEVALUTE must be a string");
  endif

  if (! isfield (options, "maxOutputLines"))
    options.maxOutputLines = Inf;
  elseif (! isscalar (options.maxOutputLines))
    error ("publish: MAXOUTPUTLINES must be an integer >= 0");
  else
    options.maxOutputLines = uint64 (options.maxOutputLines);
  endif

  if (!isfield (options, "showCode"))
    options.showCode = true;
  elseif ((! isscalar (options.showCode)) || (! isbool (options.showCode)))
    error ("publish: SHOWCODE must be TRUE or FALSE");
  endif

  doc_struct.title = "";
  doc_struct.intro = "";
  doc_struct.body = cell ();
  doc_struct.m_source = deblank (read_file_to_cellstr (file));
  doc_struct.m_source_file_name = file;

  ## Split code and paragraphs, find formatting
  doc_struct = parse_m_source (doc_struct);

  ## Create output directory
  [mkdir_stat, mkdir_msg] = mkdir (options.outputDir);
  if (mkdir_stat != 1)
    error ("publish: cannot create output directory. %s", mkdir_msg);
  endif

  if (options.evalCode)
    doc_struct = eval_code (doc_struct, options);
  endif

  output_file = create_output (doc_struct, options);
endfunction



function doc_struct = parse_m_source (doc_struct)
  ## PARSE_M_SOURCE First parsing level
  ##   This function extracts the overall structure (paragraphs and code
  ##   sections) given in doc_struct.m_source.
  ##
  ##   The result is written to doc_struct.body, which then contains a cell
  ##   vector of structs, either of
  ##
  ##     a) {struct ("type", "code", ...
  ##                 "lines", [a, b], ...
  ##                 "output", [])}
  ##     b) {struct ("type", "section", ...
  ##                 "content", title_str)}
  ##
  ##   Second parsing level is invoked for the paragraph contents, resulting
  ##   in more elements for doc_struct.body.
  ##

  ## If there is nothing to parse
  if (isempty (doc_struct.m_source))
    return;
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
  par_start_idx = find ( ...
    cellfun (is_head, doc_struct.m_source) ...
    | cellfun (is_no_break_head, doc_struct.m_source));

  ## If the whole document is code
  if (isempty (par_start_idx))
    doc_struct.body{end + 1}.type = "code";
    doc_struct.body{end}.content = strtrim (strjoin (...
      doc_struct.m_source(1:length(doc_struct.m_source)), "\n"));
    doc_struct.body{end}.lines = [1, length(doc_struct.m_source)];
    doc_struct.body{end}.output = {};
    return;
  endif

  ## Determine continuous range of paragraphs
  par_end_idx = [par_start_idx(2:end) - 1, length(doc_struct.m_source)];
  for i = 1:length(par_end_idx)
    idx = find (! cellfun (is_paragraph, ...
                    doc_struct.m_source(par_start_idx(i) + 1:par_end_idx(i))));
    if (! isempty (idx))
      par_end_idx(i) = par_start_idx(i) + idx(1) - 1;
    endif
  endfor
  ## Code sections between paragraphs
  code_start_idx = par_end_idx(1:end - 1) + 1;
  code_end_idx = par_start_idx(2:end) - 1;
  ## Code at the beginning?
  if (par_start_idx(1) > 1)
    code_start_idx = [1, code_start_idx];
    code_end_idx = [par_start_idx(1) - 1, code_end_idx];
  endif
  ## Code at the end?
  if (par_end_idx(end) < length (doc_struct.m_source))
    code_start_idx = [code_start_idx, par_end_idx(end) + 1];
    code_end_idx = [code_end_idx, length(doc_struct.m_source)];
  endif
  ## Remove overlaps
  idx = code_start_idx > code_end_idx;
  code_start_idx(idx) = [];
  code_end_idx(idx) = [];
  ## Remove empty code blocks
  idx = [];
  for i = 1:length(code_start_idx)
    if (all (cellfun (@(cstr) isempty (char (cstr)), ...
                      doc_struct.m_source(code_start_idx(i):code_end_idx(i)))))
      idx = [idx, i];
    endif
  endfor
  code_start_idx(idx) = [];
  code_end_idx(idx) = [];

  ## Try to find a document title and introduction text
  ##   1. First paragraph must start in first line
  ##   2. Second paragraph must start before any code
  title_offset = 0;
  if ((is_head (doc_struct.m_source{1})) ...
      && (! isempty (par_start_idx))
      && (par_start_idx(1) == 1) ...
      && ((isempty (code_start_idx))
          || ((length (par_start_idx) > 1)
              && (par_start_idx(2) < code_start_idx(1)))))
    doc_struct.title = doc_struct.m_source{1};
    doc_struct.title = doc_struct.title(4:end);
    content = doc_struct.m_source(2:par_end_idx(1));
    ## Strip leading "# "
    content = cellfun(@(c) cellstr (c(3:end)), content);
    doc_struct.intro = parse_paragraph_content (content);
    title_offset = 1;
  endif

  ## Add non-empty paragraphs and code to doc_struct
  j = 1;
  i = (1 + title_offset);
  while ((i <= length(par_start_idx)) || (j <= length(code_start_idx)))
    ## Add code while there is code left
    ##   and code is before the next paragraph or there are no more paragraphs
    while ((j <= length(code_start_idx))
           && ((i > length(par_start_idx))
               || (par_start_idx(i) > code_start_idx(j))))
      doc_struct.body{end + 1}.type = "code";
      lines = [code_start_idx(j), code_end_idx(j)];
      doc_struct.body{end}.content = strtrim (strjoin (...
        doc_struct.m_source(lines(1):lines(2)), "\n"));
      doc_struct.body{end}.lines = lines;
      doc_struct.body{end}.output = {};
      j++;
    endwhile

    if (i <= length(par_start_idx))
      type_str = "section";
      title_str = doc_struct.m_source{par_start_idx(i)};
      if (is_head (doc_struct.m_source(par_start_idx(i))))
        title_str = title_str(4:end);
      else
        title_str = title_str(5:end);
      endif
      ## Append, if paragraph title is given
      if (! isempty (title_str))
        doc_struct.body{end + 1}.type = type_str;
        doc_struct.body{end}.content = title_str;
      endif

      content = doc_struct.m_source(par_start_idx(i) + 1:par_end_idx(i));
      ## Strip leading "# "
      content = cellfun(@(c) cellstr (c(3:end)), content);
      doc_struct.body = [doc_struct.body, parse_paragraph_content(content)];
      i++;
    endif
  endwhile
endfunction



function [p_content] = parse_paragraph_content (content)
  ## PARSE_PARAGRAPH_CONTENT second parsing level
  ##
  ##   Parses the content of a paragraph (without potential title) and
  ##   returns a cell vector of structs, that can be appended to
  ##   doc_struct.body, either of
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
  ##

  p_content = cell ();

  if (isempty (content))
    return;
  endif

  ## Split into blocks seperated by empty lines
  idx = [0, find(cellfun (@isempty, content)), length(content) + 1];
  ## For each block
  for i = find (diff(idx) > 1)
    block = content(idx(i) + 1:idx(i+1) - 1);

    ## Octave code (two leading spaces)
    if (all (cellfun (@(c) strncmp (char (c), "  ", 2), block)))
      p_content{end+1}.type = "preformatted_code";
      block = cellfun(@(c) cellstr (c(3:end)), block);
      p_content{end}.content = strjoin (block, "\n");
      continue;
    endif

    ## Preformatted text (one leading space)
    if (all (cellfun (@(c) strncmp (char (c), " ", 1), block)))
      p_content{end+1}.type = "preformatted_text";
      block = cellfun(@(c) cellstr (c(2:end)), block);
      p_content{end}.content = strjoin (block, "\n");
      continue;
    endif

    ## Bulleted list starts with "* "
    if (strncmp (block{1}, "* ", 2))
      p_content{end+1}.type = "bulleted_list";
      p_content{end}.content = strjoin (block, "\n");
      ## Revove first "* "
      p_content{end}.content = p_content{end}.content(3:end);
      ## Split items
      p_content{end}.content = strsplit (p_content{end}.content, "\n* ");
      continue;
    endif

    ## Numbered list starts with "# "
    if (strncmp (block{1}, "# ", 2))
      p_content{end+1}.type = "numbered_list";
      p_content{end}.content = strjoin (block, "\n");
      ## Revove first "# "
      p_content{end}.content = p_content{end}.content(3:end);
      ## Split items
      p_content{end}.content = strsplit (p_content{end}.content, "\n# ");
      continue;
    endif

    ## Include <include>fname.m</include>
    if (! isempty ([~,~,~,~,fname] = regexpi (strjoin (block, ""), ...
                                              '^<include>(.*)<\/include>$')))
      ## Includes result in preformatted code
      p_content{end+1}.type = "preformatted_code";
      include_code = read_file_to_cellstr (strtrim ((fname{1}){1}));
      p_content{end}.content = strjoin (include_code, "\n");

      continue;
    endif

    ## Graphic <<myGraphic.png>>
    if (! isempty ([~,~,~,~,fname] = regexpi (strjoin (block, ""), ...
                                              '^<<(.*)>>$')))
      p_content{end+1}.type = "graphic";
      p_content{end}.content = strtrim ((fname{1}){1});
      continue;
    endif

    ## Parse remaining blocks line by line
    j = 1;
    while (j <= length(block))
      ## HTML markup
      if (strcmpi (block{j}, "<html>"))
        start_html = j + 1;
        while ((j < length(block)) && ! strcmpi (block{j}, "</html>"))
          j++;
        endwhile
        if ((j == length(block)) && ! strcmpi (block{j}, "</html>"))
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
        while ((j < length(block)) && ! strcmpi (block{j}, "</latex>"))
          j++;
        endwhile
        if ((j == length(block)) && ! strcmpi (block{j}, "</latex>"))
          warning ("publish: no closing </latex> found");
        else
          j++;  ## Skrip closing tag
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
      ##
      else
        if ((j == 1) || isempty (p_content) ...
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
  i = 1;
  m_source{i} = fgetl (fid);
  while (ischar (m_source{i}))
    i++;
    m_source{i} = fgetl (fid);
  endwhile
  fclose(fid);
  m_source = cellstr (m_source(1:end-1)); ## No EOL
endfunction



function ofile = create_output (doc_struct, options)
  ## CREATE_OUTPUT creates the desired output file
  ##

  formatter = [];
  ofile_ext = "";
  switch (options.format)
    case "html"
      formatter = @__publish_html_output__;
      ofile_ext = ".html";
    case {"latex", "pdf"}
      formatter = @__publish_latex_output__;
      ofile_ext = ".tex";
  endswitch

  ## Use title, or if not given the m-file name
  title_str = doc_struct.title;
  if (isempty (title_str))
    [~,title_str] = fileparts (doc_struct.m_source_file_name);
  endif

  content = formatter ("header", title_str, ...
    format_output (doc_struct.intro, formatter, options), ...
    get_toc (doc_struct.body));
  content = [content, format_output(doc_struct.body, formatter, options)];
  content = [content, formatter("footer", strjoin (doc_struct.m_source, "\n"))];

  ## Write file
  [~,ofile] = fileparts (doc_struct.m_source_file_name);
  ofile_name = [ofile, ofile_ext];
  ofile = [options.outputDir, filesep(), ofile_name];
  fid = fopen (ofile, "w");
  fputs (fid, content);
  fclose (fid);

  ## Compile LaTeX, if compiler found
  if (strcmp (options.format, "pdf"))
    [status, ~] = system ("pdflatex --version");
    if (status == 0)
      for i = 1:2
        system (["cd ", options.outputDir," && pdflatex ", ofile_name]);
      endfor
    endif
  endif
endfunction



function toc_cstr = get_toc (cstr)
  ## GET_TOC extracts the table of contents from a cellstring (e.g.
  ##   doc_struct.body) with each section headline as a cell in a returned
  ##   cellstring.
  ##
  toc_cstr = cell ();
  for i = 1:length(cstr)
    if (strcmp (cstr{i}.type, "section"))
      toc_cstr{end + 1} = cstr{i}.content;
    endif
  endfor
endfunction



function str = format_output (cstr, formatter, options)
  ## FORMAT_OUTPUT steps through all blocks (doc_struct.intro or
  ##   doc_struct.body) in cstr and produces a single result string
  ##   with the source code in the desired output format.
  ##
  ##   formatter has the only knowlegde how to enforce the target format
  ##   and produces for each block the necessary target format source string.
  ##

  str = "";
  for i = 1:length(cstr)
    switch (cstr{i}.type)
      case "code"
        if (options.showCode)
          str = [str, formatter(cstr{i}.type, cstr{i}.content)];
        endif
        if (options.evalCode)
          str = [str, formatter("code_output", cstr{i}.output)];
        endif
      case "text"
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
  ##

  ## Links "<http://www.someurl.com>"
  str = regexprep (str, '<(\S{3,}[^\s<>]*)>', ...
    formatter ("link", "$1", "$1"));
  ## Links "<octave:Function TEXT>"
  ## TODO: better pointer to the function documentation
  str = regexprep (str, '<octave:([^\s<>]*) *([^<>$]*)>', ...
    formatter ("link", ["https://www.gnu.org/software/octave/", ...
      "doc/interpreter/Function-Index.html"], "$2"));
  ## Links "<http://www.someurl.com TEXT>"
  str = regexprep (str, '<(\S{3,}[^\s<>]*) *([^<>$]*)>', ...
    formatter ("link", "$1", "$2"));
  oldstr = str;
  ## Save inline "$" and block "$$" LaTeX math
  [~,~,~,math_cstr] = regexp (str, '\${1,2}.*?\${1,2}');
  for i = 1:length(math_cstr)
    str = regexprep (str, '\${1,2}(.*?)\${1,2}', ...
      ["PUBLISHMATH", num2str(i)], "once");
  endfor
  ## Loop because of inlined expressions, e.g. *BOLD _ITALIC_*
  do
    oldstr = str;
    ## Bold
    str = regexprep (str, '\*([^*$_|]*)\*', formatter ("bold", "$1"));
    ## Italic
    str = regexprep (str, '_([^_$|*]*)_', formatter ("italic", "$1"));
    ## Monospaced
    str = regexprep (str, '\|([^|$_*]*)\|', formatter ("monospaced", "$1"));
  until (strcmp (str, oldstr))
  ## Restore inline "$" and block "$$" LaTeX math
  for i = length(math_cstr):-1:1
    str = strrep (str, ["PUBLISHMATH", num2str(i)], math_cstr{i});
  endfor
  ## Replace special symbols
  str = strrep (str, "(TM)", formatter("TM"));
  str = strrep (str, "(R)", formatter("R"));
endfunction



function doc_struct = eval_code (doc_struct, options)
  ## EVAL_CODE Thrid level parsing
  ##
  ##   Generates the output of the script code and takes care of generated
  ##   figures.
  ##

  ## Neccessary as the code does not run interactively
  page_screen_output (0, "local");

  ## Remember previously opened figures
  fig_ids = findall (0, "type", "figure");
  [~,fig_name] = fileparts (doc_struct.m_source_file_name);
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

  for i = 1:length(doc_struct.body)
    if (strcmp (doc_struct.body{i}.type, "code"))
      r = doc_struct.body{i}.lines;
      code_str = strjoin (doc_struct.m_source(r(1):r(2)), "\n");
      if (options.catchError)
        try
          doc_struct.body{i}.output = eval_code_helper (tmp_context, code_str);
         catch err
          doc_struct.body{i}.output = cellstr (["error: ", err.message, ...
            "\n\tin:\n\n", code_str]);
        end_try_catch
      else
        doc_struct.body{i}.output = eval_code_helper (tmp_context, code_str);
      endif

      ## Check for newly created figures ...
      fig_ids_new = setdiff (findall (0, "type", "figure"), fig_ids);
      ## ... and save them
      for j = 1:length(fig_ids_new)
        drawnow ();
        if (isempty (get (fig_ids_new(j), "children")))
          continue;
        endif
        fname = [fig_name, "-", num2str(fig_num), "."];
        if (strncmp (options.imageFormat, "eps", 3))
          fname = [fname, "eps"];
        else
          fname = [fname, options.imageFormat];
        endif
        print_opts = {fig_ids_new(j), ...
          [options.outputDir, filesep(), fname], ...
          ["-d", options.imageFormat], "-color"};
        if (! isempty (options.maxWidth) && ! isempty (options.maxHeight))
          print_opts{end + 1} = ["-S", num2str(options.maxWidth), ...
            num2str(options.maxHeight)];
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
          fig_elem = [getfield(fig_list, num2str (i)), fig_elem];
        endif
        fig_list = setfield (fig_list, num2str (i), fig_elem);
        ## Create a new figure, if there are existing plots
        if (isempty (setdiff (findall (0, "type", "figure"), fig_ids)) ...
            &&! isempty (fig_ids) && options.useNewFigure)
          figure ();
        endif
      endfor

      ## Truncate output to desired length
      if (options.maxOutputLines < length (doc_struct.body{i}.output))
        doc_struct.body{i}.output = ...
          doc_struct.body{i}.output(1:options.maxOutputLines);
      endif
      doc_struct.body{i}.output = strjoin (doc_struct.body{i}.output, "\n");
    endif
  endfor

  ## Close any by publish opened figures
  delete (setdiff (findall (0, "type", "figure"), fig_ids));

  ## Remove temporary context
  unlink (tmp_context);

  ## Insert figures to document
  fig_code_blocks = fieldnames (fig_list);
  body_offset = 0;
  for i = 1:length(fig_code_blocks)
    elems = getfield (fig_list, fig_code_blocks{i});
    ## Compute index, where the figure(s) has to be inserterd
    j = str2num (fig_code_blocks{i}) + body_offset;
    doc_struct.body = [doc_struct.body(1:j), elems, doc_struct.body(j+1:end)];
    body_offset = body_offset + length (elems);
  endfor
endfunction



function ___cstr___ = eval_code_helper (___context___, ___code___);
  ## EVAL_CODE_HELPER evaluates a given string with Octave code in an extra
  ##   temporary context and returns a cellstring with the eval output

  ## TODO: potential conflicting variables sourrounded by "___".  Maybe there
  ##       is a better solution.
  if (isempty (___code___))
    return;
  endif

  if (exist (___context___, "file") == 2)
    load (___context___);
  endif

  ___cstr___ = strsplit (evalc (___code___), "\n");

  clear ___code___
  save (___context___);
endfunction



## Bad function calls

%!error publish ()
%!error publish (1)
%!error publish ("non_existing_file.m")
%!error<Only Octave script files can be published> publish ("publish.m")
%!error publish ("test_script.m", "format", "html", "showCode")
%!error [str1, str2] = publish ("test_script.m")
