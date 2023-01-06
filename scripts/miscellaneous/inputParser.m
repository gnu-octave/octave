########################################################################
##
## Copyright (C) 2011-2023 The Octave Project Developers
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

classdef inputParser < handle

  ## -*- texinfo -*-
  ## @deftypefn {} {@var{p} =} inputParser ()
  ## Create object @var{p} of the inputParser class.
  ##
  ## This class is designed to allow easy parsing of function arguments.  The
  ## class supports four types of arguments:
  ##
  ## @enumerate
  ## @item mandatory (see @code{addRequired});
  ##
  ## @item optional (see @code{addOptional});
  ##
  ## @item named (see @code{addParameter});
  ##
  ## @item switch (see @code{addSwitch}).
  ## @end enumerate
  ##
  ## After defining the function API with these methods, the supplied arguments
  ## can be parsed with the @code{parse} method and the parsing results
  ## accessed with the @code{Results} accessor.
  ## @end deftypefn
  ##
  ## @deftypefn {} {} inputParser.Parameters
  ## Return list of parameter names already defined.
  ## @end deftypefn
  ##
  ## @deftypefn {} {} inputParser.Results
  ## Return structure with argument names as fieldnames and corresponding
  ## values.
  ## @end deftypefn
  ##
  ## @deftypefn {} {} inputParser.Unmatched
  ## Return structure similar to @code{Results}, but for unmatched parameters.
  ## See the @code{KeepUnmatched} property.
  ## @end deftypefn
  ##
  ## @deftypefn {} {} inputParser.UsingDefaults
  ## Return cell array with the names of arguments that are using default
  ## values.
  ## @end deftypefn
  ##
  ## @deftypefn {} {} inputParser.CaseSensitive = @var{boolean}
  ## Set whether matching of argument names should be case sensitive.  Defaults
  ## to false.
  ## @end deftypefn
  ##
  ## @deftypefn {} {} inputParser.FunctionName = @var{name}
  ## Set function name to be used in error messages; Defaults to empty string.
  ## @end deftypefn
  ##
  ## @deftypefn {} {} inputParser.KeepUnmatched = @var{boolean}
  ## Set whether an error should be given for non-defined arguments.  Defaults
  ## to false.  If set to true, the extra arguments can be accessed through
  ## @code{Unmatched} after the @code{parse} method.  Note that since
  ## @code{Switch} and @code{Parameter} arguments can be mixed, it is
  ## not possible to know the unmatched type.  If argument is found unmatched
  ## it is assumed to be of the @code{Parameter} type and it is expected to
  ## be followed by a value.
  ## @end deftypefn
  ##
  ## @deftypefn {} {} inputParser.StructExpand = @var{boolean}
  ## Set whether a structure can be passed to the function instead of
  ## parameter/value pairs.  Defaults to true.
  ##
  ## The following example shows how to use this class:
  ##
  ## @example
  ## function check (varargin)
  ## @c The next two comments need to be indented by one for alignment
  ##   p = inputParser ();                      # create object
  ##   p.FunctionName = "check";                # set function name
  ##   p.addRequired ("pack", @@ischar);         # mandatory argument
  ##   p.addOptional ("path", pwd(), @@ischar);  # optional argument
  ##
  ##   ## create a function handle to anonymous functions for validators
  ##   val_mat = @@(x) isvector (x) && all (x <= 1) && all (x >= 0);
  ##   p.addOptional ("mat", [0 0], val_mat);
  ##
  ##   ## create two arguments of type "Parameter"
  ##   val_type = @@(x) any (strcmp (x, @{"linear", "quadratic"@}));
  ##   p.addParameter ("type", "linear", val_type);
  ##   val_verb = @@(x) any (strcmp (x, @{"low", "medium", "high"@}));
  ##   p.addParameter ("tolerance", "low", val_verb);
  ##
  ##   ## create a switch type of argument
  ##   p.addSwitch ("verbose");
  ##
  ##   p.parse (varargin@{:@});  # Run created parser on inputs
  ##
  ##   ## the rest of the function can access inputs by using p.Results.
  ##   ## for example, get the tolerance input with p.Results.tolerance
  ## endfunction
  ## @end example
  ##
  ## @example
  ## @group
  ## check ("mech");           # valid, use defaults for other arguments
  ## check ();                 # error, one argument is mandatory
  ## check (1);                # error, since ! ischar
  ## check ("mech", "~/dev");  # valid, use defaults for other arguments
  ##
  ## check ("mech", "~/dev", [0 1 0 0], "type", "linear");  # valid
  ##
  ## ## following is also valid.  Note how the Switch argument type can
  ## ## be mixed into or before the Parameter argument type (but it
  ## ## must still appear after any Optional argument).
  ## check ("mech", "~/dev", [0 1 0 0], "verbose", "tolerance", "high");
  ##
  ## ## following returns an error since not all optional arguments,
  ## ## 'path' and 'mat', were given before the named argument 'type'.
  ## check ("mech", "~/dev", "type", "linear");
  ## @end group
  ## @end example
  ##
  ## @emph{Note 1}: A function can have any mixture of the four API types but
  ## they must appear in a specific order.  @code{Required} arguments must be
  ## first and can be followed by any @code{Optional} arguments.  Only
  ## the @code{Parameter} and @code{Switch} arguments may be mixed
  ## together and they must appear at the end.
  ##
  ## @emph{Note 2}: If both @code{Optional} and @code{Parameter} arguments
  ## are mixed in a function API then once a string Optional argument fails to
  ## validate it will be considered the end of the @code{Optional}
  ## arguments.  The remaining arguments will be compared against any
  ## @code{Parameter} or @code{Switch} arguments.
  ##
  ## @seealso{nargin, validateattributes, validatestring, varargin}
  ## @end deftypefn

  properties
    ## FIXME: set input checking for these properties
    CaseSensitive = false;
    FunctionName  = "";
    KeepUnmatched = false;
    PartialMatching = false; # FIXME: unimplemented (and default should be true)
    StructExpand    = true;
  endproperties

  properties (SetAccess = protected)
    Parameters    = cell ();
    Results       = struct ();
    Unmatched     = struct ();
    UsingDefaults = cell ();
  endproperties

  properties (Access = protected)
    ## Since Required and Optional are ordered, they get a cell array of
    ## structs with the fields "name", "def" (default), and "val" (validator).
    Required = cell ();
    Optional = cell ();
    ## Parameter and Switch are unordered so we have a struct whose fieldnames
    ## are the argname, and values are a struct with fields "def" and "val"
    Parameter = struct ();
    Switch    = struct ();

    ## List of Parameter and Switch names to ease searches
    ParameterNames = cell ();
    SwitchNames    = cell ();

    ## When checking for fieldnames in a Case Insensitive way, this variable
    ## holds the correct identifier for the last searched named using the
    ## is_argname method.
    last_name = "";
  endproperties

  properties (Access = protected, Constant = true)
    ## Default validator, always returns scalar true.
    def_val = @(~) true;
  endproperties

  methods

    function set.PartialMatching (this, val)
      if (val)
        error ("inputParser: PartialMatching is not yet implemented");
      endif
    endfunction

    function addRequired (this, name, val = inputParser.def_val)

      ## -*- texinfo -*-
      ## @deftypefn  {} {} addRequired (@var{argname})
      ## @deftypefnx {} {} addRequired (@var{argname}, @var{validator})
      ## Add new mandatory argument to the object @var{parser} of inputParser
      ## class.  This method belongs to the inputParser class and implements
      ## an ordered arguments type of API.
      ##
      ## @var{argname} must be a string with the name of the new argument.  The
      ## order in which new arguments are added with @code{addrequired},
      ## represents the expected order of arguments.
      ##
      ## @var{validator} is an optional function handle to validate the given
      ## values for the argument with name @var{argname}.  Alternatively, a
      ## function name can be used.
      ##
      ## See @code{help inputParser} for examples.
      ##
      ## @emph{Note}: this can be used together with the other type of
      ## arguments but it must be the first (see @code{@@inputParser}).
      ##
      ## @end deftypefn

      if (nargin < 2)
        print_usage ();
      elseif (numel (this.Optional) || numfields (this.Parameter)
              || numfields (this.Switch))
        error (["inputParser.addRequired: can't have a Required argument " ...
                "after Optional, Parameter, or Switch"]);
      endif
      this.validate_name ("Required", name);
      this.Required{end+1} = struct ("name", name, "val", val);

    endfunction

    function addOptional (this, name, def, val = inputParser.def_val)

      ## -*- texinfo -*-
      ## @deftypefn  {} {} addOptional (@var{argname}, @var{default})
      ## @deftypefnx {} {} addOptional (@var{argname}, @var{default}, @var{validator})
      ## Add new optional argument to the object @var{parser} of the class
      ## inputParser to implement an ordered arguments type of API
      ##
      ## @var{argname} must be a string with the name of the new argument.  The
      ## order in which new arguments are added with @code{addOptional},
      ## represents the expected order of arguments.
      ##
      ## @var{default} will be the value used when the argument is not
      ## specified.
      ##
      ## @var{validator} is an optional anonymous function to validate the
      ## given values for the argument with name @var{argname}.  Alternatively,
      ## a function name can be used.
      ##
      ## See @code{help inputParser} for examples.
      ##
      ## @emph{Note}: if a string argument does not validate, it will be
      ## considered a ParamValue key.  If an optional argument is not given a
      ## validator, anything will be valid, and so any string will be
      ## considered will be the value of the optional argument (in @sc{matlab},
      ## if no validator is given and argument is a string it will also be
      ## considered a ParamValue key).
      ##
      ## @end deftypefn

      if (nargin < 3)
        print_usage ();
      elseif (numfields (this.Parameter) || numfields (this.Switch))
        error (["inputParser.Optional: can't have Optional arguments " ...
                "after Parameter or Switch"]);
      endif
      this.validate_name ("Optional", name);
      if (iscell (def))
        def = {def};
      endif
      this.Optional{end+1} = struct ("name", name, "def", def, "val", val);

    endfunction

    function addParamValue (this, name, def, val = inputParser.def_val)

      ## -*- texinfo -*-
      ## @deftypefn  {} {} addParamValue (@var{argname}, @var{default})
      ## @deftypefnx {} {} addParamValue (@var{argname}, @var{default}, @var{validator})
      ## Add new parameter to the object @var{parser} of the class inputParser
      ## to implement a name/value pair type of API.
      ##
      ## This is an alias for @code{addParameter} method without the
      ## @qcode{"PartialMatchPriority"} option.  See it for the help text.
      ##
      ## @end deftypefn

      if (nargin < 3)
        print_usage ();
      endif
      this.addParameter (name, def, val);

    endfunction

    function addParameter (this, name, def, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {} addParameter (@var{argname}, @var{default})
      ## @deftypefnx {} {} addParameter (@var{argname}, @var{default}, @var{validator})
      ## Add new parameter to the object @var{parser} of the class inputParser
      ## to implement a name/value pair type of API.
      ##
      ## @var{argname} must be a string with the name of the new parameter.
      ##
      ## @var{default} will be the value used when the parameter is not
      ## specified.
      ##
      ## @var{validator} is an optional function handle to validate the given
      ## values for the parameter with name @var{argname}.  Alternatively, a
      ## function name can be used.
      ##
      ## See @code{help inputParser} for examples.
      ##
      ## @end deftypefn

      if (nargin < 3 || nargin > 6)
        print_usage ();
      endif

      n_opt = numel (varargin);

      if (n_opt == 0 || n_opt == 2)
        val = inputParser.def_val;
      else # n_opt is 1 or 3
        val = varargin{1};
      endif

      if (n_opt == 0 || n_opt == 1)
        match_priority = 1;
      else # n_opt is 2 or 3
        if (! strcmpi (varargin{end-1}, "PartialMatchPriority"))
          error ("inputParser.addParameter: unrecognized option");
        endif
        match_priority = varargin{end};
        validateattributes (match_priority, {"numeric"}, {"positive", "integer"},
                            "inputParser.addParameter",
                            "PartialMatchPriority");
      endif

      this.validate_name ("Parameter", name);
      this.Parameter.(name).def = def;
      this.Parameter.(name).val = val;

    endfunction

    function addSwitch (this, name)

      ## -*- texinfo -*-
      ## @deftypefn {} {} addSwitch (@var{argname})
      ## Add new switch type of argument to the object @var{parser} of
      ## inputParser class.
      ##
      ## This method belongs to the inputParser class and implements a switch
      ## arguments type of API.
      ##
      ## @var{argname} must be a string with the name of the new argument.
      ## Arguments of this type can be specified at the end, after
      ## @code{Required} and @code{Optional}, and mixed between the
      ## @code{Parameter}.  They default to false.  If one of the arguments
      ## supplied is a string like @var{argname}, then after parsing the value
      ## of @var{parse}.Results.@var{argname} will be true.
      ##
      ## See @code{help inputParser} for examples.
      ##
      ## @end deftypefn

      if (nargin != 2)
        print_usage ();
      endif
      this.validate_name ("Switch", name);
      this.Switch.(name).def = false;

    endfunction

    function parse (this, varargin)

      ## -*- texinfo -*-
      ## @deftypefn {} {} parse (@var{varargin})
      ## Parses and validates list of arguments according to object
      ## @var{parser} of the class inputParser.
      ##
      ## After parsing, the results can be accessed with the @code{Results}
      ## accessor.  See @code{help inputParser} for a more complete
      ## description.
      ##
      ## @end deftypefn

      this.Results = struct ();
      this.Unmatched = struct ();
      this.UsingDefaults = cell ();
      if (numel (varargin) < numel (this.Required))
        if (this.FunctionName)
          print_usage (this.FunctionName);
        else
          this.error ("inputParser.parse: not enough input arguments");
        endif
      endif
      pnargin = numel (varargin);

      this.ParameterNames = fieldnames (this.Parameter);
      this.SwitchNames    = fieldnames (this.Switch);

      ## Evaluate the Required arguments first
      nReq = numel (this.Required);
      for idx = 1:nReq
        req = this.Required{idx};
        this.validate_arg (req.name, req.val, varargin{idx});
      endfor

      vidx = nReq;  # current index in varargin

      ## Search for a list of Optional arguments
      idx  = 0;     # current index on the array of Optional
      nOpt = numel (this.Optional);
      while (vidx < pnargin && idx < nOpt)
        opt = this.Optional{++idx};
        in  = varargin{++vidx};
        if ((this.is_argname ("Parameter", in) && vidx < pnargin)
            || this.is_argname ("Switch", in))
          ## This looks like an optional parameter/value pair or a
          ## switch, not an positional option.  This does mean that
          ## positional options cannot be strings named like parameter
          ## keys.  See bug #50752.
          idx -= 1;
          vidx -= 1;
          break;
        endif
        try
          valid_option = opt.val (in);
        catch
          valid_option = false;
        end_try_catch
        if (! valid_option)
          ## If it does not match there's two options:
          ##    1) input is actually wrong and we should error;
          ##    2) it's a Parameter or Switch name and we should use
          ##       the default for the rest;
          ##    3) it's a struct with the Parameter pairs.
          if (ischar (in) || (this.StructExpand && isstruct (in)
                              && isscalar (in)))
            idx -= 1;
            vidx -= 1;
            break;
          else
            this.error (sprintf (["failed validation of %s\n", ...
                                  "Validation function: %s"],
                                 toupper (opt.name), disp (opt.val)));
          endif
        endif
        this.Results.(opt.name) = in;
      endwhile

      ## Fill in with defaults of missing Optional
      while (idx++ < nOpt)
        opt = this.Optional{idx};
        this.UsingDefaults{end+1} = opt.name;
        this.Results.(opt.name) = opt.def;
      endwhile

      ## Search unordered Options (Switch and Parameter)
      while (vidx++ < pnargin)
        name = varargin{vidx};

        if (this.StructExpand && isstruct (name) && isscalar (name))
          expanded_options = [fieldnames(name) struct2cell(name)]'(:);
          if (isempty (expanded_options))
            continue;  # empty, continue to next argument
          endif
          n_new_args = numel (expanded_options) -1;
          pnargin += n_new_args;
          varargin(vidx+n_new_args+1:pnargin) = varargin(vidx+1:end);
          varargin(vidx:vidx+n_new_args) = expanded_options;
          name = varargin{vidx};
        endif

        if (! ischar (name))
          this.error ("non-string for Parameter name or Switch");
        endif

        if (this.is_argname ("Parameter", name))
          if (vidx++ > pnargin)
            this.error (sprintf ("no matching value for option '%s'",
                                 toupper (name)));
          endif
          this.validate_arg (this.last_name,
                             this.Parameter.(this.last_name).val,
                             varargin{vidx});
        elseif (this.is_argname ("Switch", name))
          this.Results.(this.last_name) = true;
        else
          if (vidx++ < pnargin && this.KeepUnmatched)
            this.Unmatched.(name) = varargin{vidx};
          else
            this.error (sprintf ("argument '%s' is not a valid parameter",
                                 toupper (name)));
          endif
        endif
      endwhile
      ## Add them to the UsingDefaults list
      this.add_missing ("Parameter");
      this.add_missing ("Switch");

    endfunction

    function disp (this)

      if (nargin != 1)
        print_usage ();
      endif
      printf ("inputParser object with properties:\n\n");
      b2s = @(x) ifelse (any (x), "true", "false");
      printf (["   CaseSensitive   : %s\n   FunctionName    : %s\n" ...
               "   KeepUnmatched   : %s\n   PartialMatching : %s\n" ...
               "   StructExpand    : %s\n\n"],
               b2s (this.CaseSensitive), b2s (this.FunctionName),
               b2s (this.KeepUnmatched), b2s (this.PartialMatching),
               b2s (this.StructExpand));
      printf ("Defined parameters:\n\n   {%s}\n",
              strjoin (this.Parameters, ", "));

    endfunction

  endmethods

  methods (Access = private)

    function validate_name (this, type, name)

      if (! isvarname (name))
        error ("inputParser.add%s: NAME is an invalid identifier", method);
      elseif (any (strcmpi (this.Parameters, name)))
        ## Even if CaseSensitive is "on", we still shouldn't allow
        ## two args with the same name.
        error ("inputParser.add%s: argname '%s' has already been specified",
               type, name);
      endif
      this.Parameters{end+1} = name;

    endfunction

    function validate_arg (this, name, val, in)

      ## Checking "nargout (val)" doesn't work for builtin functions.
      ## So, we need to use this nested try-catch construct.
      err = sprintf ('Checked with "%s"', func2str (val));
      try
        ok = val (in);
      catch exception
        if (strcmp (exception.identifier, "Octave:invalid-fun-call"))
          ## check if function also fails when called without output argument
          try
            val (in);
            ok = true;
          catch exception
            ok = false;
            err = exception.message;
          end_try_catch
        else
          ok = false;
          err = exception.message;
        endif
      end_try_catch
      if (! ok)
        this.error (sprintf ("failed validation of %s. %s",
                             toupper (name), err));
      endif
      this.Results.(name) = in;

    endfunction

    function r = is_argname (this, type, name)

      r = ischar (name) && isrow (name);
      if (r)
        if (this.CaseSensitive)
          r = isfield (this.(type), name);
          if (r)
            this.last_name = name;
          endif
        else
          fnames = this.([type "Names"]);
          l = strcmpi (name, fnames);
          r = any (l(:));
          if (r)
            this.last_name = fnames{l};
          endif
        endif
      endif

    endfunction

    function add_missing (this, type)

      unmatched = setdiff (fieldnames (this.(type)), fieldnames (this.Results));
      for namec = unmatched(:)'
        name = namec{1};
        this.UsingDefaults{end+1} = name;
        this.Results.(name) = this.(type).(name).def;
      endfor

    endfunction

    function error (this, msg)

      where = "";
      if (this.FunctionName)
        where = [this.FunctionName ": "];
      endif
      error ("%s%s", where, msg);

    endfunction

  endmethods

endclassdef


%!function p = create_p ()
%!  p = inputParser ();
%!  p.CaseSensitive = true;
%!  p.addRequired ("req1", @(x) ischar (x));
%!  p.addOptional ("op1", "val", @(x) any (strcmp (x, {"val", "foo"})));
%!  p.addOptional ("op2", 78, @(x) x > 50);
%!  p.addSwitch ("verbose");
%!  p.addParameter ("line", "tree", @(x) any (strcmp (x, {"tree", "circle"})));
%!endfunction

## check normal use, only required are given
%!test
%! p = create_p ();
%! p.parse ("file");
%! r = p.Results;
%! assert (r.req1, "file");
%! assert (sort (p.UsingDefaults), sort ({"op1", "op2", "verbose", "line"}));
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "val", 78,    false,     "tree"});

## check normal use, but give values different than defaults
%!test
%! p = create_p ();
%! p.parse ("file", "foo", 80, "line", "circle", "verbose");
%! r = p.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "foo", 80,    true,      "circle"});

## check optional is skipped and considered Parameter if unvalidated string
%!test
%! p = create_p ();
%! p.parse ("file", "line", "circle");
%! r = p.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "val", 78,    false,     "circle"});

## check case insensitivity
%!test
%! p = create_p ();
%!  p.CaseSensitive = false;
%! p.parse ("file", "foo", 80, "LiNE", "circle", "vERbOSe");
%! r = p.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "foo", 80,    true,      "circle"});

## check KeepUnmatched
%!test
%! p = create_p ();
%! p.KeepUnmatched = true;
%! p.parse ("file", "foo", 80, "line", "circle", "verbose", "extra", 50);
%! assert (p.Unmatched.extra, 50);

## check error when missing required
%!error <not enough input arguments>
%! p = create_p ();
%! p.parse ();

## check error when given required does not validate
%!error <failed validation of >
%! p = create_p ();
%! p.parse (50);

## check error when given optional does not validate
%!error <is not a valid parameter>
%! p = create_p ();
%! p.parse ("file", "no-val");

## check error when given Parameter does not validate
%!error <failed validation of >
%! p = create_p ();
%! p.parse ("file", "foo", 51, "line", "round");

## check alternative method (obj, ...) API
%!function p2 = create_p2 ();
%!  p2 = inputParser ();
%!  addRequired (p2, "req1", @(x) ischar (x));
%!  addOptional (p2, "op1", "val", @(x) any (strcmp (x, {"val", "foo"})));
%!  addOptional (p2, "op2", 78, @(x) x > 50);
%!  addSwitch (p2, "verbose");
%!  addParameter (p2, "line", "tree", @(x) any (strcmp (x, {"tree", "circle"})));
%!endfunction

## check normal use, only required are given
%!test
%! p2 = create_p2 ();
%! parse (p2, "file");
%! r = p2.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "val", 78,    false,     "tree"});
%! assert (sort (p2.UsingDefaults), sort ({"op1", "op2", "verbose", "line"}));

## check normal use, but give values different than defaults
%!test
%! p2 = create_p2 ();
%! parse (p2, "file", "foo", 80, "line", "circle", "verbose");
%! r = p2.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "foo", 80,    true,      "circle"});

## We must not perform validation of default values
%!test <*45837>
%! p = inputParser ();
%! p.addParameter ("Dir", [], @ischar);
%! p.parse ();
%! assert (p.Results.Dir, []);

%!test
%! p = inputParser ();
%! p.addParameter ("positive", -1, @(x) x > 5);
%! p.parse ();
%! assert (p.Results.positive, -1);

## Throw an error on validation of optional argument to check that it
## is caught without preventing continuation into param/value pairs.
%!test
%! p = inputParser ();
%! p.addOptional ("err", "foo", @error);
%! p.addParameter ("not_err", "bar", @ischar);
%! p.parse ("not_err", "qux");
%! assert (p.Results.err, "foo");
%! assert (p.Results.not_err, "qux");

## With more Parameters to test StructExpand
%!function p3 = create_p3 ();
%!  p3 = inputParser ();
%!  addOptional (p3, "op1", "val", @(x) any (strcmp (x, {"val", "foo"})));
%!  addOptional (p3, "op2", 78, @(x) x > 50);
%!  addSwitch (p3, "verbose");
%!  addParameter (p3, "line", "tree", @(x) any (strcmp (x, {"tree", "circle"})));
%!  addParameter (p3, "color", "red", @(x) any (strcmp (x, {"red", "green"})));
%!  addParameter (p3, "style", "tt", @(x) any (strcmp (x, {"tt", "f", "i"})));
%!endfunction

## Test StructExpand
%!test
%! p3 = create_p3 ();
%! p3.parse (struct ("line", "circle", "color", "green"));
%! assert (p3.Results, struct ("op1", "val", "op2", 78, "verbose", false,
%!                             "line", "circle", "color", "green",
%!                             "style", "tt"))

%!test
%! p3 = create_p3 ();
%! p3.parse (struct ("line", "circle", "color", "green"), "line", "tree");
%! assert (p3.Results.line, "tree");
%! p3.parse ("line", "tree", struct ("line", "circle", "color", "green"));
%! assert (p3.Results.line, "circle");

%!test # unmatched parameters with StructExpand
%! p3 = create_p3 ();
%! p3.KeepUnmatched = true;
%! p3.parse (struct ("line", "circle", "color", "green", "bar", "baz"));
%! assert (p3.Unmatched.bar, "baz");

## The validation for the second optional argument throws an error with
## a struct so check that we can handle it.
%!test
%! p3 = create_p3 ();
%! p3.parse ("foo", struct ("color", "green"), "line", "tree");
%! assert (p3.Results.op1, "foo");
%! assert (p3.Results.line, "tree");
%! assert (p3.Results.color, "green");
%! assert (p3.Results.verbose, false);


## Some simple tests for addParamValue since all the other ones use add
## addParameter but they use the same codepath.
%!test
%! p = inputParser ();
%! addParameter (p, "line", "tree", @(x) any (strcmp (x, {"tree", "circle"})));
%! addParameter (p, "color", "red", @(x) any (strcmp (x, {"red", "green"})));
%! p.parse ("line", "circle");
%! assert ({p.Results.line, p.Results.color}, {"circle", "red"});

%!test
%! p = inputParser ();
%! p.addParameter ("foo", "bar", @ischar);
%! p.parse ();
%! assert (p.Results, struct ("foo", "bar"));
%! p.parse ("foo", "qux");
%! assert (p.Results, struct ("foo", "qux"));

## This behaviour means that a positional option can never be a string
## that is the name of a parameter key.  This is required for Matlab
## compatibility.
%!test <*50752>
%! p = inputParser ();
%! p.addOptional ("op1", "val");
%! p.addParameter ("line", "tree");
%! p.parse ("line", "circle");
%! assert (p.Results, struct ("op1", "val", "line", "circle"));
%!
%! p = inputParser ();
%! p.addOptional ("op1", "val1");
%! p.addOptional ("op2", "val2");
%! p.addParameter ("line", "tree");
%! p.parse ("line", "circle");
%! assert (p.Results.op1, "val1");
%! assert (p.Results.op2, "val2");
%! assert (p.Results.line, "circle");
%!
%! ## If there's enough arguments to fill the positional options and
%! ## param/key, it still skips positional options.
%! p = inputParser ();
%! p.addOptional ("op1", "val1");
%! p.addOptional ("op2", "val2");
%! p.addParameter ("line", "tree");
%! p.parse ("line", "circle", "line", "rectangle");
%! assert (p.Results, struct ("op1", "val1", "op2", "val2",
%!                            "line", "rectangle"))
%!
%! ## Even if the key/param fails validation, it does not backtrack to
%! ## check if the values are valid positional options.
%! p = inputParser ();
%! p.addOptional ("op1", "val1", @ischar);
%! p.addOptional ("op2", "val2", @isnumeric);
%! p.addParameter ("line", "circle", @ischar);
%! fail ('p.parse ("line", 89)', "failed validation of LINE")
%!
%! p = inputParser ();
%! p.addOptional ("op1", "val1");
%! p.addParamValue ("line", "circle", @ischar);
%! fail ('p.parse ("line", "line", 89)',
%!       "non-string for Parameter name or Switch")

%!test <*50752>
%! ## This fails in Matlab but works in Octave.  It is a bug there
%! ## that we do not replicate.
%! p = inputParser ();
%! p.addOptional ("op1", "val1");
%! p.addParameter ("line", "circle");
%! p.parse ("line");
%! assert (p.Results, struct ("op1", "line", "line", "circle"));

%!test <*50752>
%! p = inputParser ();
%! p.addOptional ("op1", "val1");
%! p.addSwitch ("line");
%! p.parse ("line");
%! assert (p.Results.op1, "val1");
%! assert (p.Results.line, true);

%!test
%! p = inputParser ();
%! p.addParameter ("a", []);
%! p.addParameter ("b", []);
%! p.parse ("a", 1);
%! p.parse ("b", 1);
%! assert (p.Results, struct ("a", [], "b", 1));
%! assert (p.UsingDefaults, {"a"});

%!test
%! p = inputParser ();
%! p.addParameter ("b", []);
%! p.KeepUnmatched = true;
%! p.parse ("a", 1);
%! p.parse ("b", 1);
%! assert (p.Results, struct ("b", 1));
%! assert (p.Unmatched, struct ());

## Test for patch #9241
%!error <failed validation of A.*ischar>
%! p = inputParser ();
%! p.addParameter ("a", [], @ischar);
%! p.parse ("a", 1);

%!test <*58112>
%! p = inputParser ();
%! p.addRequired ("first");
%! p.addOptional ("second", []);
%! p.parse (1, {"test", 1, 2, 3});
%! r = p.Results;
%! assert (r.first, 1);
%! assert (r.second, {"test", 1, 2, 3});

%!test <*62639>
%! p = inputParser ();
%! p.addOptional ("opt", {});
%! p.parse ();
%! r = p.Results;
%! assert (r.opt, {});
%! p.parse ("x");
%! r = p.Results;
%! assert (r.opt, "x");

%!test <*62639>
%! p = inputParser ();
%! p.addOptional ("opt", {1,2,3});
%! p.parse ();
%! r = p.Results;
%! assert (r.opt, {1,2,3});
%! p.parse ("x");
%! r = p.Results;
%! assert (r.opt, "x");
