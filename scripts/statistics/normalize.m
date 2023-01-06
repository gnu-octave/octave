########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{z} =} normalize (@var{x})
## @deftypefnx {} {@var{z} =} normalize (@var{x}, @var{dim})
## @deftypefnx {} {@var{z} =} normalize (@dots{}, @var{method})
## @deftypefnx {} {@var{z} =} normalize (@dots{}, @var{method}, @var{option})
## @deftypefnx {} {@var{z} =} normalize (@dots{}, @var{scale}, @var{scaleoption}, @var{center}, @var{centeroption})
## @deftypefnx {} {[@var{z}, @var{c}, @var{s}] =} normalize (@dots{})
##
## Return a normalization of the data in @var{x} using one of several available
## scaling and centering methods.
##
## @code{normalize} by default will return the @code{zscore} of @var{x},
## defined as the number of standard deviations each element is from the mean
## of @var{x}.  This is equivalent to centering at the mean of the data and
## scaling by the standard deviation.
##
## The returned value @var{z} will have the same size as @var{x}.  The optional
## return variables @var{c} and @var{s} are the centering and scaling factors
## used in the normalization such that:
##
## @example
## @group
##   @tcode{@var{z} = (@var{x} - @var{c}) ./ @var{s}}
## @end group
## @end example
##
## If @var{x} is a vector, @code{normalize} will operate on the data in
## @var{x}.
##
## If @var{x} is a matrix, @code{normalize} will operate independently on
## each column in @var{x}.
##
## If @var{x} is an N-dimensional array, @code{normalize} will operate
## independently on the first non-singleton dimension in @var{x}.
##
## If the optional second argument @var{dim} is given, operate along this
## dimension.
##
## The optional inputs @var{method} and @var{option} can be used to specify the
## type of normalization performed on @var{x}.  Note that only the
## @option{scale} and @option{center} options may be specified together using
## any of the methods defined below.  Valid normalization methods are:
##
## @table @code
## @item zscore
## (Default) Normalizes the elements in @var{x} to the scaled distance from a
##  central value.  Valid Options:
##
##    @table @code
##    @item std
##    (Default) Data is centered at @code{mean (@var{x})} and scaled by the
##      standard deviation.
##
##    @item robust
##    Data is centered at @code{median (@var{x})} and scaled by the median
##    absolute deviation.
##    @end table
##
## @item norm
## @var{z} is the general vector norm of @var{x}, with @var{option} being the
## normalization factor @var{p} that determines the vector norm type according
## to:
## @tex
## $$Z = \left (\sum_k \left | X_k \right |^P  \right )^{1/P}$$
## @end tex
## @ifnottex
##
## @example
## @group
##   @tcode{@var{z} = [sum (abs (@var{x}) .^ @var{p})] ^ (1/@var{p})}
## @end group
## @end example
##
## @end ifnottex
## @var{p} can be any positive scalar, specific values being:
##
##    @table @code
##    @item @var{p} = 1
##    @var{x} is normalized by @code{sum (abs (@var{x}))}.
##
##    @item @var{p} = 2
##    (Default) @var{x} is normalized by the Euclidian norm, or vector
##    magnitude, of the elements.
##
##    @item @var{P} = Inf
##    @var{x} is normalized by @code{max (abs (@var{x}))}.
##    @end table
##
## @item scale
## @var{x} is scaled by a factor determined by @var{option}, which can be a
## numeric scalar or one of the following:
##
##    @table @code
##    @item std
##    (Default) @var{x} is scaled by its standard deviation.
##
##    @item mad
##    @var{x} is scaled by its median absolute deviation.
##
##    @item first
##    @var{x} is scaled by its first element.
##
##    @item iqr
##    @var{x} is scaled by its interquartile range.
##    @end table
##
## @item range
## @var{x} is scaled to fit the range specified by @var{option} as a two
## element scalar row vector.  The default range is [0, 1].
##
## @item center
## @var{x} is shifted by an amount determined by @var{option}, which can be a
## numeric scalar or one of the following:
##
##    @table @code
##    @item mean
##    (Default) @var{x} is shifted by @code{mean (@var{x})}.
##
##    @item median
##    @var{x} is shifted by @code{median (@var{x})}.
##    @end table
##
## @item medianiqr
## @var{x} is shifted by @code{median (@var{x})} and scaled by the
## interquartile range.
## @end table
##
## Known @sc{matlab} incompatibilities:
##
## @enumerate
## @item
## The option @option{DataVariables} is not yet implemented for Table class
## @var{x} inputs.
##
## @item
## Certain arrays containing NaN elements may not return @sc{matlab} compatible
## output.
## @end enumerate
##
## @seealso{zscore, iqr, norm, rescale, std, median, mean, mad}
## @end deftypefn

function [z, c, s] = normalize (x, varargin)

  ## FIXME: Until NANFLAG/OMITNAN option is implemented in std, mean, median,
  ## etc., normalize cannot efficiently reproduce some behavior with NaNs in
  ## x.  xtests added to capture this.  (See bug #50571)

  ## FIXME: When table class is implemented, remove DataVariables error line in
  ## option checking section and add DataVariables data handling switch
  ## section.

  ## Input validation
  if (nargin < 1 || nargin > 8)
    print_usage ();
  endif

  if (! isnumeric (x))
    error ("normalize: X must be a numeric vector, matrix, or array");
  endif

  if (nargin == 1)
    ## Directly handle simple 1 input case.
    [s, c] = std (x);

  else
    ## Parse input options
    dim = [];
    method = [];
    methodoption = [];
    datavariables_flag = false;
    datavar = [];
    scale_and_center_flag = false;

    vararg_idx = 1;
    ## Only second optional input can be numeric without following a method.
    if (isnumeric (varargin{1}))
      dim = varargin{1};
      ## Check for valid dimensions
      if (! (isscalar (dim) && dim == fix (dim) && dim > 0))
        error ("normalize: DIM must be an integer and a valid dimension");
      endif
      vararg_idx++;
    endif

    ## Parse varargin to determine methods then options.
    n_varargin = nargin - 1;
    while (vararg_idx <= n_varargin)
      ## Arguments after second cannot be numeric without following a method.
      if (isnumeric (varargin{vararg_idx}))
        print_usage ();
      endif

      prop = tolower (varargin{vararg_idx});

      if (strcmp (prop, "datavariables"))
        ## FIXME: Remove error on next line and undo block comment when support
        ## for Tables is implemented.
        error ("normalize: DataVariables method not yet implemented");
        #{
        if (vararg_idx == n_varargin)
          error (["normalize: DataVariables requires a table variable", ...
                 " be specified"]);
        elseif (datavariables_flag == true)
          error ("normalize: DataVariables may only be specified once");
        else
          datavariables_flag = true;
          datavar = varargin{vararg_idx+1};
          vararg_idx++;
        endif
        #}

      else
        if (! isempty (method))
          ## Catch if a second method is passed
          if (scale_and_center_flag)
            ## if true, already specified two methods, three never possible
            error ("normalize: more than two methods specified");

          elseif (strcmp ({method, prop}, {"center", "scale"})
                  || strcmp ({method, prop}, {"scale", "center"}))
            ## Only scale and center can be called together
            scale_and_center_flag = true;
            ## scale/center order doesn't matter, avoid overwriting first one
            stored_method = method;
            method = [];
            stored_methodoption = methodoption;
            methodoption = [];
          else
            ## not scale and center, throw appropriate error
            if (any (strcmp (prop, {"zscore", "norm", "range", "scale", ...
                                    "center", "medianiqr"})))
              error ("normalize: methods '%s' and '%s' may not be combined",
                     method, prop);
            else
              error ("normalize: unknown method '%s'", prop);
            endif
          endif
        endif

        ## Determine method and whether there's an appropriate option specified
        switch (prop)
          case "zscore"
            method = "zscore";
            if (vararg_idx < n_varargin)
              nextprop = tolower (varargin{vararg_idx+1});
              if (strcmp (nextprop, "std") || strcmp (nextprop, "robust"))
                methodoption = nextprop;
                vararg_idx++;
              endif
            endif
            if (isempty (methodoption))
              methodoption = "std";
            endif

          case "norm"
            method = "norm";
            if (vararg_idx < n_varargin && isnumeric (varargin{vararg_idx+1}))
              nextprop = varargin{vararg_idx+1};
              if (isscalar (nextprop) && (nextprop > 0))
                methodoption = nextprop;
                vararg_idx++;
              else
                error (["normalize: 'norm' option must be a positive ", ...
                        "scalar or Inf"]);
              endif
            endif
            if (isempty (methodoption))
              methodoption = 2;
            endif

          case "range"
            method = "range";
            if (vararg_idx < n_varargin && isnumeric (varargin{vararg_idx+1}))
              nextprop = varargin{vararg_idx+1};
              if (any (size (nextprop) != [1 2]))
                error (["normalize: 'range' must be specified as a ", ...
                        "2-element row vector [a, b]"]);
              endif
              methodoption = nextprop;
              vararg_idx++;
            endif
            if (isempty (methodoption))
              methodoption = [0, 1];
            endif

          case "scale"
            method = "scale";
            if (vararg_idx < n_varargin)
              nextprop = tolower (varargin{vararg_idx+1});
              if (isnumeric (nextprop))
                if (! isscalar (nextprop))
                  error ("normalize: scale value must be a scalar");
                else
                  methodoption = nextprop;
                  vararg_idx++;
                endif
              elseif (any (strcmp (nextprop, {"std", "mad", "first", "iqr"})))
                methodoption = nextprop;
                vararg_idx++;
              endif
            endif

            if (isempty (methodoption))
              methodoption = 'std';
            endif

          case "center"
            method = "center";
            if (vararg_idx < n_varargin)
              nextprop = tolower (varargin{vararg_idx+1});
              if (isscalar (nextprop)
                  || any (strcmp (nextprop, {"mean", "median"})))
                methodoption = nextprop;
                vararg_idx++;
              elseif (isnumeric (nextprop))
                error ("normalize: center shift must be a scalar value");
              endif
            endif
            if (isempty (methodoption))
              methodoption = 'mean';
            endif

          case "medianiqr"
            method = "medianiqr";

          otherwise
            error ("normalize: unknown method '%s'", prop);

        endswitch
      endif

      vararg_idx++;
    endwhile

    if (scale_and_center_flag)
      method = "scaleandcenter";
    endif

    if (isempty (method))
      method = 'zscore';
      methodoption = 'std';
    endif

    if (isempty (dim))
      ## Operate on first non-singleton dimension.
      (dim = find (size (x) > 1, 1)) || (dim = 1);
    endif

    ## Perform normalization based on specified methods

    ## FIXME: DataTables option not handled below.  Fix after Table Class
    ## has been implemented.

    ## Default center/scale factors:
    c = 0;
    s = 1;

    switch (method)
      case "zscore"
        switch (methodoption)
          case "std"
            [s, c] = std (x, [], dim);
          case "robust"
            ## center/median to zero and MAD = 1
            c = median (x, dim);
            ## FIXME: Use bsxfun, rather than broadcasting, until broadcasting
            ##        supports diagonal and sparse matrices (Bugs #41441, #35787).
            s = median (abs (bsxfun (@minus, x , c)), dim);
            ## s = median (abs (x - c), dim);   # Automatic broadcasting
        endswitch

      case "norm"
        switch (methodoption)
          case 1
            s = sum (abs (x), dim);
          case Inf
            s = max (abs (x), [], dim);
          otherwise
            s = sum (abs (x) .^ methodoption, dim) .^ (1/methodoption);
        endswitch

      case "range"
        ## if any range element = 0, avoid divide by zero by replacing that
        ## range element with 1.  output will be zero+min due to x-min(x)=0.
        x_range = range (x, dim);
        x_range(x_range == 0) = 1;
        z_range = methodoption(2) - methodoption(1);
        s = x_range ./ z_range;
        c = min (x, [], dim) - (methodoption(1) .* s);

      case "scale"
        s = process_scale_option (x, dim, methodoption);

      case "center"
        c = process_center_option (x, dim, methodoption);

      case "scaleandcenter"
        ## repeats scale and center using appropriate order and info

        switch (stored_method)
          case "scale"
            ## stored info is scale, latest info is center
            center_option = methodoption;
            scale_option = stored_methodoption;

          case "center"
            ## stored info is center, latest info is scale
            center_option = stored_methodoption;
            scale_option = methodoption;
        endswitch

        s = process_scale_option (x, dim, scale_option);
        c = process_center_option (x, dim, center_option);

      case "medianiqr"
        c = median (x, dim);
        s = iqr (x, dim);

    endswitch

  endif

  ## Divide by scale factor.  If scale = 0, divide by zero = Inf, which is OK.

  ## FIXME: Use bsxfun, rather than broadcasting, until broadcasting
  ##        supports diagonal and sparse matrices (Bugs #41441, #35787).
  z = bsxfun (@rdivide, bsxfun (@minus, x , c), s);
  ## z = (x - c) ./ s;  # Automatic broadcasting

endfunction

function c = process_center_option (x, dim, center_option)

  if (isnumeric (center_option))
    c = center_option;
  else
    switch (center_option)
      case "mean"
        c = mean (x, dim);
      case "median"
        c = median (x, dim);
    endswitch
  endif

endfunction

function s = process_scale_option (x, dim, scale_option)

  warning ("off", "Octave:divide-by-zero", "local");

  if (isnumeric (scale_option))
    s = scale_option;
  else
    switch (scale_option)
      case "std"
        s = std (x, [], dim);
      case "mad"
        s = mad (x, 1, dim);
      case "first"
        dim_vector = repmat ({':'}, ndims(x), 1);
        dim_vector{dim} = 1;
        s = x(dim_vector{:});
      case "iqr"
        s = iqr (x, dim);
    endswitch
  endif

endfunction


## no method specified, using zscore & std
%!assert (normalize ([1,2,3]), [-1,0,1])
%!assert (normalize ([1,2,3], 2), [-1,0,1])
%!assert (normalize (single ([1,2,3])), single ([-1,0,1]))
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2]), [1,0,-1;0,1,0;-1,-1,1])
%!assert (normalize (magic (3)), [[3;-2;-1]/sqrt(7),[-1;0;1],[1;2;-3]/sqrt(7)])
%!assert (normalize (magic (3), 2), [[3 -4 1]/sqrt(13);[-1 0 1];[-1 4 -3]/sqrt(13)])

## Method: zscore, [std, robust]
%!assert (normalize ([1,2,3],"zscore","std"), [-1,0,1])
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2],"zscore","std"), [1,0,-1;0,1,0;-1,-1,1])
%!assert (normalize (magic (3),"zscore","std"), [[3;-2;-1]/sqrt(7),[-1;0;1],[1;2;-3]/sqrt(7)])
%!assert (normalize ([1,2,3],"zscore","robust"), [-1,0,1])
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2],"zscore","robust"), [1,0,-1;0,1,0;-1,-1,1])
%!assert (normalize (magic (3),"zscore","robust"), [4 -1 0; -1 0 1; 0 1 -4])

## Method: norm [1, 2, inf]
%!assert (normalize ([1,2,3],"norm",1), [1/6 1/3 1/2])
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2],"norm",1), [1,0,-1;0,1,0;-1,-1,1]/2)
%!assert (normalize (magic (3),"norm",1), magic(3)/15)
%!assert (normalize ([1,2,3],"norm",2), [1 2 3]./3.741657386773941, eps)
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2],"norm",2), [1,0,-1;0,1,0;-1,-1,1]*(sqrt(2)/2), eps)
%!assert (normalize ([1,2,3],"norm",Inf), [1/3 2/3 1])
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2],"norm",Inf), [1,0,-1;0,1,0;-1,-1,1])
%!assert (normalize (magic (3),"norm",Inf), [[8;3;4]/8,[1;5;9]/9,[6;7;2]/7])

## Method: range
%!assert (normalize ([1,2,3],"range"), [0 0.5 1])
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2],"range",[0 1]), [1,0.5,0;0.5,1,0.5;0,0,1])
%!assert (normalize (magic (3),"range",[-1 1]), [1 -1 0.6; -1 0 1; -0.6 1 -1], eps)

## Method: scale [mad first iqr number]
%!assert (normalize ([1,2,3],"scale"), [1 2 3])
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2],"scale","std"), [1 0 -1; 0 1 0; -1 -1 1])
%!assert (normalize (magic (3),"scale",2), (magic(3)/2))

%!assert (normalize ([1,2,3],"scale", "mad"), [1 2 3])
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2],"scale","mad"), [1 0 -1; 0 1 0; -1 -1 1])
%!assert (normalize (magic (3),"scale","mad"), [8 0.25 6; 3 1.25 7; 4 2.25 2])

%!assert (normalize ([1,2,3],"scale", "first"), [1 2 3])
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2],"scale","first"), [1 NaN 1; 0 Inf 0; -1 -Inf -1])
%!assert (normalize (magic (3),"scale","first"), [1 1 1; 3/8 5 7/6; 0.5 9 1/3])
%!assert (normalize (magic (3),2,"scale","first"), [1 1/8 3/4;1 5/3 7/3;1 9/4 0.5])
%!test
%! x = reshape (magic (4),2,2,2,2);
%! y3 = cat (4, cat (3,ones(2),[1/8 7/9;11/5 7/2]), cat (3,ones(2),[13/3 2; 4/5 1/15]));
%! y4 = cat (4, ones (2,2,2), cat (3,[3/16 2/3; 2 15/4],[6.5 12/7; 8/11 1/14] ));
%! assert (normalize (x, 3, "scale", "first"), y3);
%! assert (normalize (x, 4, "scale", "first"), y4);

%!assert (normalize ([1,2,3], "scale", "iqr"), [1 2 3]*2/3)
%!assert (normalize ([1,2,3]', "scale", "iqr"), ([1 2 3]')*2/3)
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2],"scale","iqr"), [1 0 -1; 0 1 0; -1 -1 1]* 2/3, eps)
%!assert (normalize (magic (3),"scale","iqr"), [[8;3;4]/3.75,[1;5;9]/6,[6;7;2]/3.75],eps)

## Method: center [mean median number]
%!assert (normalize ([1,2,3], "center"), [-1 0 1])
%!assert (normalize ([1,2,3], 1, "center"), [0 0 0])
%!assert (normalize ([1,2,3], "center", 10), [-9 -8 -7])
%!assert (normalize ([1 2 3 10], "center", "mean"), [-3 -2 -1 6])
%!assert (normalize ([1 2 3 10], "center", "median"), [-1.5 -0.5 0.5 7.5])

## Method: medianiqr
%!assert (normalize ([1,2,3], "medianiqr"), [-1 0 1]*2/3)
%!assert (normalize ([1,2,3]', "medianiqr"), ([-1 0 1]')*2/3)
%!assert (normalize ([2,0,-2;0,2,0;-2,-2,2], "medianiqr"), [1 0 -1; 0 1 0; -1 -1 1]*2/3)
%!assert (normalize (magic (3), "medianiqr"), [8/5 -1 0; -2/5 0 2/5; 0 1 -8/5]*2/3)

## Test NaN and Inf
%!assert (normalize ([1 2 Inf], 2), [NaN, NaN, NaN])
%!assert (normalize ([1 2 3], 1), [NaN, NaN, NaN])
%!assert (normalize ([1 2 3], 3), [NaN, NaN, NaN])
%!assert (normalize (ones (3,2,2,2)), NaN (3,2,2,2))
%!assert (normalize (Inf), NaN)
%!assert (normalize (NaN), NaN)
%!assert (normalize ([Inf, NaN]), [NaN, NaN])
%!assert (normalize ([Inf, NaN]'), [NaN, NaN]')
%!assert (normalize ([Inf, Inf], 1), [NaN, NaN])
%!assert (normalize ([Inf, Inf], 2), [NaN, NaN])
%!assert (normalize ([Inf, Inf]', 1), [NaN, NaN]')
%!assert (normalize ([Inf, Inf]', 2), [NaN, NaN]')
%!assert (normalize ([1 2 NaN; NaN 3 4], 1), [NaN -1 NaN; NaN 1 NaN]*sqrt(2)/2, eps)

## Two input methods, must be scale and center
%!assert (normalize (magic(3), "scale", "center"), normalize (magic(3), "zscore"), eps)
%!assert (normalize (magic(3), "center", "scale"), normalize (magic(3), "zscore"), eps)

## Test additional outputs
%!test
%! [z, c, s] = normalize ([1, 2, 3], 2);
%! assert ({z, c, s}, {[-1 0 1], [2], [1]});
%! [z, c, s] = normalize (magic (3), "zscore", "std");
%! assert ({z, c, s}, {[[3;-2;-1]/sqrt(7),[-1;0;1],[1;2;-3]/sqrt(7)], [5 5 5], [sqrt(7) 4 sqrt(7)]});
%! [z, c, s] = normalize (magic (3), "zscore", "robust");
%! assert ({z, c, s}, {[4 -1 0; -1 0 1; 0 1 -4], [4 5 6], [1 4 1]});
%! [z, c, s] = normalize (magic (3), "norm", 1);
%! assert ({z, c, s}, {magic(3)/15 , 0, [15 15 15]});
%! [z, c, s] = normalize ([2,0,-2;0,2,0;-2,-2,2],"norm",2);
%! assert ({z, c, s}, {[1,0,-1;0,1,0;-1,-1,1]*(sqrt(2)/2), 0, [1 1 1]*2*sqrt(2)}, eps);
%! [z, c, s] = normalize ([1 2 3], "norm", Inf);
%! assert ({z, c, s}, {[1 2 3]/3, 0, 3}, eps);
%! [z, c, s] = normalize (magic (3),"range",[-1 1]);
%! assert ({z, c, s}, {[1 -1 0.6; -1 0 1; -0.6 1 -1], [5.5 5 4.5], [2.5 4 2.5]}, eps);
%! [z, c, s] = normalize (magic (3),"scale","mad");
%! assert ({z, c, s}, {[8 0.25 6; 3 1.25 7; 4 2.25 2], 0, [1 4 1]});
%! [z, c, s] = normalize (magic (3),"scale","first");
%! assert ({z, c, s}, {[1 1 1; 3/8 5 7/6; 0.5 9 1/3],0, [8 1 6]}, eps);
%! [z, c, s] = normalize ([1,2,3]', "scale", "iqr");
%! assert ({z, c, s}, {([1 2 3]')*2/3, 0, 1.5});
%! [z, c, s] = normalize ([1,2,3], "center", 10);
%! assert ({z, c, s}, {[-9 -8 -7], 10, 1});
%! [z, c, s] = normalize ([1 2 3 10], "center", "mean");
%! assert ({z, c, s}, {[-3 -2 -1 6], 4, 1});
%! [z, c, s] = normalize ([1 2 3 10], "center", "median");
%! assert ({z, c, s}, {[-1.5 -0.5 0.5 7.5], 2.5, 1});
%! [z, c, s] = normalize (magic (3), "medianiqr");
%! assert ({z, c, s}, {[8/5 -1 0; -2/5 0 2/5; 0 1 -8/5]*2/3, [4 5 6], [3.75 6 3.75]}, eps);
%! [z, c, s] = normalize ([1 2 Inf], 2);
%! assert ({z, c, s}, {[NaN, NaN, NaN], Inf, NaN});
%! [z, c, s] = normalize (Inf);
%! assert ({z, c, s}, {NaN, Inf, NaN});

## Test sparse and diagonal inputs
%!test
%! [z, c, s] = normalize (eye (2));
%! assert (z, (sqrt(2)/2)*[1, -1; -1, 1], eps);
%! assert (c, [0.5, 0.5], eps);
%! assert (s, (sqrt(2)/2)*[1, 1], eps);
%!test
%! [z, c, s] = normalize (sparse (eye (2)));
%! assert (full (z), (sqrt(2)/2)*[1, -1; -1, 1], eps);
%! assert (full (c), [0.5, 0.5], eps);
%! assert (full (s), (sqrt(2)/2)*[1, 1], eps);
%!test
%! [z, c, s] = normalize (sparse (magic (3)), "zscore", "robust");
%! assert (full (z), [4 -1 0; -1 0 1; 0 1 -4], eps);
%! assert (full (c), [4, 5, 6], eps);
%! assert (full (s), [1, 4, 1], eps);
%!test <55765>
%! [z, c, s] = normalize (sparse (eye(2)));
%! assert (issparse (z));
%! assert (issparse (c));
%! assert (issparse (s));
%!test <55765>
%! [z, c, s] = normalize (sparse (magic (3)), "zscore", "robust");
%! assert (issparse (z));
%! assert (issparse (c));
%! assert (issparse (s));

## Matlab ignores NaNs, operating as if the vector had one less element, then
## returns the result retaining the NaN in the solution.
%!assert <50571> (normalize ([1 2 NaN], 2), [-1, 1, NaN]*sqrt(2)/2)
%!assert <50571> (normalize ([1 2 NaN; 1 2 3], 2), [[-1 1 NaN]*sqrt(2)/2; -1 0 1], eps)

## Test input validation
%!error <Invalid call> normalize ()
%!error <Invalid call> normalize (1, 2, 3)
%!error <X must be a numeric> normalize (['A'; 'B'])
%!error <DIM must be an integer> normalize (1, ones (2,2))
%!error <DIM must be an integer> normalize (1, 1.5)
%!error <DIM must be .* a valid dimension> normalize (1, 0)
%!error <more than two methods specified> normalize ([1 2 3], "scale", "center", "norm")
%!error <methods .* may not be combined> normalize ([1 2 3], "norm", "zscore")
%!error <unknown method 'foo'> normalize ([1 2 3], "norm", "foo")
%
%!error <'norm' option must be a positive scalar or Inf> normalize ([1 2 3], "norm", [1 2])
%!error <'norm' option must be a positive scalar or Inf> normalize ([1 2 3], "norm", -1)
%!error <'range' must be specified as> normalize ([1 2 3], "range", [1 2]')
%!error <'range' must be specified as> normalize ([1 2 3], "range", [1 2 3])
%!error <'range' must be specified as> normalize ([1 2 3], "range", 1)
%!error <scale value must be a scalar> normalize ([1 2 3], "scale", [1 2 3])
%!error <center shift must be a scalar value> normalize ([1 2 3], "center", [1 2])
%!error <unknown method 'foo'> normalize ([1 2 3], "foo")
