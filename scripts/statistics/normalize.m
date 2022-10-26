########################################################################
##
## Copyright (C) 2017-2020 The Octave Project Developers
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
## @deftypefn {} {@var{Z} =} normalize (@var{X})
## @deftypefnx {} {@var{Z} =} normalize (@var{X}, @var{dim})
## @deftypefnx {} {@var{Z} =} normalize (@dots{}, @var{method})
## @deftypefnx {} {@var{Z} =} normalize (@dots{}, @var{method}, @var{Option})
## @deftypefnx {} {@var{Z} =} normalize (@dots{}, @var{scale}, @var{ScaleOption}, @var{center}, @var{CenterOption})
## @deftypefnx {} {[@var{Z}, @var{C}, @var{S}] =} normalize (@dots{})
##
## Returns a normalization of @var{X}.  The default normalization is the
## @code{zscore} of @var{X}, defined as the number of standard deviations each
## element is from the mean of @var{X}.  The returned value @var{Z} will have
## the same size as @var{X}. The optional return variables @var{C} and @var{S}
## are the centering and scaling factors used in the normalization such that:
##
## @example
## @group
##   @tcode{@var{Z} = (@var{X} - @var{C}) ./ @var{S}}
## @end group
## @end example
##
## If @var{X} is a vector, @code{normalize} will operate on the data in @var{X}.
##
## If @var{X} is a matrix, @code{normalize} will operate independently on
## each column in @var{X}.
##
## If @var{X} is a n-dimensional array, @code{normalize} will operate
## independently on the first non-singleton dimension in @var{X}.
##
## The optional second input variable @var{DIM} can be used to force @code{normalize}
## to operate over the specified dimension. @var{DIM} must be an integer scalar.
##
## The optional input variables @var{Method} and @var{Option} can be used to
## specify the type of normalization performed on @var{X}.  Note that only the
## @option{scale} and @option{center} options may be specified together using
## any of the methods defined below.  Valid normalization methods are:
##
## @table @code
## @item zscore
## (Default) Normalizes the elements in @var{X} to the scaled distance from a
##  central value. Valid Options:
##
##    @table @code
##    @item std
##    (Default) Data is centered at @code{mean(@var{X})} and scaled by the
#      standard deviation.
##    @item robust
##    Data is centered at @code{median(@var{X})} and scaled by the median
##    absolute deviation.
##    @end table
##
## @item norm
## @var{Z} is the general vector norm of @var{X}, with @var{OPTION} being the
## normalization factor @var{P} that determines the vector norm type according
## to:
## @tex
## $$Z = \left (\sum_k \left | X_k \right |^P  \right )^{1/P}$$
## @end tex
## @ifnottex
##
## @example
## @group
##   @tcode{@var{Z} = [sum (abs(@var{X})^@var{P})]^(1/@var{P})}
## @end group
## @end example
##
## @end ifnottex
## @var{P} can be any positive scalar, specific values being:
##
##    @table @code
##    @item @var{P} = 1
##    @var{X} is normalized by @code{sum (abs (@var{X}))}.
##    @item @var{P} = 2
##    (Default) @var{X} is normalized by the Euclidian norm, or vector
##    magnitude, of the elements.
##    @item @var{P} = Inf
##    @var{X} is normalized by @code{max (abs (@var{X}))}.
##    @end table
##
## @item scale
## @var{X} is scaled by a factor determined by @var{Option}, which can be a
## numeric scalar or one of the following:
##
##    @table @code
##    @item std
##    (Default) @var{X} is scaled by its standard deviation.
##    @item mad
##    @var{X} is scaled by its median absolute deviation.
##    @item first
##    @var{X} is scaled by the first element in the vector.
##    @item iqr
##    @var{X} is scaled by the vector's interquartile range.
##    @end table
##
## @item range
## @var{X} is scaled to fit the range specified by @var{Option} as a two element
## scalar row vector.  The default range is [0 1].
##
## @item center
## @var{X} is shifted by an amount determined by @var{Option}, which can be a
## numeric scalar or one of the following:
##
##    @table @code
##    @item mean
##    (Default) @var{X} is shifted by @code{mean (@var{X})}.
##    @item median
##    @var{X} is shifted by @code{median (@var{X})}.
##    @end table
##
## @item medianiqr
## @var{X} is shifted such by @code{median (@var{X})} and scaled by the vector's
## interquartile range.
## @end table
##
## Known @sc{matlab} incompatibilities:
##
## @enumerate
## @item
## The DataVariables option is not yet implemented in @code{normalize} for
## Table class @var{X} inputs.
##
## @item
## Certain arrays containing NaN elements may not return Matlab compatible
## output.
## @end enumerate
##
## @seealso{zscore, iqr, norm, rescale, std, median, mean, mad}
## @end deftypefn

function [z, centervalues, scalevalues] = normalize (x, varargin)
  ## FIXME: until NANFLAG/OMITNAN option is implemented in std, mean, median,
  ## etc., normalize cannot efficiently reproduce some behavior with NaN's in x.
  ## xtests added to capture this. (see bug #50571)

  ## FIXME: when table class is implemented, remove DataVariables error line in
  ## option checking section and add DataVariables data handling switch setion.

  ## input checks
  if ((nargin < 1) || (nargin > 8))
    print_usage ();
  endif

  if !(isnumeric (x) || islogical (x))
    error ("normalize: X must be a numeric vector, matrix, or array");
  endif

  if (nargin == 1)
    ## directly handle simple 1 input case.
    [scalevalues, centervalues] = std (x);
    z = (x - centervalues)./ scalevalues;

  else
    ## parse input options
    dim = []; # default = vector dimension, matrix column, or 1st n-dim >1
    method = []; # default = zscore
    methodoption = []; # default = std
    datavariables_flag = false;
    datavar = [];
    scale_and_center_flag = false;

    vararg_idx = 1;
    ##only second optional input can be numeric without following a method
    if isnumeric (varargin{1})
      dim = varargin{1};
      ##check for valid dimensions
      if !((isscalar (dim)) && (dim > 0) && (dim == fix (dim)))
        error ("normalize: DIM must be a positive scalar integer");
      endif
      vararg_idx++;
    endif

    ## parse varargin to determine methods then options
    while (vararg_idx <= (nargin - 1))
      ## arguments after second cannot be numeric without following a method
      if isnumeric (varargin{vararg_idx})
        print_usage ();
      endif

      prop = tolower (varargin{vararg_idx});

      if (strcmp (prop,"datavariables"))
        ##FIXME: remove error on next line when Tables is implemented
        error ("normalize: DataVariables method not yet implemented.");
        if (vararg_idx == (nargin - 1))
          error (["normalize: DataVariables requires a table variable", ...
                 " be specified"]);
        elseif (datavariables_flag == true)
          error ("normalize: DataVariables may only be specified once");
        else
          datavariables_flag = true;
          datavar = varargin{vararg_idx+1}; #no tolower if Tables case sensitive
          vararg_idx++;
        endif

      else
        if (!isempty (method))
        ## catch if a second method is passed
          if (scale_and_center_flag)
            ## if true, already specified two methods, three never possible
            error ("normalize: too many methods specified");

          elseif ((strcmp ({method, prop}, {"center", "scale"})) ...
                || strcmp ({method, prop}, {"scale", "center"}))
            ## only scale and center can be called together
            scale_and_center_flag = true;
            ## scale/center order doesn't matter, avoid overwriting first one
            stored_method = method;
            method = [];
            stored_methodoption = methodoption;
            methodoption = [];
          else
            ## not scale and center, throw appropriate error
            if any (strcmp (prop, {"zscore", "norm", "range", "scale", ...
                       "center", "medianiqr"}))
              error ("normalize: methods `%s` and `%s` may not be combined", ...
                       method, prop);
            else
              error ("normalize: unknown method `%s`", prop);
            endif
          endif
        endif

        ## determine method and whether there's an appropriate option specified
        switch (prop)
          case "zscore"
            method = "zscore";
            if (vararg_idx < (nargin - 1))
              nextprop = tolower (varargin{vararg_idx+1});
              if (strcmp (nextprop, "std") || strcmp (nextprop, "robust"))
                if (!isempty (methodoption))
                  error ("normalize: only one method option may be specified");
                endif
                methodoption = nextprop;
                vararg_idx++;
              endif
            endif
            if (isempty (methodoption))
              methodoption = "std";
            endif

          case "norm"
            method = "norm";
            if ((vararg_idx < (nargin - 1)) && ...
                    isnumeric (varargin{vararg_idx+1}))
              nextprop = varargin{vararg_idx+1};
              if (isscalar (nextprop) && (nextprop > 0))
                if (!isempty (methodoption))
                  error ("normalize: only one method option may be specified");
                endif
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
            if (vararg_idx < (nargin - 1) && isnumeric (varargin{vararg_idx+1}))
              nextprop = varargin{vararg_idx+1};
              if (size (nextprop) == [1 2])
                if (!isempty (methodoption))
                  error ("normalize: only one method option may be specified");
                endif
                methodoption = nextprop;
                vararg_idx++;
              else
                error (["normalize: 'range' must be specified as a ", ...
                        "2-element row vector [a b]"]);
              endif
            endif
            if (isempty (methodoption))
              methodoption = [0 1];
            endif

          case "scale"
            method = "scale";
            if (vararg_idx < (nargin - 1))
              nextprop = tolower (varargin{vararg_idx+1});
              if (isnumeric (nextprop))
                if (!isscalar (nextprop))
                  error ("normalize: scale value must be a scalar");
                else
                  methodoption = nextprop;
                  vararg_idx++;
                endif
              elseif (any (strcmp (nextprop, {"std", "mad", "first", "iqr"})))
                if (!isempty (methodoption))
                  error ("normalize: only one method option may be specified");
                endif
                methodoption = nextprop;
                vararg_idx++;
              endif
            endif

            if (isempty (methodoption))
              methodoption = 'std';
            endif

          case "center"
            method = "center";
            if (vararg_idx < (nargin - 1))
              nextprop = tolower (varargin{vararg_idx+1});
              if (isscalar (nextprop) || ...
                   any (strcmp (nextprop, {"mean", "median"})))
                if (!isempty (methodoption))
                  error ("normalize: only one method option may be specified");
                endif
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

    if scale_and_center_flag
      method = "scaleandcenter";
    endif

    if isempty (method)
      method = 'zscore';
      methodoption = 'std';
    endif

    if isempty (dim)
      if isvector (x)
        if iscolumn (x)
          dim = 1;
        else
          dim = 2;
        endif
      elseif (ismatrix(x))
        ## matrix x, operate independently along columns
        dim = 1;
      else
        ## operate on first non-singleton dimension.
        dim = find ((size (x) > 1), 1);
      endif

    endif


    ## Perform normalization based on specified methods

    ## FIXME: DataTables option not handled below. Fix after Table Class
    ## has been implemented.

    ## Default scaling factors:
    centervalues = 0;
    scalevalues = 1;

    switch (method)
      case "zscore"
        switch methodoption
          case "std"
            [scalevalues, centervalues] = std (x, [], dim);
          case "robust"
            ##center/median to zero and MAD = 1
            centervalues = median (x, dim);
            scalevalues = median (abs (x - centervalues), dim);
        endswitch

      case "norm"
        switch methodoption
          case 1
            scalevalues = sum (abs (x), dim);
          case Inf
            scalevalues = max (abs (x), [], dim);
          otherwise
            scalevalues = sum ((abs (x) .^ methodoption), dim) .^...
                                 (1./methodoption);
        endswitch

      case "range"
        ## if any range element = 0, avoid divide by zero by replacing that
        ## range element with 1. output will be zero+min due to x-min(x)=0.
        x_range = range (x, dim);
        x_range(x_range == 0) = 1;
        z_range = methodoption(2) - methodoption(1);
        scalevalues = x_range ./ z_range;
        centervalues = min (x, [], dim) - (methodoption(1) .* scalevalues);

      case "scale"
        scalevalues = process_scale_option (x, dim, methodoption);

      case "center"
        centervalues = process_center_option (x, dim, methodoption);

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

        scalevalues = process_scale_option (x, dim, scale_option);
        centervalues = process_center_option (x, dim, center_option);

      case "medianiqr"
        centervalues = median (x, dim);
        scalevalues = iqr (x, dim);
    endswitch

  endif

  ## div by scale factor.  if scale = 0, div by zero = Inf is ok.
  z = (x - centervalues) ./ scalevalues;
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
  warning("off", "Octave:divide-by-zero");
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
  warning("on", "Octave:divide-by-zero")
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

##test nan and inf
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

##Two input methods, must be scale and center
%!assert (normalize (magic(3), "scale", "center"), normalize (magic(3), "zscore"), eps)
%!assert (normalize (magic(3), "center", "scale"), normalize (magic(3), "zscore"), eps)

##Test additional outputs
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
%! assert ({z, c, s}, {[1,0,-1;0,1,0;-1,-1,1]*(sqrt(2)/2), 0, [1 1 1]*2*sqrt(2)}, eps)
%! [z, c, s] = normalize ([1 2 3], "norm", Inf);
%! assert ({z, c, s}, {[1 2 3]/3, 0, 3}, eps);
%! [z, c, s] = normalize (magic (3),"range",[-1 1]);
%! assert ({z, c, s}, {[1 -1 0.6; -1 0 1; -0.6 1 -1], [5.5 5 4.5], [2.5 4 2.5]}, eps)
%! [z, c, s] = normalize (magic (3),"scale","mad");
%! assert ({z, c, s}, {[8 0.25 6; 3 1.25 7; 4 2.25 2], 0, [1 4 1]});
%! [z, c, s] = normalize (magic (3),"scale","first");
%! assert ({z, c, s}, {[1 1 1; 3/8 5 7/6; 0.5 9 1/3],0, [8 1 6]}, eps);
%! [z, c, s] = normalize ([1,2,3]', "scale", "iqr");
%! assert ({z, c, s}, {([1 2 3]')*2/3, 0, 1.5});
%! [z, c, s] = normalize ([1,2,3], "center", 10);
%! assert ({z, c, s}, {[-9 -8 -7], 10, 1});
%! [z, c, s] = normalize ([1 2 3 10], "center", "mean");
%! assert ({z, c, s}, {[-3 -2 -1 6], 4, 1})
%! [z, c, s] = normalize ([1 2 3 10], "center", "median");
%! assert ({z, c, s}, {[-1.5 -0.5 0.5 7.5], 2.5, 1});
%! [z, c, s] = normalize (magic (3), "medianiqr");
%! assert ({z, c, s}, {[8/5 -1 0; -2/5 0 2/5; 0 1 -8/5]*2/3, [4 5 6], [3.75 6 3.75]}, eps)
%! [z, c, s] = normalize ([1 2 Inf], 2);
%! assert ({z, c, s}, {[NaN, NaN, NaN], Inf, NaN});
%! [z, c, s] = normalize (Inf);
%! assert ({z, c, s}, {NaN, Inf, NaN});

## Matlab ignores NaNs, operating as if the vector had one less element, then
## returns the result retaining the NaN in the solution.
%!assert <50571> (normalize ([1 2 NaN], 2), [-1, 1, NaN]*sqrt(2)/2)
%!assert <50571> (normalize ([1 2 NaN; 1 2 3], 2), [[-1 1 NaN]*sqrt(2)/2; -1 0 1], eps)

## Test input validation
%!error normalize ()
%!error normalize (1, 2, 3)
%!error <X must be a numeric> normalize (['A'; 'B'])
%!error <DIM must be a positive scalar integer> normalize (1, ones (2,2))
%!error <DIM must be a positive scalar integer> normalize (1, 1.5)
%!error <DIM must be a positive scalar integer> normalize (1, -1)
%!error <DIM must be a positive scalar integer> normalize (1, [1 2])
%!error <DIM must be a positive scalar integer> normalize (1, 0)
%!error <may not be combined> normalize ([1 2 3], "norm", "zscore")
%!error <unknown method> normalize ([1 2 3], "norm", "foo")
%!error <too many methods specified> normalize ([1 2 3], "scale", "center", "norm")
%!error <'norm' option must be a positive scalar or Inf> normalize ([1 2 3], "norm", -1)
%!error <'norm' option must be a positive scalar or Inf> normalize ([1 2 3], "norm", -Inf)
%!error <'norm' option must be a positive scalar or Inf> normalize ([1 2 3], "norm", [1 2])
%!error <'range' must be specified as> normalize ([1 2 3], "range", [1 2]')
%!error <'range' must be specified as> normalize ([1 2 3], "range", [1 2 3])
%!error <'range' must be specified as> normalize ([1 2 3], "range", 1)
%!error <scale value must be a scalar> normalize ([1 2 3], "scale", [1 2 3])
%!error <scale value must be a scalar> normalize ([1 2 3], "scale", [1 2 3]')
%!error <scale value must be a scalar> normalize ([1 2 3], "scale", [1 2; 3 4])
%!error <center shift must be a scalar value> normalize ([1 2 3], "center", [1 2])
%!error <center shift must be a scalar value> normalize ([1 2 3], "center", [1 2]')
%!error <center shift must be a scalar value> normalize ([1 2 3], "center", [1 2; 3 4])
%!error <unknown method> normalize ([1 2 3], "foo")
