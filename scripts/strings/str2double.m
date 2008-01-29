## Copyright (C) 2004, 2005, 2006, 2007 by Alois Schloegl
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
## @deftypefn {Function File} {[@var{num}, @var{status}, @var{strarray}] =} str2double (@var{str}, @var{cdelim}, @var{rdelim}, @var{ddelim})
## Convert strings into numeric values.
##
## @code{str2double} can replace @code{str2num}, but avoids the use of
## @code{eval} on unknown data.
##
## @var{str} can be the form @samp{[+-]d[.]dd[[eE][+-]ddd]} in which
## @samp{d} can be any of digit from 0 to 9, and @samp{[]} indicate
## optional elements.
##
## @var{num} is the corresponding numeric value.  If the conversion
## fails, status is -1 and @var{num} is NaN.
##
## @var{status} is 0 if the conversion was successful and -1 otherwise.
##
## @var{strarray} is a cell array of strings.
##
## Elements which are not defined or not valid return NaN and the
## @var{status} becomes -1.
##
## If @var{str} is a character array or a cell array of strings, then
## @var{num} and @var{status} return matrices of appropriate size. 
##
## @var{str} can also contain multiple elements separated by row and
## column delimiters (@var{cdelim} and @var{rdelim}).
## 
## The parameters @var{cdelim}, @var{rdelim}, and @var{ddelim} are
## optional column, row, and decimal delimiters.
##
## The default row-delimiters are newline, carriage return and semicolon
## (ASCII 10, 13 and 59).  The default column-delimiters are tab, space
## and comma (ASCII 9, 32, and 44).  The default decimal delimiter is
## @samp{.} (ASCII 46).
##
## @var{cdelim}, @var{rdelim}, and @var{ddelim} must contain only nul,
## newline, carriage return, semicolon, colon, slash, tab, space, comma,
## or @samp{()[]@{@}} (ASCII 0, 9, 10, 11, 12, 13, 14, 32, 33, 34, 40,
## 41, 44, 47, 58, 59, 91, 93, 123, 124, 125).
##
## Examples:
##
## @example
## str2double ("-.1e-5")
## @result{} -1.0000e-006
##
## str2double (".314e1, 44.44e-1, .7; -1e+1")
## @result{}
##    3.1400    4.4440    0.7000
##  -10.0000       NaN       NaN
##
## line = "200, 300, NaN, -inf, yes, no, 999, maybe, NaN";
## [x, status] = str2double (line)
## @result{} x =
##     200   300   NaN  -Inf   NaN   NaN   999   NaN   NaN
## @result{} status =
##       0     0     0     0    -1    -1     0    -1     0
## @end example
## @end deftypefn

## Author: Alois Schloegl <a.schloegl@ieee.org>
## Adapted-by: jwe

function [num, status, strarray] = str2double (s, cdelim, rdelim, ddelim)

  ## digits, sign, exponent,NaN,Inf
  ## valid_char = '0123456789eE+-.nNaAiIfF';

  ## valid delimiters
  valid_delim = char (sort ([0, 9:14, 32:34, abs("()[]{},;:\"|/")]));

  if (nargin < 1)
    error ("missing input argument");
  endif

  if (nargin < 2)
    ## column delimiter
    cdelim = char ([9, 32, abs(",")]);
  else
    ## make unique cdelim
    cdelim = char (sort (cdelim(:)));
    tmp = [1; 1+find(diff(abs(cdelim))>0)];
    cdelim = cdelim(tmp)';
  endif

  if (nargin < 3)
    ## row delimiter
    rdelim = char ([0, 10, 13, abs(";")]);
  else
    ## make unique rdelim
    rdelim = char (sort (rdelim(:)));
    tmp = [1; 1+find(diff(abs(rdelim))>0)];
    rdelim = rdelim(tmp)';
  endif

  if (nargin < 4)
    ddelim = ".";
  elseif (length (ddelim) != 1)
    error ("decimal delimiter must be exactly one character");
  endif

  ## check if RDELIM and CDELIM are distinct

  delim = sort (abs ([cdelim, rdelim, ddelim]));
  tmp   = [1, 1+find(diff(delim)>0)];
  delim = delim(tmp);
  ## [length(delim),length(cdelim),length(rdelim)]
  if (length (delim) < (length(cdelim) + length(rdelim))+1)
    ## length (ddelim) must be one.
    error ("row, column and decimal delimiter are not distinct");
  endif

  ## check if delimiters are valid
  tmp  = sort (abs ([cdelim, rdelim]));
  flag = zeros (size (tmp));
  k1 = 1;
  k2 = 1;
  while (k1 <= length (tmp) && k2 <= length (valid_delim)),
    if (tmp(k1) == valid_delim(k2))
      flag(k1) = 1;
      k1++;
    elseif (tmp(k1) < valid_delim(k2))
      k1++;
    elseif (tmp(k1) > valid_delim(k2))
      k2++;
    endif
  endwhile
  if (! all (flag))
    error ("invalid delimiters!");
  endif

  ## various input parameters

  if (isnumeric (s))
    if (all (s < 256) && all (s >= 0))
      s = char (s);
    else
      error ("str2double: input variable must be a string");
    endif
  endif

  if (isempty (s))
    num = [];
    status = 0;
    return;
  elseif (iscell (s))
    strarray = s;
  elseif (ischar (s) && all (size (s) > 1))
    ## char array transformed into a string.
    for k = 1:size (s, 1)
      tmp = find (! isspace (s(k,:)));
      strarray{k,1} = s(k,min(tmp):max(tmp));
    endfor
  elseif (ischar (s)),
    num = [];
    status = 0;
    strarray = {};
    ## add stop sign; makes sure last digit is not skipped
    s(end+1) = rdelim(1);
    RD = zeros (size (s));
    for k = 1:length (rdelim),
      RD = RD | (s == rdelim(k));
    endfor
    CD = RD;
    for k = 1:length (cdelim),
      CD = CD | (s==cdelim(k));
    endfor

    k1 = 1; # current row
    k2 = 0; # current column
    k3 = 0; # current element

    sl = length (s);
    ix = 1;
    ## while (ix < sl) & any(abs(s(ix))==[rdelim,cdelim]),
    while (ix < sl && CD(ix))
      ix++;
    endwhile
    ta = ix;
    te = [];
    while (ix <= sl)
      if (ix == sl)
        te = sl;
      endif
      ## if any(abs(s(ix))==[cdelim(1),rdelim(1)]),
      if (CD(ix))
        te = ix - 1;
      endif
      if (! isempty (te))
        k2++;
        k3++;
        strarray{k1,k2} = s(ta:te);
        ## strarray{k1,k2} = [ta,te];

        flag = 0;
        ## while any(abs(s(ix))==[cdelim(1),rdelim(1)]) & (ix < sl),
        while (CD(ix) && ix < sl)
          flag = flag | RD(ix);
          ix++;
        endwhile

        if (flag)
          k2 = 0;
          k1++;
        endif
        ta = ix;
        te = [];
      endif
      ix++;
    endwhile
  else
    error ("str2double: invalid input argument");
  endif

  [nr, nc]= size (strarray);
  status = zeros (nr, nc);
  num = repmat (NaN, nr, nc);

  for k1 = 1:nr
    for k2 = 1:nc
      t = strarray{k1,k2};
      if (length (t) == 0)
	## return error code
	status(k1,k2) = -1;
	num(k1,k2) = NaN;
      else
	## get mantisse
	g = 0;
	v = 1;
	if (t(1) == "-")
	  v = -1;
	  l = min (2, length(t));
	elseif (t(1) == "+")
	  l = min (2, length (t));
	else
	  l = 1;
	endif

	if (strcmpi (t(l:end), "inf"))
	  num(k1,k2) = v*Inf;
	elseif (strcmpi (t(l:end), "NaN"));
	  num(k1,k2) = NaN;
	else
	  if (ddelim == ".")
	    t(t==ddelim) = ".";
	  endif
	  [v, tmp2, c] = sscanf(char(t), "%f %s", "C");
	  ## [v,c,em,ni] = sscanf(char(t),"%f %s");
	  ## c = c * (ni>length(t));
	  if (c == 1),
	    num(k1,k2) = v;
	  else
	    num(k1,k2) = NaN;
	    status(k1,k2) = -1;
	  endif
	endif
      endif
    endfor
  endfor

endfunction
