## Copyright (C) 1995, 1996, 1997  Kurt Hornik
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
## 
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details. 
## 
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## Performs a one-way analysis of variance (ANOVA).  The goal is to test
## whether the population means of data taken from k different groups
## are all equal.
##
## anova (y, g) provides all data in a single vector y;  g is the vector
## of corresponding group labels (e.g., numbers from 1 to k). This is
## the general form which does not impose any restriction on the number
## of data in each group or the group labels (other than that they must
## be scalars).
##
## anova (y), where y is a matrix, treats each column as a group. This
## form is only appropriate for balanced ANOVA where the numbers of
## samples from each group are all equal.
##
## Under the null of constant means, the statistic f follows an F
## distribution with df_b and df_w degrees of freedom.  pval is the
## p-value (1 minus the CDF of this distribution at f) of the test.
##
## If no output argument is given, the standard one-way ANOVA table is
## printed.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  One-way analysis of variance (ANOVA)
  
function [pval, f, df_b, df_w] = anova (y, g)
  
  if ((nargin < 1) || (nargin > 2))
    usage ("anova (y [, g])");
  elseif (nargin == 1)
    if (is_vector (y))
      error ("anova:  for `anova (y)', y must not be a vector");
    endif
    [group_count, k] = size (y);
    n = group_count * k;
    group_mean = mean (y);
  else
    if (! is_vector (y))
      error ("anova:  for `anova (y, g)', y must be a vector");
    endif
    n = length (y);
    if (! is_vector (g) || (length (g) != n))
      error (["anova:  for `anova (y, g)', g must be a vector", ...
		" of the same length y"]);
    endif
    s = sort (g);
    i = find (s (2 : n) > s(1 : (n-1)));
    k = length (i) + 1;
    if (k == 1)
      error ("anova:  there should be at least 2 groups");
    else
      group_label = s ([1, (reshape (i, 1, k-1) + 1)]);
    endif
    for i = 1 : k;
      v = y (find (g == group_label (i)));
      group_count (i) = length (v);
      group_mean (i) = mean (v);
    endfor
    
  endif
  
  total_mean = mean (group_mean);  
  SSB = sum (group_count .* (group_mean - total_mean) .^ 2);
  SST = sumsq (reshape (y, n, 1) - total_mean); 
  SSW = SST - SSB;
  df_b = k - 1;
  df_w = n - k;
  v_b = SSB / df_b;
  v_w = SSW / df_w;
  f = v_b / v_w;
  pval = 1 - f_cdf (f, df_b, df_w);
  
  if (nargout == 0)
    ## This eventually needs to be done more cleanly ...
    printf ("\n");
    printf ("One-way ANOVA Table:\n");
    printf ("\n");
    printf ("Source of Variation   Sum of Squares    df  Empirical Var\n");
    printf ("*********************************************************\n");
    printf ("Between Groups       %15.4f  %4d  %13.4f\n", SSB, df_b, v_b);
    printf ("Within Groups        %15.4f  %4d  %13.4f\n", SSW, df_w, v_w);
    printf ("---------------------------------------------------------\n");
    printf ("Total                %15.4f  %4d\n", SST, n - 1);
    printf ("\n");
    printf ("Test Statistic f     %15.4f\n", f);    
    printf ("p-value              %15.4f\n", pval);
    printf ("\n");
  endif  
  
endfunction
