function picks = texas_lotto ()

# usage: texas_lotto
#
# Pick 6 unique numbers between 1 and 50 that are guaranteed to win
# the Texas Lotto.
#
# See also: rand

  if (nargin != 0)
    disp ("win_texas_lotto: ignoring extra arguments");
  endif

  picks = zeros (1,6);
  picks (1) = round (50-49*(1-rand));
  n = 2;
  while (n < 7)
    tmp = round (50-49*(1-rand));
    equal = 0;
    for i = 1:n
      if (tmp == picks (i))
        equal = 1;
        break;
      endif
    endfor
    if (! equal)
      picks (n) = tmp;
      n++;
    endif
  endwhile

  picks = sort (picks);

endfunction
