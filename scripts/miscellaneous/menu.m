function s = menu (t,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16)

# usage: menu (title, opt1, opt2, ..., opt16)
#
# See also: disp, printf, input

  if (nargin < 2 || nargin > 17)
    error ("usage: menu (title, opt1, opt2, ..., opt16)");
  endif

# Force pending output to appear before the menu.

  fflush (stdout);

# Don't send the menu through the pager since doing that can cause
# major confusion.

  save_page_screen_output = page_screen_output;
  page_screen_output = "false";

  if (! isempty (t))
    disp (t);
    printf ("\n");
  endif

  nopt = nargin - 1;

  s = 0;
  while (1)
    page_screen_output = "false";
    for i = 1:nopt
      command = sprintf ("printf (\"  [%2d] \"); disp (x%d)", i, i);
      eval (command);
    endfor
    printf ("\n");
    page_screen_output = save_page_screen_output;
    s = input ("pick a number, any number: ");
    if (s < 1 || s > nopt)
      printf ("\nerror: input out of range\n\n");
    else
      break;
    endif
  endwhile

  page_screen_output = save_page_screen_output;

endfunction
