# Octave demo

# This is not a function file.
1;

function demo_clear ()
  clc ();
endfunction

function demo_set_term ()
  term = getenv ("TERM");
  display = getenv ("DISPLAY");
  if (! strcmp (term, "xterm") || columns (display) == 0)
    graphics_terminal = input ("enter graphics terminal type: ", "s");
    command = sprintf ("set term %s", graphics_terminal);
    eval (command);
  endif
endfunction

function demo_graphics ()
  demo_clear ();
  demo_set_term ()
  demo_clear ();
  printf ("Graphics\n");
  fflush (stdout);
  sombrero (31);
  pause (10);
  printf ("press return to continue\n");
  fflush (stdout);
  pause ();
endfunction

function demo_linear_algebra ()
  demo_clear ();
  printf ("Solving Linear Algebra Problems\n");
  fflush (stdout);
  pause  ();
endfunction

function demo_odes ()
  demo_clear ();
  printf ("Solving ODEs and DAEs\n");
  fflush (stdout);
  pause  ();
endfunction

while (1)

  demo_clear ();

  choice = menu ("Octave\n======\n\n", ...
                 "Graphics", ...
                 "Linear Algebra", ...
                 "ODEs and DAEs", ...
                 "Exit");

  if (choice == 1)
    demo_graphics ();
  elseif (choice == 2)
    demo_linear_algebra ();
  elseif (choice == 3)
    demo_odes ();
  elseif (choice == 4)
    printf ("\nGoodbye and good luck!\n\n");
    fflush (stdout);
    break;
  endif

endwhile
