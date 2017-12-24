## Include script globals.m which defines global variables
define_globals

## Test for presence of a global variable defined above
if (! isglobal ("c"))
  error ("global variable 'c' not defined");
endif

