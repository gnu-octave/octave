## Automake fails to process "include %reldir%/module.mk" in the directory
## above.  All of the commands which would normally be in this file were
## manually placed in scripts/module.mk to avoid using the "include" directive.
##
## This is an Automake bug.  Automake has switched to a Perl backend which uses
## the following pattern to detect a path:
##
## my $PATH_PATTERN = '(\w|[+/.-])+';
##
## This pattern only includes alphanumeric, '_', and [+/.-], but not "@".
