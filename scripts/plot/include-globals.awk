/^ *__uiobject_globals__;?.*$/ {
  print "  ## BEGIN text from __uiobject_globals__.m";
  system (sprintf ("cat %s", file));
  print "  ## END text from __uiobject_globals__.m";
  next;
} {
  print $0;
}
