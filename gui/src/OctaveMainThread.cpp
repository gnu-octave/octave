#include "OctaveMainThread.h"

OctaveMainThread::OctaveMainThread (QObject * parent):QThread (parent)
{
}

void
OctaveMainThread::run ()
{
  int argc = 3;
  const char *argv[] = { "octave", "--interactive", "--line-editing" };
  octave_main (argc, (char **) argv, 1);
  emit ready();
  main_loop ();
  clean_up_and_exit (0);
}
