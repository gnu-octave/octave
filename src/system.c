#if defined (__CYGWIN32__)

#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>

int
system (const char *cmd)
{
  pid_t pid;

  int status = 1;

  struct sigaction ignore, saved_sigint, saved_sigquit;

  sigset_t child_mask, saved_mask;

  if (cmd)
    {
      ignore.sa_handler = SIG_IGN;

      sigemptyset (&ignore.sa_mask);

      ignore.sa_flags = 0;

      if (sigaction (SIGINT, &ignore, &saved_sigint) < 0)
	return -1;

      if (sigaction (SIGQUIT, &ignore, &saved_sigquit) < 0)
	return -1;

      sigemptyset (&child_mask);

      sigaddset (&child_mask, SIGCHLD);

      if (sigprocmask (SIG_BLOCK, &child_mask, &saved_mask) < 0)
	return -1;

      if ((pid = fork ()) < 0)
	status = -1;
      else if (pid == 0)
	{
	  sigaction (SIGINT, &saved_sigint, 0);
	  sigaction (SIGQUIT, &saved_sigquit, 0);

	  sigprocmask (SIG_SETMASK, &saved_mask, 0);

	  execl ("/bin/sh", "sh", "-c", cmd, 0);

	  exit (127);
	}
      else
	{
	  while (waitpid (pid, &status, 0) < 0)
	    {
	      if (errno != EINTR)
		{
		  status = -1;
		  break;
		}
	    }
	}

      if (sigaction (SIGINT, &saved_sigint, 0) < 0)
	return -1;

      if (sigaction (SIGQUIT, &saved_sigquit, 0) < 0)
	return -1;

      if (sigprocmask (SIG_SETMASK, &saved_mask, 0) < 0)
	return -1;      
    }

  return status;
}

#if defined (TEST)
int
main (void)
{
  system ("info");
  while (1)
    {
      printf ("foo-i-hithere\n");
      sleep (1);
    }
  return 0;
}
#endif

#endif
