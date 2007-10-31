Octave works best with gnuplot 4.2, which is available from
http://www.gnuplot.info.

Octave now sends data over the same pipe that is used to send commands
to gnuplot.  While this avoids the problem of cluttering /tmp with
data files, it is no longer possible to use the mouse to zoom in on
plots.  This is a limitation of gnuplot, which is unable to zoom when
the data it plots is not stored in a file.  Some work has been done to
fix this problem in newer versions of gnuplot (> 4.2.2).  See for
example, this thread

  http://www.nabble.com/zooming-of-inline-data-tf4357017.html#a12416496

on the gnuplot development list.


John W. Eaton
jwe@bevo.che.wisc.edu
University of Wisconsin-Madison
Department of Chemical Engineering

Last updated: Wed, 31 Oct 2007 16:28:39 EDT
