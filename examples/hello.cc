// hello.cc -- example of a dynamically linked function for Octave.

// To use this file, your version of Octave must support dynamic
// linking.  To find out if it does, type the command
//
//   octave_config_info ("ENABLE_DYNAMIC_LINKING")
//
// at the Octave prompt.  Support for dynamic linking is included if
// this expression returns the string "true".
//
// To compile this file, type the command
//
//   mkoctfile hello.cc
//
// at the shell prompt.  The script mkoctfile should have been
// installed along with Octave.  Running it will create a file called
// hello.oct that can be loaded by Octave.  To test the hello.oct
// file, start Octave and type the command
//
//   hello ("easy as", 1, 2, 3)
//
// at the Octave prompt.  Octave should respond by printing
//
//   Hello, world!
//   easy as
//   1
//   2
//   3
//   ans = 3

// Additional examples are available in the files in the src directory
// of the Octave distribution that use the macro DEFUN_DLD_BUILTIN.
// Currently, this includes the files
//
//   balance.cc  fft.cc      ifft.cc     minmax.cc   sort.cc
//   chol.cc     fft2.cc     ifft2.cc    pinv.cc     svd.cc
//   colloc.cc   filter.cc   inv.cc      qr.cc       syl.cc
//   dassl.cc    find.cc     log.cc      quad.cc
//   det.cc      fsolve.cc   lsode.cc    qzval.cc
//   eig.cc      givens.cc   lu.cc       rand.cc
//   expm.cc     hess.cc     minmax.cc   schur.cc
//
// The difference between DEFUN_DLD and DEFUN_DLD_BUILTIN is that
// DEFUN_DLD_BUILTIN can define a built-in function that is not
// dynamically loaded if the operating system does not support dynamic
// linking.  To define your own dynamically linked functions you
// should use DEFUN_DLD.

#include <octave/config.h>

#include <iostream>

#include <octave/defun-dld.h>
#include <octave/error.h>
#include <octave/oct-obj.h>
#include <octave/pager.h>
#include <octave/symtab.h>
#include <octave/variables.h>

// DEFUN_DLD and the macros that it depends on are defined in the
// files defun-dld.h, defun.h, and defun-int.h.

// Note that the third parameter (nargout) is not used, so it is
// omitted from the list of arguments to DEFUN_DLD in order to avoid
// the warning from gcc about an unused function parameter. 

DEFUN_DLD (hello, args, ,
  "[...] = hello (...)\n\
\n\
Print greeting followed by the values of all the arguments passed.\n\
Returns all arguments in reverse order.")
{
  // The list of values to return.  See the declaration in oct-obj.h

  octave_value_list retval;

  // This stream is normally connected to the pager.

  octave_stdout << "Hello, world!\n";

  // The arguments to this function are available in args.

  int nargin = args.length ();

  // The octave_value_list class is a zero-based array of octave_value
  // objects.  The declaration for the octave_value class is in the
  // file ov.h.  The print() method will send its output to
  // octave_stdout, so it will also end up going through the pager.

  for (int i = 0; i < nargin; i++)
    {
      octave_value tmp = args (i);
      tmp.print (octave_stdout);
      retval (nargin-i-1) = tmp;
    }

  return retval;
}
