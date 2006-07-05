/*

Copyright (C) 2001, 2006 Paul Kienzle

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

// This code was originally distributed as part of Octave Forge under
// the following terms:
//
// Author: Paul Kienzle
// I grant this code to the public domain.
// 2001-03-22
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
// USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
// OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
// SUCH DAMAGE.

#include <cfloat>
#include <csetjmp>
#include <cstdlib>

#include <iomanip>
#include <set>
#include <string>

typedef void *Pix;
typedef std::set<Pix> MemSet;

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "oct.h"
#include "pager.h"
#include "f77-fcn.h"
#include "unwind-prot.h"
#include "lo-mappers.h"
#include "lo-ieee.h"
#include "parse.h"
#include "toplev.h"
#include "variables.h"
#include "oct-map.h"
#include "str-vec.h"

// mex file context
//
// Class mex keeps track of all memory allocated and frees anything
// not explicitly marked persistent when the it is destroyed.  It also
// maintains the setjump/longjump buffer required for non-local exit
// from the mex file, and any other state local to this instance of
// the mex function invocation.
class mxArray;

// Prototypes for external functions.  Must declare mxArray as a class
// before including this file.
#include "mexproto.h"

class mex
{
public:

  mex (void) { }

  ~mex (void)
  {
    if (! memlist.empty ())
      error("mex: no cleanup performed");
  }

  // free all unmarked pointers obtained from malloc and calloc
  static void cleanup (void *context);

  // allocate a pointer, and mark it to be freed on exit
  Pix malloc (int n);

  // allocate a pointer to be freed on exit, and initialize to 0
  Pix calloc (int n, int t);

  // reallocate a pointer obtained from malloc or calloc
  Pix realloc (Pix ptr, int n);

  // free a pointer obtained from malloc or calloc
  void free (Pix ptr);

  // mark a pointer so that it will not be freed on exit
  void persistent (Pix ptr) { unmark (ptr); }

  // make a new array value and initialize it with zeros; it will be
  // freed on exit unless marked as persistent
  mxArray *make_value (int nr, int nc, int cmplx);

  // make a new array value and initialize from an octave value; it will be
  // freed on exit unless marked as persistent
  mxArray *make_value (const octave_value&);

  // make a new structure value and initialize with empty matrices
  // FIXME does this leak memory?  Is it persistent?
  mxArray *make_value (int nr, int nc, const string_vector& keys);

  // free an array and its contents
  void free_value (mxArray *ptr);

  // mark an array and its contents so it will not be freed on exit
  void persistent (mxArray *ptr);

  // 1 if error should be returned to MEX file, 0 if abort
  int trap_feval_error;

  // longjmp return point if mexErrMsgTxt or error
  jmp_buf jump;

  // trigger a long jump back to the mex calling function
  void abort (void) { longjmp (jump, 1); }

private:

  // list of memory resources that need to be freed upon exit
  MemSet memlist;

  // mark a pointer to be freed on exit
  void mark (Pix p);

  // unmark a pointer to be freed on exit, either because it was
  // made persistent, or because it was already freed
  void unmark (Pix p);
};

// Current context
mex *__mex = 0;

// free all unmarked pointers obtained from malloc and calloc
void
mex::cleanup (Pix ptr)
{
  mex *context = static_cast<mex *> (ptr);

  for (MemSet::iterator p = context->memlist.begin ();
	p != context->memlist.end (); p++)
    ::free (*p);

  context->memlist.clear ();
}

// mark a pointer to be freed on exit
void
mex::mark (Pix p)
{
#ifdef DEBUG
  if (memlist.find (p) != memlist.end ())
    warning ("%s: double registration ignored", mexFunctionName ());
#endif

  memlist.insert (p);
}

// unmark a pointer to be freed on exit, either because it was
// made persistent, or because it was already freed
void
mex::unmark (Pix p)
{
#ifdef DEBUG
  if (memlist.find (p) != memlist.end ())
    warning ("%s: value not marked", mexFunctionName ());
#endif

  memlist.erase (p);
}

// allocate a pointer, and mark it to be freed on exit
Pix
mex::malloc (int n)
{
  if (n == 0)
    return 0;
#if 0
  // FIXME -- how do you allocate and free aligned, non-typed
  // memory in C++?
  Pix ptr = Pix (new double[(n+sizeof(double)-1)/sizeof(double)]);
#else
  // FIXME -- can we mix C++ and C-style heap management?
  Pix ptr = ::malloc (n);

  if (! ptr)
    {
      // FIXME -- could use "octave_new_handler();" instead
      error ("%s: out of memory", mexFunctionName ());
      abort ();
    }
#endif

  mark (ptr);

  return ptr;
}

// allocate a pointer to be freed on exit, and initialize to 0
Pix
mex::calloc (int n, int t)
{
  Pix v = malloc (n*t);

  memset (v, 0, n*t);

  return v;
}

// reallocate a pointer obtained from malloc or calloc
Pix
mex::realloc (Pix ptr, int n)
{
#if 0
  error ("%s: cannot reallocate using C++ new/delete operations",
	 mexFunctionName ());
  abort ();
#else
  Pix v = 0;
  if (n == 0)
    free (ptr);
  else if (! ptr)
    v = malloc (n);
  else
    {
      v = ::realloc (ptr, n);
      MemSet::iterator p = memlist.find (ptr);
      if (v && p != memlist.end ())
	{
	  memlist.erase (p);
	  memlist.insert (v);
	}
    }
#endif
  return v;
}

// free a pointer obtained from malloc or calloc
void
mex::free (Pix ptr)
{
  unmark (ptr);
#if 0
  delete [] ptr;
#else
  ::free (ptr);
#endif
}

// mxArray data type
//
// Class mxArray is not much more than a struct for keeping together
// dimensions and data.  It doesn't even ensure consistency between
// the dimensions and the data.  Unfortunately you can't do better
// than this without restricting the operations available in Matlab
// for directly manipulating its mxArray type.

typedef unsigned short mxChar;
const int mxMAXNAM=64;

class mxArray
{
public:

  mxArray(void)
  {
    nr = nc = -1;
    pr = pi = NULL;
    keys = NULL;
    pmap = NULL;
    isstr = false;
    aname[0] = '\0';
  }

  ~mxArray (void)
  { 
    if (pmap)
      {
      // FIXME why don't string_vectors work?
	for (int i = 0; i < pmap->length (); i++)
	  delete [] keys[i];

	delete [] keys;
      }
  }

  octave_value as_octave_value (void) const;

  int rows (void) const { return nr; }
  int columns (void) const { return nc; }
  void rows (int r) { nr = r; }
  void columns (int c) { nc = c; }
  int dims (void) const { return 2; }

  double *imag (void) const { return pi; }
  double *real (void) const { return pr; }
  void imag (double *p) { pi = p; }
  void real (double *p) { pr = p; }

  bool is_empty (void) const { return nr==0 || nc==0; }
  bool is_numeric (void) const { return ! isstr && (pr || nr == 0 || nc == 0); }
  bool is_complex (void) const { return pi; }
  bool is_sparse (void) const { return false; }
  bool is_struct (void) const { return pmap; }

  bool is_string (void) const { return isstr; }
  void is_string (bool set) { isstr = set; }

  const char *name (void) const { return aname; }
  void name (const char *nm)
  {
    strncpy (aname, nm, mxMAXNAM);
    aname[mxMAXNAM]='\0';
  }

  // Structure support functions.  Matlab uses a fixed field order
  // (the order in which the fields were added?), but Octave uses an
  // unordered hash for structs.  We can emulate a fixed field order
  // using pmap->keys(), which returns a string_vector of key names,
  // but these keys will not be in the same order as the keys given in
  // mxCreateStruct*.  Within the creating function, we can populate
  // the key name vector in the order given, so the only problem will
  // be those functions which assume the key order is maintained
  // between calls from Matlab.  Unfortunately, these might exist and
  // I can't detect them :-(

  // Return the map value
  Octave_map *map (void) const { return pmap; }

  // New structure with the given presumed field order (CreateStruct call)
  void map (Octave_map *p, const string_vector& mapkeys)
  {
    pmap = p;
    keys = mapkeys.c_str_vec ();
  }

  // New structure with unknown field order (passed in from Octave)
  void map (Octave_map *p)
  { 
    pmap = p;
    if (p)
      keys = p->keys().c_str_vec ();
  }

  // Get field given field name
  mxArray *field (const std::string& key_arg, const int index) const
  {
    if (pmap && pmap->contains (key_arg))
      return __mex->make_value (pmap->contents(key_arg)(index));
    else
      return 0;
  }

  // Set field given field name
  void field (const std::string& key_arg, const int index, mxArray *value)
  {
    if (pmap) 
      pmap->assign (octave_value (index+1), 
		    key_arg, Cell (value->as_octave_value ()));

    if (error_state)
      __mex->abort ();
  }

  // Return number of fields in structure
  int num_keys(void) const { return pmap ? pmap->length () : 0; } 

  // Return field name from field number
  const std::string key (const int key_num) const
  {
    if (key_num >= 0 && key_num < pmap->length ())
      return keys[key_num];
    else
      return 0;
  }
  // Return field number from field name
  int key (const std::string &key_name) const
  {
    for (int i = 0; i < pmap->length (); i++)
      if (key_name == std::string (keys[i]))
	return i;

    return -1;
  }

  // Get field using field number
  mxArray *field (const int key_num, const int index) const
  {
    if (key_num >= 0 && key_num < pmap->length ())
      return field (keys[key_num], index);
    else
      return 0;
  }

  // Set field using field number
  void field (const int key_num, const int index , mxArray *value)
  {
    if (key_num >= 0 && key_num < pmap->length ())
      field (keys[key_num], index, value);
  }

private:
  int nr;
  int nc;
  double *pr;
  double *pi;
  // FIXME -- need to have a typeid here instead of complex logic on
  // isstr, pmap, pr, pi, etc.
  Octave_map *pmap;
  // string_vector keys;
  char **keys;
  bool isstr;
  char aname[mxMAXNAM+1];
};

octave_value
mxArray::as_octave_value (void) const
{
  octave_value ret;

  if (isstr)
    {
      charMatrix chm (nr, nc);
      char *pchm = chm.fortran_vec ();
      for (int i=0; i < nr*nc; i++)
	pchm[i] = NINT (pr[i]);
      ret = octave_value (chm, true);
    }
  else if (pmap)
    {
      ret = octave_value (*pmap);
    }
  else if (pi)
    {
      ComplexMatrix cm (nr, nc);
      Complex *pcm = cm.fortran_vec ();
      for (int i=0; i < nr*nc; i++)
	pcm[i] = Complex (pr[i], pi[i]);
      ret = cm;
    }
  else if (pr)
    {
      Matrix m (nr, nc);
      double *pm = m.fortran_vec ();
      memcpy (pm, pr, nr*nc*sizeof(double));
      ret = m;
    }
  else
    ret = Matrix (0, 0);

  return ret;
}


// mex/mxArray interface

// Make a new array value and initialize from an octave value; it will
// be freed on exit unless marked as persistent.

mxArray *mex::make_value(const octave_value &ov)
{
  int nr = -1;
  int nc = -1;
  double *pr = 0;
  double *pi = 0;
  Octave_map *pmap = 0;

  if (ov.is_numeric_type () || ov.is_string ())
    {
      nr = ov.rows ();
      nc = ov.columns ();
    }
  if (ov.is_map ())
    {
      pmap = new Octave_map (ov.map_value ());
      nr = ov.rows ();
      nc = ov.columns ();
    }
  else if (nr > 0 && nc > 0)
    {
      if (ov.is_string ())
	{
	  // FIXME - must use 16 bit unicode to represent strings.
	  const Matrix m (ov.matrix_value (1));
	  pr = static_cast<double *> (malloc(nr*nc*sizeof(double)));
	  memcpy (pr, m.data (), nr*nc*sizeof(double));
	}
      else if (ov.is_complex_type ())
	{
	  // FIXME -- may want to consider lazy copying of the
	  // matrix, but this will only help if the matrix is being
	  // passed on to octave via callMATLAB later.
	  const ComplexMatrix cm (ov.complex_matrix_value ());
	  const Complex *pz = cm.data ();
	  pr = static_cast<double *> (malloc (nr*nc*sizeof(double)));
	  pi = static_cast<double *> (malloc (nr*nc*sizeof(double)));
	  for (int i = 0; i < nr*nc; i++)
	    {
	      pr[i] = real (pz[i]);
	      pi[i] = imag (pz[i]);
	    }
	}
      else
	{
	  const Matrix m (ov.matrix_value ());
	  pr = static_cast<double *> (malloc (nr*nc*sizeof(double)));
	  memcpy (pr, m.data (), nr*nc*sizeof(double));
	}
    }

  mxArray *value = static_cast<mxArray *> (malloc (sizeof(mxArray)));

  value->is_string (ov.is_string ());
  value->real (pr);
  value->imag (pi);
  value->map (pmap);
  value->rows (nr);
  value->columns (nc);
  value->name ("");

  return value;
}

// Make a new array value and initialize it with zeros; it will be
// freed on exit unless marked as persistent.

mxArray *
mex::make_value (int nr, int nc, int cmplx)
{

  mxArray *value = static_cast<mxArray *> (malloc (sizeof(mxArray)));
  double *p = static_cast<double *>  (calloc (nr*nc, sizeof(double)));

  value->real (p);
  if (cmplx)
    value->imag (static_cast<double *> (calloc (nr*nc, sizeof(double))));
  else
    value->imag (static_cast<double *> (Pix (0)));
  value->rows (nr);
  value->columns (nc);
  value->is_string (false);
  value->map (0);
  value->name ("");

  return value;
}

// Make a new structure value and initialize with empty matrices
// FIXME does this leak memory?  Is it persistent?

mxArray *
mex::make_value (int nr, int nc, const string_vector& keys)
{
  if (keys.length () == 0)
    return 0;

  Cell empty (nr, nc);
  Octave_map *pmap = new Octave_map (keys[0], empty);
  for (int i=1; i < keys.length (); i++)
    pmap->assign (keys[i], empty);

  mxArray *value = static_cast<mxArray *> (malloc (sizeof(mxArray)));

  value->rows (nr);
  value->columns (nc);
  value->map (pmap, keys);

  return value;
}

// free an array and its contents

void
mex::free_value (mxArray *ptr)
{
  free (ptr->real ());
  free (ptr->imag ());
  free (ptr);
}

// mark an array and its contents so it will not be freed on exit

void
mex::persistent (mxArray *ptr)
{
  persistent (Pix (ptr->real ()));
  persistent (Pix (ptr->imag ()));
  persistent (Pix (ptr));
}


// Octave interface to mex files

#if 0
// Don't bother trapping stop/exit
// To trap for STOP in fortran code, this needs to be registered with atexit
static void mex_exit()
{
  if (__mex)
    {
      error ("%s: program aborted", mexFunctionName ());
      __mex->abort ();
    }
}
#endif

typedef void (*cmex_fptr) (int nlhs, mxArray **plhs, int nrhs, mxArray **prhs);
typedef F77_RET_T (*fmex_fptr) (int& nlhs, mxArray **plhs, int& nrhs, mxArray **prhs);

enum callstyle { use_fortran, use_C };

octave_value_list
call_mex (callstyle cs, void *f, const octave_value_list& args, int nargout)
{
#if 0
  // Don't bother trapping stop/exit
  // FIXME -- should really push "mex_exit" onto the octave
  // atexit stack before we start and pop it when we are through, but
  // the stack handle isn't exported from toplev.cc, so we can't.  mex_exit
  // would have to be declared as DEFUN(mex_exit,,,"") of course.
  static bool unregistered = true;
  if (unregistered)
    {
      atexit (mex_exit);
      unregistered = false;
    }
#endif

  // Use nargout+1 since even for zero specified args, still want to
  // be able to return an ans.

  int nargin = args.length ();
  OCTAVE_LOCAL_BUFFER(mxArray*, argin, nargin);
  for (int i = 0; i < nargin; i++)
    argin[i] = 0;

  int nout = nargout == 0 ? 1 : nargout;
  OCTAVE_LOCAL_BUFFER(mxArray*, argout, nout);
  for (int i = 0; i < nout; i++)
    argout[i] = 0;

  mex context;
  unwind_protect::add (mex::cleanup, Pix (&context));

  for (int i = 0; i < nargin; i++)
    argin[i] = context.make_value (args(i));

  // Save old mex pointer.
  unwind_protect_ptr (__mex);

  if (setjmp (context.jump) == 0)
    {
      __mex = &context;

      if (cs == use_fortran)
	{
	  fmex_fptr fcn = FCN_PTR_CAST (fmex_fptr, f);

	  int tmp_nargout = nargout;
	  int tmp_nargin = nargin;

	  fcn (tmp_nargout, argout, tmp_nargin, argin);
	}
      else
	{
	  cmex_fptr fcn = FCN_PTR_CAST (cmex_fptr, f);

	  fcn (nargout, argout, nargin, argin);
	}
    }

  // Restore old mex pointer.
  unwind_protect::run ();

  // Convert returned array entries back into octave values.

  octave_value_list retval;

  if (! error_state)
    {
      if (nargout == 0 && argout[0])
	retval(0) = argout[0]->as_octave_value ();
      else
	{
	  retval.resize (nargout);

	  for (int i = 0; i < nargout; i++)
	    if (argout[i])
	      retval(i) = argout[i]->as_octave_value ();
	}
    }

  // Clean up mex resources.
  unwind_protect::run ();

  return retval;
}

octave_value_list
Fortran_mex (void *f, const octave_value_list& args, int nargout)
{
  return call_mex (use_fortran, f, args, nargout);
}

octave_value_list
C_mex (void *f, const octave_value_list& args, int nargout)
{
  return call_mex (use_C, f, args, nargout);
}

// C interface to mex functions:

extern "C" {

const char *
mexFunctionName (void)
{
  static char *retval = 0;

  delete [] retval;

  octave_function *fcn = octave_call_stack::current ();

  if (fcn)
    {
      std::string nm = fcn->name ();
      retval = strsave (nm.c_str ());
    }
  else
    retval = strsave ("unknown");

  return retval;
}

void
mexErrMsgTxt (const char *s)
{
  if (s && strlen (s) > 0)
    error ("%s: %s", mexFunctionName (), s);
  else
    // Just set the error state; don't print msg.
    error ("");

  __mex->abort();
}

void
mexErrMsgIdAndTxt (const char *id, const char *s)
{
  if (s && strlen (s) > 0)
    error_with_id (id, "%s: %s", mexFunctionName (), s);
  else
    // Just set the error state; don't print msg.
    error ("");

  __mex->abort();
}

void
mexWarnMsgTxt (const char *s)
{
  warning ("%s", s);
}

void
mexWarnMsgIdAndTxt (const char *id, const char *s)
{
  warning_with_id (id, "%s", s);
}

void
mexPrintf (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  octave_vformat (octave_stdout, fmt, args);
  va_end (args);
}

// Floating point representation.

int mxIsFinite (const double v) { return lo_ieee_finite (v) != 0; }
int mxIsInf (const double v) { return lo_ieee_isinf (v) != 0; }
int mxIsNaN (const double v) { return lo_ieee_isnan (v) != 0; }

double mxGetEps (void) { return DBL_EPSILON; }
double mxGetInf (void) { return lo_ieee_inf_value (); }
double mxGetNaN (void) { return lo_ieee_nan_value (); }

int
mexEvalString (const char *s)
{
  int parse_status;
  octave_value_list ret;
  ret = eval_string (s, false, parse_status, 0);
  if (parse_status || error_state)
    {
      error_state = 0;
      return 1;
    }
  else
    return 0;
}

int
mexCallMATLAB (int nargout, mxArray *argout[],
	       int nargin, mxArray *argin[],
	       const char *fname)
{
  octave_value_list args;

  // FIXME -- do we need unwind protect to clean up args?  Off hand, I
  // would say that this problem is endemic to Octave and we will
  // continue to have memory leaks after Ctrl-C until proper exception
  // handling is implemented.  longjmp() only clears the stack, so any
  // class which allocates data on the heap is going to leak.

  args.resize (nargin);

  for (int i = 0; i < nargin; i++)
    args(i) = argin[i]->as_octave_value ();

  octave_value_list retval = feval (fname, args, nargout);

  if (error_state && __mex->trap_feval_error == 0)
    {
      // FIXME -- is this the correct way to clean up?  abort() is
      // going to trigger a long jump, so the normal class destructors
      // will not be called.  Hopefully this will reduce things to a
      // tiny leak.  Maybe create a new octave memory tracer type
      // which prints a friendly message every time it is
      // created/copied/deleted to check this.

      args.resize (0);
      retval.resize (0);
      __mex->abort ();
    }

  int num_to_copy = retval.length ();

  if (nargout < retval.length ())
    num_to_copy = nargout;

  for (int i = 0; i < num_to_copy; i++)
    {
      // FIXME -- it would be nice to avoid copying the value here,
      // but there is no way to steal memory from a matrix, never mind
      // that matrix memory is allocated by new[] and mxArray memory
      // is allocated by malloc().
      argout[i] = __mex->make_value (retval (i));
    }

  while (num_to_copy < nargout)
    argout[num_to_copy++] = 0;

  if (error_state)
    {
      error_state = 0;
      return 1;
    }
  else
    return 0;
}

void mexSetTrapFlag (int flag) { __mex->trap_feval_error = flag;  }

Pix mxMalloc (int n) { return __mex->malloc(n);  }
Pix mxCalloc (int n, int size) { return __mex->calloc (n, size); }
Pix mxRealloc (Pix ptr, int n) { return __mex->realloc (ptr, n); }
void mxFree (Pix ptr) { __mex->free (ptr); }
void mexMakeMemoryPersistent (Pix ptr) { __mex->persistent (ptr); }

mxArray *
mxCreateDoubleMatrix (int nr, int nc, int iscomplex)
{
  return __mex->make_value(nr, nc, iscomplex);
}

mxArray *
mxCreateDoubleScalar (double val)
{ 
  mxArray *ptr = mxCreateDoubleMatrix (1, 1, 0);
  *mxGetPr (ptr) = val;
  return ptr;
}

mxArray *
mxCreateLogicalScalar (int val)
{ 
  mxArray *ptr = mxCreateDoubleMatrix (1, 1, 0);
  *mxGetPr (ptr) = val;
  return ptr;
}

void mxDestroyArray (mxArray *v) { __mex->free (v);  }

mxArray *
mxDuplicateArray (const mxArray *ptr)
{
  return __mex->make_value (ptr->as_octave_value ());
}

void mexMakeArrayPersistent (mxArray *ptr) { __mex->persistent (ptr); }

int mxIsChar (const mxArray *ptr) { return ptr->is_string (); }
int mxIsSparse (const mxArray *ptr) { return ptr->is_sparse (); }
int mxIsFull(const mxArray *ptr) { return !ptr->is_sparse (); }
int mxIsNumeric (const mxArray *ptr) { return ptr->is_numeric (); }
int mxIsComplex (const mxArray *ptr) { return ptr->is_complex (); }
int mxIsDouble (const mxArray *) { return true; }
int mxIsEmpty (const mxArray *ptr) { return ptr->is_empty (); }

int
mxIsLogicalScalar (const mxArray *ptr)
{
  return (ptr->is_numeric ()
	  && ptr->rows () == 1 && ptr->columns () == 1
	  && *ptr->real ());
}

double *mxGetPr (const mxArray *ptr) { return ptr->real (); }
double *mxGetPi (const mxArray *ptr) { return ptr->imag (); }
int mxGetM (const mxArray *ptr) { return ptr->rows (); }
int mxGetN (const mxArray *ptr) { return ptr->columns (); }
int mxGetNumberOfDimensions (const mxArray *ptr) { return ptr->dims (); }
int mxGetNumberOfElements (const mxArray *ptr) { return ptr->rows () * ptr->columns (); }
void mxSetM (mxArray *ptr, int M) { ptr->rows (M); }
void mxSetN (mxArray *ptr, int N) { ptr->columns (N); }
void mxSetPr (mxArray *ptr, double *pr) { ptr->real (pr); }
void mxSetPi (mxArray *ptr, double *pi) { ptr->imag (pi); }

double
mxGetScalar (const mxArray *ptr)
{
  double *pr =  ptr->real ();
  if (! pr)
    mexErrMsgTxt ("calling mxGetScalar on an empty matrix");
  return pr[0];
}

int
mxGetString (const mxArray *ptr, char *buf, int buflen)
{
  if (ptr->is_string ())
    {
      int nr = ptr->rows ();
      int nc = ptr->columns ();
      int n = nr*nc < buflen ? nr*nc : buflen;
      const double *pr = ptr->real ();
      for (int i = 0; i < n; i++)
	buf[i] = NINT (pr[i]);
      if (n < buflen)
	buf[n] = '\0';
      return n >= buflen;
    }
  else
    return 1;
}

char *
mxArrayToString (const mxArray *ptr)
{
  int nr = ptr->rows ();
  int nc = ptr->columns ();
  int n = nr*nc*sizeof(mxChar)+1;
  char *buf = static_cast<char *> (mxMalloc (n));
  if (buf)
    mxGetString (ptr, buf, n);

  return buf;
}

mxArray *
mxCreateString (const char *str)
{
  int n = strlen (str);
  mxArray *m = __mex->make_value (1, n, 0);
  if (! m)
    return m;
  m->is_string (true);

  double *pr = m->real ();
  for (int i = 0; i < n; i++)
    pr[i] = str[i];

  return m;
}

mxArray *
mxCreateCharMatrixFromStrings (int n, const char **str)
{
  // Find length of the individual strings.
  Array<int> len (n);

  for (int i = 0; i < n; i++)
    len(i) = strlen (str[i]);

  // Find maximum length.
  int maxlen = 0;
  for (int i = 0; i < n; i++)
    if (len(i) > maxlen)
      maxlen = len(i);

  // Need a place to copy them.
  mxArray *m = __mex->make_value (n, maxlen, 0);
  if (! m)
    return m;
  m->is_string (true);

  // Do the copy (being sure not to exceed the length of any of the
  // strings).
  double *pr = m->real ();
  for (int j = 0; j < maxlen; j++)
    for (int i = 0; i < n; i++)
      if (j < len(i))
	*pr++ = str[i][j];
      else
	*pr++ = '\0';

  return m;
}

int
mexPutVariable (const char *space, const char *name, mxArray *ptr)
{
  if (! ptr)
    return 1;

  if (! name)
    return 1;

  if (name[0] == '\0')
    name = ptr->name ();

  if (! name || name[0] == '\0')
    return 1;

  if (! strcmp (space, "global"))
    set_global_value (name, ptr->as_octave_value ());
  else if (! strcmp (space, "caller"))
    {
      // FIXME -- this belongs in variables.cc.
      symbol_record *sr = curr_sym_tab->lookup (name, true);
      if (sr)
	sr->define (ptr->as_octave_value ());
      else
	panic_impossible ();
    }
  else if (! strcmp (space, "base"))
    mexErrMsgTxt ("mexPutVariable: 'base' symbol table not implemented");
  else
    mexErrMsgTxt ("mexPutVariable: symbol table does not exist");
  return 0;
}

mxArray *
mexGetVariable (const char *space, const char *name)
{
  mxArray *retval = 0;

  // FIXME -- this should be in variable.cc, but the correct
  // functionality is not exported.  Particularly, get_global_value()
  // generates an error if the symbol is undefined.

  symbol_record *sr = 0;

  if (! strcmp (space, "global"))
    sr = global_sym_tab->lookup (name);
  else if (! strcmp (space, "caller"))
    sr = curr_sym_tab->lookup (name);
  else if (! strcmp (space, "base"))
    mexErrMsgTxt ("mexGetVariable: 'base' symbol table not implemented");
  else
    mexErrMsgTxt ("mexGetVariable: symbol table does not exist");

  if (sr)
    {
      octave_value sr_def = sr->def ();

      if (sr_def.is_defined ())
	{
	  retval = __mex->make_value (sr_def);
	  retval->name (name);
	}
    }

  return retval;
}

const mxArray *
mexGetVariablePtr (const char *space, const char *name)
{
  return mexGetVariable (space, name);
}

const char *mxGetName (const mxArray *ptr) { return ptr->name (); }

void mxSetName (mxArray *ptr, const char*nm) { ptr->name (nm); }

mxArray *
mxCreateStructMatrix (int nr, int nc, int num_keys, const char **keys)
{
  const string_vector ordered_keys (keys, num_keys);
  mxArray *m = __mex->make_value (nr, nc, ordered_keys);
  return m;
}

mxArray *
mxGetField (const mxArray *ptr, int index, const char *key)
{
  return ptr->field (key, index);
}

void
mxSetField (mxArray *ptr, int index, const char *key, mxArray *val)
{
  ptr->field (key, index, val);
}

int mxGetNumberOfFields (const mxArray *ptr) { return ptr->num_keys (); }
int mxIsStruct (const mxArray *ptr) { return ptr->is_struct (); }

const char *
mxGetFieldNameByNumber (const mxArray *ptr, int key_num) 
{
  return ptr->key(key_num).c_str ();
}

int
mxGetFieldNumber (const mxArray *ptr, const char *key)
{
  return ptr->key (key);
}

mxArray *
mxGetFieldByNumber (const mxArray *ptr, int index, int key_num)
{
  return ptr->field (key_num, index);
}

void
mxSetFieldByNumber (mxArray *ptr, int index, int key_num, mxArray *val)
{
  return ptr->field (key_num,index,val);
}

} // extern "C"

// Fortran interface to mex functions
//
// Where possible, these call the equivalent C function since that API
// is fixed.  It costs and extra function call, but is easier to
// maintain.

extern "C" {

void F77_FUNC (mexerrmsgtxt, MEXERRMSGTXT) (const char *s, long slen)
{
  if (slen > 1 || (slen == 1 && s[0] != ' ') )
    error ("%s: %.*s", mexFunctionName (), slen, s);
  else
    // Just set the error state; don't print msg.
    error ("");

  __mex->abort();
}

void F77_FUNC (mexprintf, MEXPRINTF) (const char *s, long slen)
{
  mexPrintf ("%.*s\n", slen, s);
}

int F77_FUNC (mexisfinite, MEXISFINITE) (double v) { return mxIsFinite (v); }
int F77_FUNC (mexisinf, MEXISINF) (double v) { return mxIsInf (v); }
int F77_FUNC (mexisnan, MEXISNAN) (double v) { return mxIsNaN (v); }

double F77_FUNC (mexgeteps, MEXGETEPS) (void) { return mxGetEps (); }
double F77_FUNC (mexgetinf, MEXGETINF) (void) { return mxGetInf (); }
double F77_FUNC (mexgetnan, MEXGETNAN) (void) { return mxGetNaN (); }

// Array access:

Pix F77_FUNC (mxcreatefull, MXCREATEFULL)
  (const int& nr, const int& nc, const int& iscomplex)
{
  return mxCreateDoubleMatrix (nr, nc, iscomplex);
}

void F77_FUNC (mxfreematrix, MXFREEMATRIX) (mxArray* &p)
{
  mxDestroyArray (p);
}

Pix F77_FUNC (mxcalloc, MXCALLOC) (const int& n, const int& size)
{
  return mxCalloc (n, size);
}

void F77_FUNC (mxfree, MXFREE) (const Pix &p) { mxFree (p); }

int F77_FUNC (mxgetm, MXGETM) (const mxArray* &p) { return mxGetM (p); }
int F77_FUNC (mxgetn, MXGETN) (const mxArray* &p) { return mxGetN (p); }

Pix F77_FUNC (mxgetpi, MXGETPI) (const mxArray* &p) { return mxGetPi (p); }
Pix F77_FUNC (mxgetpr, MXGETPR) (const mxArray* &p) { return mxGetPr (p); }

void F77_FUNC (mxsetm, MXSETM) (mxArray* &p, const int& m) { mxSetM (p, m); }
void F77_FUNC (mxsetn, MXSETN) (mxArray* &p, const int& n) { mxSetN (p, n); }

void F77_FUNC (mxsetpi, MXSETPI) (mxArray* &p, double *pi) { mxSetPi (p, pi); }
void F77_FUNC (mxsetpr, MXSETPR) (mxArray* &p, double *pr) { mxSetPr (p, pr); }

int F77_FUNC (mxiscomplex, MXISCOMPLEX) (const mxArray* &p)
{
  return mxIsComplex (p);
}

int F77_FUNC (mxisdouble, MXISDOUBLE) (const mxArray* &p)
{
  return mxIsDouble (p);
}

int F77_FUNC (mxisnumeric, MXISNUMERIC) (const mxArray* &p)
{
  return mxIsNumeric(p);
}

int F77_FUNC (mxisfull, MXISFULL) (const mxArray* &p)
{
  return 1 - mxIsSparse (p);
}

int F77_FUNC (mxissparse, MXISSPARSE) (const mxArray* &p)
{
  return mxIsSparse (p);
}

int F77_FUNC (mxisstring, MXISSTRING) (const mxArray* &p)
{
  return mxIsChar (p);
}

int F77_FUNC (mxgetstring, MXGETSTRING)
  (const mxArray* &ptr, char *str, const int& len)
{
  return mxGetString (ptr, str, len);
}

int F77_FUNC (mexcallmatlab, MEXCALLMATLAB)
  (const int& nargout, mxArray **argout,
   const int& nargin, mxArray **argin,
   const char *fname,
   long fnamelen)
{
  char str[mxMAXNAM+1];
  strncpy (str, fname, (fnamelen < mxMAXNAM ? fnamelen : mxMAXNAM));
  str[fnamelen] = '\0';
  return mexCallMATLAB (nargout, argout, nargin, argin, str);
}

// Fake pointer support:

void F77_FUNC (mxcopyreal8toptr, MXCOPYREAL8TOPTR)
  (const double *d, const int& prref, const int& len)
{
  double *pr = (double *) prref;
  for (int i = 0; i < len; i++)
    pr[i] = d[i];
}

void F77_FUNC (mxcopyptrtoreal8, MXCOPYPTRTOREAL8)
  (const int& prref, double *d, const int& len)
{
  double *pr = (double *) prref;
  for (int i = 0; i < len; i++)
    d[i] = pr[i];
}

void F77_FUNC (mxcopycomplex16toptr, MXCOPYCOMPLEX16TOPTR)
  (const double *d, int& prref, int& piref, const int& len)
{
  double *pr = (double *) prref;
  double *pi = (double *) piref;
  for (int i = 0; i < len; i++)
    {
      pr[i] = d[2*i];
      pi[i] = d[2*i+1];
    }
}

void F77_FUNC (mxcopyptrtocomplex16, MXCOPYPTRTOCOMPLEX16)
  (const int& prref, const int& piref, double *d, const int& len)
{
  double *pr = (double *) prref;
  double *pi = (double *) piref;
  for (int i = 0; i < len; i++)
    {
      d[2*i] = pr[i];
      d[2*i+1] = pi[i];
    }
}

} // extern "C"

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
