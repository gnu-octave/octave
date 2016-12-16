/*

Copyright (C) 2016 Francesco Faccio <francesco.faccio@mail.polimi.it>

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "dColVector.h"
#include "dMatrix.h"
#include "dSparse.h"

#include "defun-dld.h"
#include "error.h"
#include "ov.h"
#include "ov-struct.h"
#include "parse.h"
#include "errwarn.h"

#if defined (HAVE_SUNDIALS)

#  if defined (HAVE_IDA_IDA_H)
#    include <ida/ida.h>
#    include <ida/ida_dense.h>
#    include <ida/ida_klu.h>
#    include <sundials/sundials_sparse.h>
#  endif

#  if defined (HAVE_NVECTOR_NVECTOR_SERIAL_H)
#    include <nvector/nvector_serial.h>
#  endif

namespace octave
{
  class IDA
  {
  public:

    typedef
    ColumnVector (*DAERHSFuncIDA) (const ColumnVector& x,
                                   const ColumnVector& xdot,
                                   realtype t, octave_function *idaf);

    typedef
    Matrix (*DAEJacFuncDense) (const ColumnVector& x,
                               const ColumnVector& xdot, realtype t,
                               realtype cj, octave_function *idaj);

    typedef
    SparseMatrix (*DAEJacFuncSparse) (const ColumnVector& x,
                                      const ColumnVector& xdot,
                                      realtype t, realtype cj,
                                      octave_function *idaj);

    typedef
    Matrix (*DAEJacCellDense) (Matrix *dfdy, Matrix *dfdyp,
                               realtype cj);

    typedef
    SparseMatrix (*DAEJacCellSparse) (SparseMatrix *dfdy,
                                      SparseMatrix *dfdyp, realtype cj);

    //Default
    IDA (void)
      : t0 (0.0), y0 (), yp0 (), havejac (false), havejacfun (false),
        havejacsparse (false), mem (0), num (), ida_fun (0),
        ida_jac (0), dfdy (0), dfdyp (0), spdfdy (0),
        spdfdyp (0), fun (0), jacfun (0), jacspfun (0),
        jacdcell (0), jacspcell (0)
    { };


    IDA (realtype t, ColumnVector y, ColumnVector yp,
         octave_function *ida_fcn, DAERHSFuncIDA daefun)
      : t0 (t), y0 (y), yp0 (yp), havejac (false), havejacfun (false),
        havejacsparse (false), mem (0), num (), ida_fun (ida_fcn),
        ida_jac (0), dfdy (0), dfdyp (0), spdfdy (0),
        spdfdyp (0), fun (daefun), jacfun (0), jacspfun (0),
        jacdcell (0), jacspcell (0)
    { };


    ~IDA (void) { IDAFree (&mem); }

    IDA&
    set_jacobian (octave_function *jac, DAEJacFuncDense j)
    {
      jacfun = j;
      ida_jac = jac;
      havejac = true;
      havejacfun = true;
      havejacsparse = false;
      return *this;
    }

    IDA&
    set_jacobian (octave_function *jac, DAEJacFuncSparse j)
    {
      jacspfun = j;
      ida_jac = jac;
      havejac = true;
      havejacfun = true;
      havejacsparse = true;
      return *this;
    }

    IDA&
    set_jacobian (Matrix *dy, Matrix *dyp, DAEJacCellDense j)
    {
      jacdcell = j;
      dfdy = dy;
      dfdyp = dyp;
      havejac = true;
      havejacfun = false;
      havejacsparse = false;
      return *this;
    }

    IDA&
    set_jacobian (SparseMatrix *dy, SparseMatrix *dyp,
                  DAEJacCellSparse j)
    {
      jacspcell = j;
      spdfdy = dy;
      spdfdyp = dyp;
      havejac = true;
      havejacfun = false;
      havejacsparse = true;
      return *this;
    }

    void
    set_userdata (void);

    void
    initialize (void);

    static ColumnVector
    NVecToCol (N_Vector& v, long int n);

    static N_Vector
    ColToNVec (const ColumnVector& data, long int n);

    void
    set_up (void);

    void
    set_tolerance (ColumnVector& abstol, realtype reltol);

    void
    set_tolerance (realtype abstol, realtype reltol);

    static int
    resfun (realtype t, N_Vector yy, N_Vector yyp,
            N_Vector rr, void *user_data);

    void
    resfun_impl (realtype t, N_Vector& yy,
                 N_Vector& yyp, N_Vector& rr);

    static int
    jacdense (long int Neq, realtype t,  realtype cj,
              N_Vector yy, N_Vector yyp, N_Vector resvec,
              DlsMat JJ, void *user_data, N_Vector tempv1,
              N_Vector tempv2, N_Vector tempv3)
    {
      IDA *self =
        static_cast <IDA *> (user_data);

      self -> jacdense_impl (Neq, t, cj, yy, yyp, JJ);
      return 0;
    }

    void
    jacdense_impl (long int Neq, realtype t, realtype cj,
                   N_Vector& yy, N_Vector& yyp, DlsMat& JJ);

    static int
    jacsparse (realtype t, realtype cj, N_Vector yy, N_Vector yyp,
               N_Vector r, SlsMat Jac, void *user_data, N_Vector tmp1,
               N_Vector tmp2, N_Vector tmp3)
    {
      IDA *self =
        static_cast <IDA *> (user_data);

      self -> jacsparse_impl (t, cj, yy, yyp, Jac);
      return 0;
    }

    void
    jacsparse_impl (realtype t, realtype cj, N_Vector& yy,
                    N_Vector& yyp, SlsMat& Jac);

    void
    set_maxstep (realtype maxstep);

    void
    set_initialstep (realtype initialstep);

    bool
    interpolate (int& cont, Matrix& output, ColumnVector& tout,
                 int refine, realtype tend, bool haveoutputfcn,
                 bool haveoutputsel, octave_function *output_fcn,
                 ColumnVector& outputsel, bool haveeventfunction,
                 octave_function * event_fcn, ColumnVector& te,
                 Matrix& ye, ColumnVector& ie, ColumnVector& oldval,
                 ColumnVector& oldisterminal, ColumnVector& olddir,
                 int& temp, ColumnVector& yold);

    bool
    outputfun (octave_function *output_fcn, bool haveoutputsel,
               const ColumnVector& output, realtype tout, realtype tend,
               ColumnVector& outputsel, std::string flag);


    bool
    event (octave_function *event_fcn,
           ColumnVector& te, Matrix& ye, ColumnVector& ie,
           realtype tsol, const ColumnVector& y, std::string flag,
           const ColumnVector& yp, ColumnVector& oldval,
           ColumnVector& oldisterminal, ColumnVector& olddir,
           int cont, int& temp, realtype told, ColumnVector& yold);

    void
    set_maxorder (int maxorder);

    octave_value_list
    integrate (const int numt, const ColumnVector& tt,
               const ColumnVector& y0, const ColumnVector& yp0,
               const int refine, bool haverefine, bool haveoutputfcn,
               octave_function *output_fcn, bool haveoutputsel,
               ColumnVector& outputsel, bool haveeventfunction,
               octave_function *event_fcn);

    void
    print_stat (void);

  private:

    realtype t0;
    ColumnVector y0;
    ColumnVector yp0;
    bool havejac;
    bool havejacfun;
    bool havejacsparse;
    void *mem;
    int num;
    octave_function *ida_fun;
    octave_function *ida_jac;
    Matrix *dfdy;
    Matrix *dfdyp;
    SparseMatrix *spdfdy;
    SparseMatrix *spdfdyp;
    DAERHSFuncIDA fun;
    DAEJacFuncDense jacfun;
    DAEJacFuncSparse jacspfun;
    DAEJacCellDense jacdcell;
    DAEJacCellSparse jacspcell;

  };

  int
  IDA::resfun (realtype t, N_Vector yy, N_Vector yyp, N_Vector rr,
               void *user_data)
  {
    IDA *self =
      static_cast <IDA *> (user_data);

    self -> resfun_impl (t, yy, yyp, rr);
    return 0;
  }

  void
  IDA::resfun_impl (realtype t, N_Vector& yy,
                    N_Vector& yyp, N_Vector& rr)
  {
    BEGIN_INTERRUPT_WITH_EXCEPTIONS;

    ColumnVector y =
      IDA::NVecToCol (yy, num);

    ColumnVector yp =
      IDA::NVecToCol (yyp, num);

    ColumnVector res =
      (*fun) (y, yp, t, ida_fun);

    realtype *puntrr =
      NV_DATA_S (rr);

    for (octave_idx_type i = 0; i < num; i++)
      puntrr [i] = res (i);

    END_INTERRUPT_WITH_EXCEPTIONS;
  }

  void
  IDA::set_up (void)
  {
    if (havejacsparse)
      {
        int flag =
          IDAKLU (mem, num, num*num, CSC_MAT);
        if (flag != 0)
          {
            error ("IDAKLU solver not initialized");
          }
        flag = IDASlsSetSparseJacFn (mem, IDA::jacsparse);
      }
    else
      {
        int flag =
          IDADense (mem, num);
        if (flag != 0)
          {
            error ("IDADense solver not initialized");
          }
        if (havejac)
          {
            flag = IDADlsSetDenseJacFn (mem, IDA::jacdense);
            if (flag != 0)
              {
                error ("Dense Jacobian not set");
              }
          }
      }
  }

  void
  IDA::jacdense_impl (long int Neq, realtype t, realtype cj,
                      N_Vector& yy, N_Vector& yyp, DlsMat& JJ)

  {
    BEGIN_INTERRUPT_WITH_EXCEPTIONS;

    ColumnVector y =
      NVecToCol (yy, Neq);

    ColumnVector yp =
      NVecToCol (yyp, Neq);

    Matrix jac;

    if (havejacfun)
      jac = (*jacfun) (y, yp, t, cj, ida_jac);
    else
      jac = (*jacdcell) (dfdy, dfdyp, cj);

    std::copy (jac.fortran_vec (),
               jac.fortran_vec () + jac.numel (),
               JJ -> data);

    END_INTERRUPT_WITH_EXCEPTIONS;
  }

  void
  IDA::jacsparse_impl (realtype t, realtype cj, N_Vector& yy, N_Vector& yyp,
                       SlsMat& Jac)

  {
    BEGIN_INTERRUPT_WITH_EXCEPTIONS;

    ColumnVector y =
      NVecToCol (yy, num);

    ColumnVector yp =
      NVecToCol (yyp, num);

    SparseMatrix jac;

    if (havejacfun)
      jac = (*jacspfun) (y, yp, t, cj, ida_jac);
    else
      jac = (*jacspcell) (spdfdy, spdfdyp, cj);

    SparseSetMatToZero (Jac);
    int *colptrs = *(Jac -> colptrs);
    int *rowvals = *(Jac -> rowvals);

    for (int i = 0; i < num + 1; i++)
      colptrs[i] = jac.cidx(i);

    for (int i = 0; i < jac.nnz (); i++)
      {
        rowvals[i] = jac.ridx (i);
        Jac -> data[i] = jac.data (i);
      }

    END_INTERRUPT_WITH_EXCEPTIONS;
  }

  ColumnVector
  IDA::NVecToCol (N_Vector& v, long int n)
  {
    ColumnVector data (n);
    realtype *punt;
    punt = NV_DATA_S (v);

    for (octave_idx_type i = 0; i < n; i++)
      data (i) = punt [i];

    return data;
  }

  N_Vector
  IDA::ColToNVec (const ColumnVector &data, long int n)
  {
    N_Vector v =
      N_VNew_Serial (n);

    realtype * punt;
    punt = NV_DATA_S (v);

    for (octave_idx_type i = 0; i < n; i++)
      punt [i] = data (i);

    return v;
  }

  void
  IDA::set_userdata (void)
  {
    void * userdata = this;

    int flag =
      IDASetUserData(mem, userdata);
    if (flag != 0)
      {
        error ("User data not set");
      }
  }

  void
  IDA::initialize (void)
  {
    num = y0.numel();
    mem = IDACreate ();

    N_Vector yy =
      ColToNVec(y0, num);

    N_Vector yyp =
      ColToNVec(yp0, num);

    IDA::set_userdata ();

    int flag =
      IDAInit (mem, IDA::resfun, t0, yy, yyp);
    if (flag != 0)
      {
        error ("IDA not initialized");
      }
  }

  void
  IDA::set_tolerance (ColumnVector& abstol, realtype reltol)
  {
    N_Vector abs_tol =
      ColToNVec (abstol, num);

    int flag =
      IDASVtolerances (mem, reltol, abs_tol);
    if (flag != 0)
      {
        error ("IDA: Tolerance not set");
      }
    N_VDestroy_Serial (abs_tol);
  }

  void
  IDA::set_tolerance (realtype abstol, realtype reltol)
  {
    int flag =
      IDASStolerances (mem, reltol, abstol);
    if (flag != 0)
      {
        error ("IDA: Tolerance not set");
      }
  }

  octave_value_list
  IDA::integrate (const int numt, const ColumnVector& tspan,
                  const ColumnVector& y, const ColumnVector& yp,
                  const int refine, bool haverefine, bool haveoutputfcn,
                  octave_function *output_fcn, bool haveoutputsel,
                  ColumnVector& outputsel, bool haveeventfunction,
                  octave_function *event_fcn)
  {
    // Set up output
    ColumnVector tout, yout (num), ypout (num), ysel (outputsel.numel ());
    ColumnVector ie, te, oldval, oldisterminal, olddir;
    Matrix output, ye;
    int cont = 0, flag = 0, temp = 0;
    bool status = 0;
    std::string string = "";
    ColumnVector yold = y;
    octave_value_list retval;

    realtype tsol =
      tspan (0);
    realtype tend =
      tspan (numt - 1);

    N_Vector yyp =
      ColToNVec (yp, num);

    N_Vector yy =
      ColToNVec (y, num);

    // Initialize OutputFcn
    if (haveoutputfcn)
      status = IDA::outputfun (output_fcn, haveoutputsel, y,
                               tsol, tend, outputsel, "init");

    // Initialize Events
    if (haveeventfunction)
      status = IDA::event (event_fcn, te, ye, ie, tsol, y,
                           "init", yp, oldval, oldisterminal,
                           olddir, cont, temp, tsol, yold);

    if (numt > 2)
      {
        // First output value
        tout.resize (numt);
        tout (0) = tsol;
        output.resize (numt, num);

        for (octave_idx_type i = 0; i < num; i++)
          output.elem (0, i) = y.elem (i);

        //Main loop
        for (octave_idx_type j = 1; j < numt && status == 0; j++)
          {
            // IDANORMAL already interpolates tspan(j)
            flag = IDASolve (mem, tspan (j), &tsol, yy, yyp, IDA_NORMAL);
            if (flag != 0)
              {
                error ("IDASolve failed");
              }

            yout = NVecToCol (yy, num);
            ypout = NVecToCol (yyp, num);
            tout (j) = tsol;

            for (octave_idx_type i = 0; i < num; i++)
              output.elem (j, i) = yout.elem (i);

            if (haveoutputfcn)
              status = IDA::outputfun (output_fcn, haveoutputsel, yout, tsol,
                                       tend, outputsel, string);

            if (haveeventfunction)
              status = IDA::event (event_fcn, te, ye, ie, tout (j), yout,
                                   string, ypout, oldval, oldisterminal,
                                   olddir, j, temp, tout (j - 1), yold);

            // If integration is stopped, return only the reached steps
            if (status == 1)
              {
                output.resize (j + 1, num);
                tout.resize (j + 1);
              }

          }
      }
    else // numel (tspan) == 2
      {
        // First output value
        tout.resize (1);
        tout (0) = tsol;
        output.resize (1, num);

        for (octave_idx_type i = 0; i < num; i++)
          output.elem (0, i) = y.elem (i);

        bool posdirection =
          (tend > tsol);

        //main loop
        while (((posdirection == 1 && tsol < tend)
               || (posdirection == 0 && tsol > tend))
               && status == 0)
          {
            flag = IDASolve (mem, tend, &tsol, yy, yyp, IDA_ONE_STEP);
            if (flag != 0)
              {
                error ("IDASolve failed");
              }

            if (haverefine)
              status = IDA::interpolate (cont, output, tout, refine, tend,
                                         haveoutputfcn, haveoutputsel,
                                         output_fcn, outputsel,
                                         haveeventfunction, event_fcn, te,
                                         ye, ie, oldval, oldisterminal,
                                         olddir, temp, yold);

            ypout = NVecToCol (yyp, num);
            cont += 1;
            output.resize (cont + 1, num); // This may be not efficient
            tout.resize (cont + 1);
            tout (cont) = tsol;
            yout = NVecToCol (yy, num);

            for (octave_idx_type i = 0; i < num; i++)
              output.elem (cont, i) = yout.elem (i);

            if (haveoutputfcn && ! haverefine && tout (cont) < tend)
              status = IDA::outputfun (output_fcn, haveoutputsel, yout, tsol,
                                       tend, outputsel, string);

            if (haveeventfunction && ! haverefine && tout (cont) < tend)
              status = IDA::event (event_fcn, te, ye, ie, tout (cont), yout,
                                   string, ypout, oldval, oldisterminal,
                                   olddir, cont, temp, tout (cont - 1), yold);
          }
        if (status == 0)
          {
            // Interpolate in tend
            N_Vector dky =
              N_VNew_Serial (num);

            flag = IDAGetDky (mem, tend, 0, dky);
            if (flag != 0)
              {
                error ("IDA failed to interpolate y");
              }

            tout (cont) = tend;
            yout = NVecToCol (dky, num);

            for (octave_idx_type i = 0; i < num; i++)
              output.elem (cont, i) = yout.elem (i);

            // Plot final value
            if (haveoutputfcn)
              {
                status = IDA::outputfun (output_fcn, haveoutputsel, yout,
                                         tend, tend, outputsel, string);

                // Events during last step
                if (haveeventfunction)
                  status = IDA::event (event_fcn, te, ye, ie, tend, yout,
                                       string, ypout, oldval, oldisterminal,
                                       olddir, cont, temp, tout (cont - 1),
                                       yold);
              }

            N_VDestroy_Serial (dky);
          }
        // Cleanup plotter
        status = IDA::outputfun (output_fcn, haveoutputsel, yout, tend, tend,
                                 outputsel, "done");

      }

    retval (0) = tout;
    retval (1) = output;
    retval (2) = te;
    retval (3) = ye;
    retval (4) = ie;

    return retval;
  }

  bool
  IDA::event (octave_function *event_fcn,
              ColumnVector& te, Matrix& ye, ColumnVector& ie,
              realtype tsol, const ColumnVector& y, std::string flag,
              const ColumnVector& yp, ColumnVector& oldval,
              ColumnVector& oldisterminal, ColumnVector& olddir, int cont,
              int& temp, realtype told, ColumnVector& yold)
  {
    bool status = 0;
    octave_value_list args (3);
    octave_value_list output (3);
    ColumnVector val, isterminal, dir;
    args (0) = tsol;
    args (1) = y;
    args (2) = yp;

    // cont is the number of steps reached by the solver
    // temp is the number of events registered

    if (flag == "init")
      {
        output = feval (event_fcn, args, 3);
        oldval = output(0).vector_value ();
        oldisterminal = output(1).vector_value ();
        olddir = output(2).vector_value ();
      }
    else if (flag == "")
      {
        ColumnVector index (0);
        output = feval (event_fcn, args, 3);
        val = output(0).vector_value ();
        isterminal = output(1).vector_value ();
        dir = output(2).vector_value ();

        // Get the index of the changed values
        for (octave_idx_type i = 0; i < val.numel (); i++)
          {
            if ((val(i) > 0 && oldval(i) < 0 && dir(i) != -1) // increasing
                || (val(i) < 0 && oldval(i) > 0 && dir(i) != 1)) // decreasing
              {
                index.resize (index.numel () + 1);
                index (index.numel () - 1) = i;
              }
          }

        if (cont == 1 && index.numel () > 0)  // Events in first step
          {
            temp = 1; // register only the first event
            te.resize (1);
            ye.resize (1, num);
            ie.resize (1);

            // Linear interpolation
            ie (0) = index (0);
            te (0) = tsol - val (index (0)) * (tsol - told)
              / (val (index (0)) - oldval (index (0)));

            ColumnVector ytemp =
              y - ((tsol - te (0)) * (y - yold ) / (tsol - told));

            for (octave_idx_type i = 0; i < num; i++)
              ye.elem (0, i) = ytemp.elem (i);

          }
        else if (index.numel () > 0)
          // Not first step: register all events and test if stop integration or not
          {
            te.resize (temp + index.numel ());
            ye.resize (temp + index.numel (), num);
            ie.resize (temp + index.numel ());

            for (octave_idx_type i = 0; i < index.numel (); i++)
              {

                if (isterminal (index (i)) == 1)
                  status = 1; // Stop integration

                // Linear interpolation
                ie (temp + i) = index (i);
                te (temp + i) = tsol -
                  val (index (i)) * (tsol - told) /
                  (val (index (i)) - oldval (index (i)));

                ColumnVector ytemp =
                  y - (tsol - te (temp + i)) * (y - yold) / (tsol - told);

                for (octave_idx_type j = 0; j < num; j++)
                  ye.elem (temp + i, j) = ytemp.elem (j);

              }

            temp += index.numel ();
          }

        // Update variables
        yold = y;
        told = tsol;
        olddir = dir;
        oldval = val;
        oldisterminal = isterminal;
      }

    return status;
  }

  bool
  IDA::interpolate (int& cont, Matrix& output, ColumnVector& tout,
                    int refine, realtype tend, bool haveoutputfcn,
                    bool haveoutputsel, octave_function *output_fcn,
                    ColumnVector& outputsel, bool haveeventfunction,
                    octave_function * event_fcn, ColumnVector& te,
                    Matrix& ye, ColumnVector& ie, ColumnVector& oldval,
                    ColumnVector& oldisterminal, ColumnVector& olddir,
                    int& temp, ColumnVector& yold)
  {
    realtype h = 0, tcur = 0;
    bool status = 0;

    N_Vector dky =
      N_VNew_Serial (num);

    N_Vector dkyp =
      N_VNew_Serial (num);

    ColumnVector yout (num);
    ColumnVector ypout (num);
    std::string string = "";

    int flag =
      IDAGetLastStep (mem, &h);

    if (flag != 0)
      {
        error ("IDA failed to return last step");
      }
    flag = IDAGetCurrentTime (mem, &tcur);
    if (flag != 0)
      {
        error ("IDA failed to return the current time");
      }

    realtype tin =
      tcur - h;

    realtype step =
      h / refine;

    for (octave_idx_type i = 1; i < refine && tin + step * i < tend
           && status == 0; i++)
      {
        flag = IDAGetDky (mem, tin + step*i, 0, dky);
        if (flag != 0)
          {
            error ("IDA failed to interpolate y");
          }
        flag = IDAGetDky (mem, tin + step*i, 1, dkyp);
        if (flag != 0)
          {
            error ("IDA failed to interpolate yp");
          }
        cont += 1;
        output.resize (cont + 1, num);
        tout.resize (cont + 1);

        tout (cont) = tin + step * i;
        yout = NVecToCol (dky, num);
        ypout = NVecToCol (dkyp, num);

        for (octave_idx_type j = 0; j < num; j++)
          output.elem (cont, j) = yout.elem (j);

        if (haveoutputfcn)
          status = IDA::outputfun (output_fcn, haveoutputsel, yout,
                                   tout (cont), tend, outputsel, "");

        if (haveeventfunction)
          status = IDA::event (event_fcn, te, ye, ie, tout (cont),
                               yout, string, ypout, oldval,
                               oldisterminal, olddir, cont, temp,
                               tout (cont - 1), yold);
      }
    N_VDestroy_Serial (dky);
    return status;
  }

  bool
  IDA::outputfun (octave_function *output_fcn, bool haveoutputsel,
                  const ColumnVector& yout, realtype tsol,
                  realtype tend, ColumnVector& outputsel,
                  std::string flag)
  {
    bool status = 0;
    octave_value_list output, val;
    ColumnVector ysel (outputsel.numel ());

    if (haveoutputsel)
      {
        for (octave_idx_type i = 0; i < outputsel.numel (); i++)
          ysel (i) = yout (outputsel (i));

        output (1) = ysel;
      }
    else
      output (1) = yout;

    output (2) = flag;

    if (flag == "init")
      {
        ColumnVector toutput (2);
        toutput (0) = tsol;
        toutput (1) = tend;
        output (0) = toutput;

        feval (output_fcn, output, 0);
      }
    else if (flag == "")
      {
        output (0) = tsol;
        val = feval (output_fcn, output, 1);
        status = val(0).bool_value ();
      }
    else
      {  // Cleanup plotter
        output (0) = tend;
        feval (output_fcn, output, 0);
      }

    return status;
  }

  void
  IDA::set_maxstep (realtype maxstep)
  {
    int flag =
      IDASetMaxStep (mem, maxstep);

    if (flag != 0)
      {
        error ("IDA: Max Step not set");
      }
  }

  void
  IDA::set_initialstep (realtype initialstep)
  {
    int flag =
      IDASetInitStep (mem, initialstep);

    if (flag != 0)
      {
        error ("IDA: Initial Step not set");
      }
  }

  void
  IDA::set_maxorder (int maxorder)
  {
    int flag =
      IDASetMaxOrd (mem, maxorder);

    if (flag != 0)
      {
        error ("IDA: Max Order not set");
      }
  }

  void
  IDA::print_stat (void)
  {
    long int nsteps = 0, netfails = 0, nrevals = 0;
    int flag =
      IDAGetNumSteps(mem, &nsteps);

    if (flag != 0)
      {
        error ("IDA failed to return the number of internal steps");
      }
    flag = IDAGetNumErrTestFails(mem, &netfails);
    if (flag != 0)
      {
        error ("IDA failed to return the number of internal errors");
      }
    flag = IDAGetNumResEvals(mem, &nrevals);
    if (flag != 0)
      {
        error ("IDA failed to return the number of residual evaluations");
      }

    std::cout<<nsteps<<" successful steps\n";
    std::cout<<netfails<<" failed attempts\n";
    std::cout<<nrevals<<" function evaluations\n";
    //std::cout<<<<" partial derivatives\n";
    //std::cout<<<<" LU decompositions\n";
    //std::cout<<<<" solutions of linear systems\n";
  }

  ColumnVector
  ida_user_function (const ColumnVector& x, const ColumnVector& xdot,
                     double t, octave_function *ida_fc)
  {
    ColumnVector retval;
    octave_value_list args;

    args(2) = xdot;
    args(1) = x;
    args(0) = t;

    octave_value_list tmp;

    try
      {
        tmp = ida_fc -> do_multi_index_op (1, args);
      }
    catch (octave::execution_exception& e)
      {
        err_user_supplied_eval (e, "__ode15__");
      }

    retval = tmp(0).vector_value ();

    return retval;
  }

  Matrix
  ida_dense_jac (const ColumnVector& x, const ColumnVector& xdot,
                 double t, double cj, octave_function *ida_jc)
  {
    Matrix retval;
    octave_value_list newargs (3);

    newargs (0) = t;
    newargs (1) = x;
    newargs (2) = xdot;

    octave_value_list tmp;

    try
      {
        tmp = ida_jc -> do_multi_index_op (2, newargs);
      }
    catch (octave::execution_exception& e)
      {
        err_user_supplied_eval (e, "__ode15__");
      }

    retval = tmp(0).matrix_value () + cj * tmp(1).matrix_value ();

    return retval;
  }

  SparseMatrix
  ida_sparse_jac (const ColumnVector& x, const ColumnVector& xdot,
                  double t, double cj, octave_function *ida_jc)
  {
    SparseMatrix retval;
    octave_value_list newargs (3);

    newargs (0) = t;
    newargs (1) = x;
    newargs (2) = xdot;

    octave_value_list tmp;

    try
      {
        tmp = ida_jc -> do_multi_index_op (2, newargs);
      }
    catch (octave::execution_exception& e)
      {
        err_user_supplied_eval (e, "__ode15__");
      }

    retval = tmp(0).sparse_matrix_value () +
      cj * tmp(1).sparse_matrix_value ();

    return retval;
  }

  Matrix
  ida_dense_cell_jac (Matrix *dfdy, Matrix *dfdyp, double cj)
  {
    Matrix retval;
    retval = (*dfdy) + cj * (*dfdyp);
    return retval;
  }

  SparseMatrix
  ida_sparse_cell_jac (SparseMatrix *spdfdy, SparseMatrix *spdfdyp,
                       double cj)
  {
    SparseMatrix retval;
    retval = (*spdfdy) + cj * (*spdfdyp);
    return retval;
  }

  octave_value_list
  do_ode15 (octave_function *ida_fcn,
            const ColumnVector &tspan,
            const int numt,
            const realtype t0,
            const ColumnVector &y0,
            const ColumnVector &yp0,
            const octave_scalar_map &options)
  {
    octave_value_list retval;

    // Create object
    IDA dae (t0, y0, yp0, ida_fcn, ida_user_function);

    // Set Jacobian
    bool havejac =
      options.getfield ("havejac").bool_value ();

    bool havejacsparse =
      options.getfield ("havejacsparse").bool_value ();

    bool havejacfun =
      options.getfield ("havejacfun").bool_value ();

    Matrix ida_dfdy, ida_dfdyp, *dfdy, *dfdyp;
    SparseMatrix ida_spdfdy, ida_spdfdyp, *spdfdy, *spdfdyp;
    octave_function *ida_jac;
    Cell jaccell;

    if (havejac)
      {
        if (havejacfun)
          {
            ida_jac = options.getfield ("Jacobian").function_value ();
            if (havejacsparse)
              dae.set_jacobian (ida_jac, ida_sparse_jac);
            else
              dae.set_jacobian (ida_jac, ida_dense_jac);
          }
        else
          {
            jaccell = options.getfield ("Jacobian").cell_value ();

            if (havejacsparse)
              {
                ida_spdfdy = jaccell(0).sparse_matrix_value ();
                ida_spdfdyp = jaccell(1).sparse_matrix_value ();
                spdfdy = &ida_spdfdy;
                spdfdyp = &ida_spdfdyp;
                dae.set_jacobian (spdfdy, spdfdyp, ida_sparse_cell_jac);
              }
            else
              {
                ida_dfdy = jaccell(0).matrix_value ();
                ida_dfdyp = jaccell(1).matrix_value ();
                dfdy = &ida_dfdy;
                dfdyp = &ida_dfdyp;
                dae.set_jacobian (dfdy, dfdyp, ida_dense_cell_jac);
              }
          }
      }

    // Initialize IDA
    dae.initialize ();

    // Set tolerances
    realtype rel_tol =
      options.getfield("RelTol").double_value ();

    bool haveabstolvec =
      options.getfield ("haveabstolvec").bool_value ();

    if (haveabstolvec)
      {
        ColumnVector abs_tol =
          options.getfield("AbsTol").vector_value ();

        dae.set_tolerance (abs_tol, rel_tol);
      }
    else
      {
        realtype abs_tol =
          options.getfield("AbsTol").double_value ();

        dae.set_tolerance (abs_tol, rel_tol);
      }

    //Set max step
    realtype maxstep =
      options.getfield("MaxStep").double_value ();

    dae.set_maxstep (maxstep);

    //Set initial step
    if (!(options.getfield("InitialStep").is_empty ()))
      {
        realtype initialstep =
          options.getfield("InitialStep").double_value ();

        dae.set_initialstep (initialstep);
      }

    //Set max order FIXME: it doesn't work
    int maxorder =
      options.getfield("MaxOrder").int_value ();

    dae.set_maxorder (maxorder);

    //Set Refine
    const int refine =
      options.getfield("Refine").int_value ();

    bool haverefine =
      (refine > 1);

    octave_function *output_fcn = 0;
    ColumnVector outputsel;

    // OutputFcn
    bool haveoutputfunction =
      options.getfield("haveoutputfunction").bool_value ();

    if (haveoutputfunction)
      output_fcn = options.getfield("OutputFcn").function_value ();

    // OutputSel
    bool haveoutputsel =
      options.getfield("haveoutputselection").bool_value ();

    if (haveoutputsel)
      outputsel = options.getfield("OutputSel").vector_value ();

    octave_function *event_fcn = 0;

    // Events
    bool haveeventfunction =
      options.getfield("haveeventfunction").bool_value ();

    if (haveeventfunction)
      event_fcn = options.getfield("Events").function_value ();

    // Set up linear solver
    dae.set_up ();

    // Integrate
    retval = dae.integrate (numt, tspan, y0, yp0, refine,
                            haverefine, haveoutputfunction,
                            output_fcn, haveoutputsel, outputsel,
                            haveeventfunction, event_fcn);

    // Statistics
    bool havestats =
      options.getfield("havestats").bool_value ();

    if (havestats)
      dae.print_stat ();

    return retval;
  }
}
#endif


DEFUN_DLD (__ode15__, args, nargout, doc: /* -*- texinfo -*-
@deftypefn  {} {@var{t}, @var{y} =} __ode15__ (@var{fun}, @
@var{tspan}, @var{y0}, @var{yp0}, @var{options})
Undocumented internal function.
@end deftypefn */)
{

#if defined (HAVE_SUNDIALS)

  // Check number of parameters
  int nargin = args.length ();

  if (nargin != 5 || nargout != 5)
    print_usage ();

  // Check odefun
  octave_function *ida_fcn = 0;

  octave_value f_arg = args(0);

  if (f_arg.is_function_handle ())
    ida_fcn = f_arg.function_value ();
  else
    error ("__ode15__: odefun must be a function handle");

  // Check input tspan
  ColumnVector tspan =
    args(1).xvector_value ("__ode15__: TRANGE must be a vector of numbers");

  int numt =
    tspan.numel ();

  realtype t0 =
    tspan (0);

  if (numt < 2)
    error ("__ode15__: TRANGE must contain at least 2 elements");
  else if (!(tspan.is_sorted ()) || (tspan(0) == tspan(numt - 1)))
    error ("__ode15__: TRANGE must be strictly monotonic");

  // input y0 and yp0
  ColumnVector y0  =
    args(2).xvector_value ("__ode15__: initial state y0 must be a vector");

  ColumnVector yp0 =
    args(3).xvector_value ("__ode15__: initial state yp0 must be a vector");


  if (y0.numel () != yp0.numel ())
    error ("__ode15__: initial state y0 and yp0 must have the same length");
  else if (y0.numel () < 1)
    error ("__ode15__: initial state yp0 must be a vector or a scalar");


  if (! args(4).is_map ())
    error ("__ode15__: OPTS argument must be a structure");

  octave_scalar_map options =
    args(4).xscalar_map_value ("__ode15__:",
    "OPTS argument must be a scalar structure");


  return octave::do_ode15 (ida_fcn, tspan, numt, t0,
                           y0, yp0, options);


#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("__ode15__", "sundials_ida, sundials_nvecserial");

#endif
}



