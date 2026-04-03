Summary of important user-visible changes for version 12 (yyyy-mm-dd):
---------------------------------------------------------------------

### General improvements

* The size of `classdef` objects is now reported by `who` (bug #55810).  A
  value class' byte size is calculated by summing up the byte size of all its
  properties.  A handle class' byte size is calculated by getting the size of
  the machine word on the users computer (most likely 4 or 8 bytes).

* The constructors of `classdef` classes now support returning more than one
  output argument.  The first output argument must be the constructed object.


### Graphical User Interface

* When opening a file from the Find Files dialog, all occurrences of the search
  text are highlighted in the editor.

### Graphics backend


### Matlab compatibility

* Sparse matrices can only be 2-dimensional.  Octave now throws an error if the
  input to `sparse` is an N-dimensional array.  An error is also thrown if an
  N-dimensional indexing operation of a sparse matrix is attempted.  Both
  behaviors are Matlab-compatible.


### Alphabetical list of new functions added in Octave 12


### Deprecated functions, properties, and operators

The following functions and properties have been deprecated in Octave 12
and will be removed from Octave 14 (or whatever version is the second
major release after 12):

- Functions

        Function               | Replacement
        -----------------------|--------------------------


- Core

        Function                            | Replacement
        ------------------------------------|-------------------------------------------
        string_vector::list_in_columns (os) | string_vector::list_in_columns (os, width)
                                            |   width = command_editor::terminal_cols ()

The following features were deprecated in Octave 10 and have been removed
from Octave 12.

- Functions

        Function    | Replacement
        ------------|------------
        dsearch     | dsearchn

- Core

        Function                                     | Replacement
        ---------------------------------------------|-----------------------------------
        symbol_record_rep::mark_as_variable          | symbol_record_rep::mark_variable
        symbol_record_rep::unmark_as_variable        | symbol_record_rep::unmark_variable
        symbol_record::mark_as_variable              | symbol_record::mark_variable
        symbol_record::unmark_as_variable            | symbol_record::unmark_variable
        interpreter::verbose                         | interpreter::init_trace
        cmdline_options::verbose                     | cmdline_options::init_trace
        interpreter::read_init_files                 | interpreter::read_user_files
        cmdline_options::read_init_files             | cmdline_options::read_user_files
        __lo_ieee_isnan,    __lo_ieee_float_isnan    | std::isnan     or  isnan
        __lo_ieee_isfinite, __lo_ieee_float_isfinite | std::isfinite  or  isfinite
        __lo_ieee_isinf,    __lo_ieee_float_isinf    | std::isinf     or  isinf
        __lo_ieee_signbit,  __lo_ieee_float_signbit  | std::signbit   or  signbit

The following features were deprecated in Octave 11 and have been removed
from Octave 12.

- Function typedefs

    * `b_d_Mapper`
    * `b_c_Mapper`
    * `d_d_Mapper`
    * `d_c_Mapper`
    * `c_c_Mapper`
    * `b_f_Mapper`
    * `b_fc_Mapper`
    * `f_f_Mapper`
    * `f_fc_Mapper`
    * `fc_fc_Mapper`


### Functions removed without the usual deprecation period

The following changes have been made without the usual deprecation period of two
release cycles because we believe that they are unlikely to have been used in
code outside of core Octave.

The header `oct-atomic.h` has been removed.  Use the `std::atomic` object
defined in the standard C++ header `<atomic>` instead.

The following C++ functions have been removed from Octave 12:

        Function                | Replacement
        ------------------------|--------------------------------------------
        octave_get_float_format | octave::mach_info::native_float_format
        octave_is_big_endian    | octave::mach_info::words_big_endian
                                | (or octave::mach_info::words_little_endian)
        octave_atomic_increment | ++std::atomic<...>
        octave_atomic_decrement | --std::atomic<...>

### Function APIs changed without the usual deprecation period

The `Sparse` class for sparse matrices behaves similarly to the `Array` class
for full matrices.  The `Array` class provides a `data()` method which returns a
read-only `const` pointer to the underlying data, and a `rwdata()` method which
returns a pointer that can be used to modify the underlying data.  The `Sparse`
class did not follow these conventions, but has been modified in Octave 12 to
behave identically to the `Array` class.

        Old Function                | New Functions
        ----------------------------|--------------------------------------
        Sparse::data (write-access) | Sparse::rwdata
                                    | Sparse::rwridx (write-access to ridx)
                                    | Sparse::rwcidx (write-access to cidx)

The virtual functions `octave_base_value::all` and `octave_base_value::any`
have been changed to throw an error if the subclass type does not override `all`
and `any`.  The `all` and `any` function only work on numeric or logical input.
They will error if passed any other input.  In previous versions of Octave,
`all` and `any` returned 0 if the input type was neither numeric nor logical,
which is not MATLAB compatible.

### Old release news

- [Octave 11.x](etc/NEWS.11.md)
- [Octave 10.x](etc/NEWS.10.md)
- [Octave 9.x](etc/NEWS.9.md)
- [Octave 8.x](etc/NEWS.8.md)
- [Octave 7.x](etc/NEWS.7.md)
- [Octave 6.x](etc/NEWS.6.md)
- [Octave 5.x](etc/NEWS.5.md)
- [Octave 4.x](etc/NEWS.4)
- [Octave 3.x](etc/NEWS.3)
- [Octave 2.x](etc/NEWS.2)
- [Octave 1.x](etc/NEWS.1)
