Summary of important user-visible changes for version 12 (yyyy-mm-dd):
---------------------------------------------------------------------

### General improvements


### Graphical User Interface


### Graphics backend


### Matlab compatibility


### Alphabetical list of new functions added in Octave 12


### Deprecated functions, properties, and operators

The following functions and properties have been deprecated in Octave 12
and will be removed from Octave 14 (or whatever version is the second
major release after 12):

- Functions

        Function               | Replacement
        -----------------------|------------------


- Core

        Function                  | Replacement
        --------------------------|--------------------------

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
