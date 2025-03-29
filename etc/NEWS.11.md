Summary of important user-visible changes for version 11 (yyyy-mm-dd):
---------------------------------------------------------------------

### General improvements

- The internal interface to Java has been updated to be more memory-efficient (faster culling of unused objects).  Octave now requires Java 1.9 to build from sources.  Distributed versions of Octave will run with any JVM.

### Graphical User Interface

- The GUI now uses scalable SVG icons for beautiful display at any size.

### Graphics backend

### Matlab compatibility

### Alphabetical list of new functions added in Octave 11

* `corrcov`

### Deprecated functions, properties, and operators

The following functions and properties have been deprecated in Octave 11
and will be removed from Octave 13 (or whatever version is the second
major release after 11):

- Functions

        Function               | Replacement
        -----------------------|------------------

- Core

        Function               | Replacement
        -----------------------|------------------

The following features were deprecated in Octave 9 and have been removed
from Octave 11.

- Core

        Function                            | Replacement
        ------------------------------------|-----------------------------------
        octave::initialized                 | octave::is_initialized
        octave::same_file                   | octave::sys::same_file
        octave::interpreter_initialized     | octave::interpreter_is_initialized
        all_ok (Array<octave::idx_vector>&) | 
        idx_vector::bool ()                 | 

### Old release news

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
