#-------------------------------------------------------------------------------
# UMFPACK makefile (for GNU make or original make)
#-------------------------------------------------------------------------------

# UMFPACK requires the AMD package to be in ../AMD

# compile everything (except hb, fortran, and fortran64), including AMD and the
# MATLAB mexFunctions
all:
	( cd ../AMD ; make )
	( cd Source ; make )
	( cd Demo   ; make )
	( cd MATLAB ; make )
	- cat Doc/License

# compile just the C-callable libraries and demo programs (not mexFunctions)
lib:
	( cd ../AMD ; make lib )
	( cd Source ; make )
	( cd Demo   ; make )
	- cat Doc/License

# compile the FORTRAN interface and demo program
fortran:
	( cd Demo   ; make fortran )

# compile the 64-bit FORTRAN interface and demo program
fortran64:
	( cd Demo   ; make fortran64 )

# compile the Harwell/Boeing demo program
hb:
	( cd Demo   ; make hb )

# compile a Octave version
# (not compiled by "make all")
octave:
	( cd OCTAVE ; make )
	- cat Doc/License

# remove object files, but keep the compiled programs and library archives
clean:
	( cd ../AMD ; make clean )
	( cd Source ; make clean )
	( cd Demo   ; make clean )
	( cd MATLAB ; make clean )
	( cd OCTAVE ; make clean )
	( cd Doc    ; make clean )

# clean, and then remove compiled programs and library archives
purge:
	( cd ../AMD ; make purge )
	( cd Source ; make purge )
	( cd Demo   ; make purge )
	( cd MATLAB ; make purge )
	( cd OCTAVE ; make purge )
	( cd Doc    ; make purge )

# create PDF documents for the original distribution
doc:
	( cd ../AMD ; make doc )
	( cd Doc    ; make )

# get ready for distribution
dist: purge
	( cd ../AMD ; make dist )
	( cd Demo   ; make dist )
	( cd Doc    ; make )
