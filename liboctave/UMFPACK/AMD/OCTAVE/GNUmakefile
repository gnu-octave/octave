#------------------------------------------------------------------------------
# GNUmakefile for the AMD MATLAB mexFunction
#------------------------------------------------------------------------------

all: amd

include ../Make/Make.include

MKOCT = mkoctfile -I../Include

OCT_SPARSE_INC = -I../../../

AMD = amd_aat amd_1 amd_2 amd_dump amd_postorder amd_post_tree amd_defaults \
	amd_order amd_control amd_info amd_valid

INC = ../Include/amd.h ../Source/amd_internal.h 

OCTAMD = $(addsuffix .o, $(subst amd_,amd_o_,$(AMD)))

amd_o_%.o: ../Source/amd_%.c $(INC)
	$(MKOCT) -DDINT -c $< -o $@
	- $(MV) ../Source/amd_$*.o

# Note temporary addition of octave sparse path
amd: amd.cc $(OCTAMD) $(INC)
	$(MKOCT) amd.cc $(OCTAMD) $(OCT_SPARSE_INC) -o amd.oct

#------------------------------------------------------------------------------
# Remove all but the files in the original distribution
#------------------------------------------------------------------------------

purge: clean
	- $(RM) amd.oct
