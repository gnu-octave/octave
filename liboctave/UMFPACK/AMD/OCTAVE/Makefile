#------------------------------------------------------------------------------
# compile the AMD mexFunction for MATLAB (original make only)
#------------------------------------------------------------------------------

# This is a very ugly Makefile, and is only provided for those who do not
# have GNU make.  Note that it is not used if you have GNU make.  It ignores
# dependency checking and just compiles everything.  It was created
# automatically, via make -n using the GNUmakefile.  That way, I don't have
# maintain two Makefiles.

all: amd

include ../Make/Make.include

MKOCT = mkoctfile -I../Include

OCT_SPARSE_INC = ../../../

amd:
	$(MKOCT) -DDINT -o amd_o_aat.o -c ../Source/amd_aat.c
	$(MKOCT) -DDINT -o amd_o_1.o -c ../Source/amd_1.c
	$(MKOCT) -DDINT -o amd_o_2.o -c ../Source/amd_2.c
	$(MKOCT) -DDINT -o amd_o_dump.o -c ../Source/amd_dump.c
	$(MKOCT) -DDINT -o amd_o_postorder.o -c ../Source/amd_postorder.c
	$(MKOCT) -DDINT -o amd_o_post_tree.o -c ../Source/amd_post_tree.c
	$(MKOCT) -DDINT -o amd_o_defaults.o -c ../Source/amd_defaults.c
	$(MKOCT) -DDINT -o amd_o_order.o -c ../Source/amd_order.c
	$(MKOCT) -DDINT -o amd_o_control.o -c ../Source/amd_control.c
	$(MKOCT) -DDINT -o amd_o_info.o -c ../Source/amd_info.c
	$(MKOCT) -DDINT -o amd_o_valid.o -c ../Source/amd_valid.c
	$(MKOCT) -output amd.oct amd_mex.c amd_o_aat.o \
	    amd_o_1.o amd_o_2.o amd_o_dump.o amd_o_postorder.o \
	    amd_o_post_tree.o amd_o_defaults.o amd_o_order.o amd_o_control.o \
	    amd_o_info.o amd_o_valid.o $(OCT_SPARSE_INC) -o amd.oct

#------------------------------------------------------------------------------
# Remove all but the files in the original distribution
#------------------------------------------------------------------------------

purge: clean
	- $(RM) amd.oct
