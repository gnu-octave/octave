#-------------------------------------------------------------------------------
# UMFPACK GNUmakefile for the UMFPACK OCTAVE oct-file (GNU "make" only)
#-------------------------------------------------------------------------------

.PRECIOUS: %.o

# UMFPACK Version 4.3 (Jan. 16, 2004), Copyright (c) 2004 by Timothy A.
# Davis.  All Rights Reserved.  See ../README for License.

all: umfpack luflop

include ../Make/Make.include

MKOCT = mkoctfile $(CONFIG) -DNRECIPROCAL -I/usr/include/atlas -I../Include -I../Source -I../../AMD/Include -I../../AMD/Source

OCT_SPARSE_INC = -I../../../


#-------------------------------------------------------------------------------
# source files
#-------------------------------------------------------------------------------

# non-user-callable umf_*.[ch] files:
UMFCH = umf_assemble umf_blas3_update \
	umf_build_tuples umf_create_element \
	umf_dump umf_extend_front umf_garbage_collection \
	umf_get_memory umf_init_front umf_kernel \
	umf_kernel_init umf_kernel_wrapup \
	umf_local_search umf_lsolve umf_ltsolve \
	umf_mem_alloc_element umf_mem_alloc_head_block \
	umf_mem_alloc_tail_block umf_mem_free_tail_block \
	umf_mem_init_memoryspace \
	umf_report_vector umf_row_search umf_scale_column \
	umf_set_stats umf_solve umf_symbolic_usage umf_transpose \
	umf_tuple_lengths umf_usolve umf_utsolve umf_valid_numeric \
	umf_valid_symbolic umf_grow_front umf_start_front umf_2by2 \
	umf_store_lu umf_scale

# non-user-callable umf_*.[ch] files, int/long versions only (no real/complex):
UMFINT = umf_analyze umf_apply_order umf_colamd umf_free umf_fsize \
	umf_is_permutation umf_malloc umf_realloc umf_report_perm \
	umf_singletons

# non-user-callable and user-callable amd_*.[ch] files (int/long versions only):
AMD = amd_aat amd_1 amd_2 amd_dump amd_postorder amd_post_tree amd_defaults \
	amd_order amd_control amd_info amd_valid

# non-user-callable, created from umf_ltsolve.c, umf_utsolve.c,
# umf_triplet.c, and umf_assemble.c , with int/long and real/complex versions:
UMF_CREATED = umf_lhsolve umf_uhsolve umf_triplet_map_nox \
	umf_triplet_nomap_x umf_triplet_nomap_nox umf_triplet_map_x \
	umf_assemble_fixq umf_store_lu_drop

# non-user-callable, int/long and real/complex versions:
UMF = $(UMF_CREATED) $(UMFCH)

# user-callable umfpack_*.[ch] files (int/long and real/complex):
UMFPACK = umfpack_col_to_triplet umfpack_defaults umfpack_free_numeric \
	umfpack_free_symbolic umfpack_get_numeric umfpack_get_lunz \
	umfpack_get_symbolic umfpack_get_determinant umfpack_numeric \
	umfpack_qsymbolic umfpack_report_control umfpack_report_info \
	umfpack_report_matrix umfpack_report_numeric umfpack_report_perm \
	umfpack_report_status umfpack_report_symbolic umfpack_report_triplet \
	umfpack_report_vector umfpack_solve umfpack_symbolic \
	umfpack_transpose umfpack_triplet_to_col umfpack_scale \
	umfpack_load_numeric umfpack_save_numeric \
	umfpack_load_symbolic umfpack_save_symbolic

# user-callable, created from umfpack_solve.c (umfpack_wsolve.h exists, though):
# with int/long and real/complex versions:
UMFPACKW = umfpack_wsolve

USER = $(UMFPACKW) $(UMFPACK)

# user-callable, only one version for int/long, real/complex, *.[ch] files:
GENERIC = umfpack_timer umfpack_tictoc

#-------------------------------------------------------------------------------
# include files:
#-------------------------------------------------------------------------------

AMDH = ../../AMD/Source/amd_internal.h ../../AMD/Include/amd.h

INC1 = umf_config.h umf_version.h umf_internal.h umf_triplet.h

INC = ../Include/umfpack.h \
	$(addprefix ../Source/, $(INC1)) \
	$(addprefix ../Source/, $(addsuffix .h,$(UMFCH))) \
	$(addprefix ../Source/, $(addsuffix .h,$(UMFINT))) \
	$(addprefix ../Include/, $(addsuffix .h,$(USER))) \
	$(addprefix ../Include/, $(addsuffix .h,$(GENERIC))) \
	$(AMDH)

#-------------------------------------------------------------------------------
# Create the umfpack and amd oct-file for OCTAVE (int versions only)
#-------------------------------------------------------------------------------

OCTI   = $(addsuffix .o, $(subst umf_,umf_o_,$(UMFINT)))
OCTDI  = $(addsuffix .o, $(subst umf_,umf_od_,$(UMF)) $(subst umfpack_,umfpack_od_,$(USER)))
OCTZI  = $(addsuffix .o, $(subst umf_,umf_oz_,$(UMF)) $(subst umfpack_,umfpack_oz_,$(USER)) )
OCTAMD = $(addsuffix .o, $(subst amd_,amd_o_,$(AMD)))
OCTGN  = $(addsuffix .o, $(subst umfpack_,umfpack_o_,$(GENERIC)))

OCTUMFPACK = $(OCTI) $(OCTDI) $(OCTZI) $(OCTGN)

OCTUMFPACK_LIB = umfpack_octave.o

# Note that mkoctfile has an "-o" option, but it doesn't work in conjunction
# with the "-c" option, thus the need for $(MV) commands.
# If it did, then the rules would be much simpler:
#	$(MKOCT) -DDINT -c $< -o $@

#----------------------------------------
# integer-only routines (no real/complex):
#----------------------------------------

amd_o_%.o: ../../AMD/Source/amd_%.c $(AMDH)
	$(MKOCT) -DDINT -c $<
	- $(MV) ../../AMD/Source/amd_$*.o $@


umf_o_%.o: ../Source/umf_%.c $(INC)
	$(MKOCT) -DDINT -c $<
	- $(MV) ../Source/umf_$*.o $@

#----------------------------------------
# Double precision, int version, for OCTAVE
#----------------------------------------

umf_od_%.o: ../Source/umf_%.c $(INC)
	$(MKOCT) -DDINT -c $<
	- $(MV) ../Source/umf_$*.o $@

umf_od_%hsolve.o: ../Source/umf_%tsolve.c $(INC)
	$(MKOCT) -DDINT -DCONJUGATE_SOLVE -c $<
	- $(MV) ../Source/umf_$*tsolve.o $@

umf_od_triplet_map_x.o: ../Source/umf_triplet.c $(INC)
	$(MKOCT) -DDINT -DDO_MAP -DDO_VALUES -c $<
	- $(MV) ../Source/umf_triplet.o $@

umf_od_triplet_map_nox.o: ../Source/umf_triplet.c $(INC)
	$(MKOCT) -DDINT -DDO_MAP -c $<
	- $(MV) ../Source/umf_triplet.o $@

umf_od_triplet_nomap_x.o: ../Source/umf_triplet.c $(INC)
	$(MKOCT) -DDINT -DDO_VALUES -c $<
	- $(MV) ../Source/umf_triplet.o $@

umf_od_triplet_nomap_nox.o: ../Source/umf_triplet.c $(INC)
	$(MKOCT) -DDINT -c $<
	- $(MV) ../Source/umf_triplet.o $@

umf_od_assemble_fixq.o: ../Source/umf_assemble.c $(INC)
	$(MKOCT) -DDINT -DFIXQ -c $<
	- $(MV) ../Source/umf_assemble.o $@

umf_od_store_lu_drop.o: ../Source/umf_store_lu.c $(INC)
	$(MKOCT) -DDINT -DDROP -c $<
	- $(MV) ../Source/umf_store_lu.o $@

umfpack_od_wsolve.o: ../Source/umfpack_solve.c $(INC)
	$(MKOCT) -DDINT -DWSOLVE -c $<
	- $(MV) ../Source/umfpack_solve.o $@

umfpack_od_%.o: ../Source/umfpack_%.c $(INC)
	$(MKOCT) -DDINT -c $<
	- $(MV) ../Source/umfpack_$*.o $@

#----------------------------------------
# Complex double precision, int version, for OCTAVE
#----------------------------------------

umf_oz_%.o: ../Source/umf_%.c $(INC)
	$(MKOCT) -DZINT -c $<
	- $(MV) ../Source/umf_$*.o $@

umf_oz_%hsolve.o: ../Source/umf_%tsolve.c $(INC)
	$(MKOCT) -DZINT -DCONJUGATE_SOLVE -c $<
	- $(MV) ../Source/umf_$*tsolve.o $@

umf_oz_triplet_map_x.o: ../Source/umf_triplet.c $(INC)
	$(MKOCT) -DZINT -DDO_MAP -DDO_VALUES -c $<
	- $(MV) ../Source/umf_triplet.o $@

umf_oz_triplet_map_nox.o: ../Source/umf_triplet.c $(INC)
	$(MKOCT) -DZINT -DDO_MAP -c $<
	- $(MV) ../Source/umf_triplet.o $@

umf_oz_triplet_nomap_x.o: ../Source/umf_triplet.c $(INC)
	$(MKOCT) -DZINT -DDO_VALUES -c $<
	- $(MV) ../Source/umf_triplet.o $@

umf_oz_triplet_nomap_nox.o: ../Source/umf_triplet.c $(INC)
	$(MKOCT) -DZINT -c $<
	- $(MV) ../Source/umf_triplet.o $@

umf_oz_assemble_fixq.o: ../Source/umf_assemble.c $(INC)
	$(MKOCT) -DZINT -DFIXQ -c $<
	- $(MV) ../Source/umf_assemble.o $@

umf_oz_store_lu_drop.o: ../Source/umf_store_lu.c $(INC)
	$(MKOCT) -DZINT -DDROP -c $<
	- $(MV) ../Source/umf_store_lu.o $@

umfpack_oz_wsolve.o: ../Source/umfpack_solve.c $(INC)
	$(MKOCT) -DZINT -DWSOLVE -c $<
	- $(MV) ../Source/umfpack_solve.o $@

umfpack_oz_%.o: ../Source/umfpack_%.c $(INC)
	$(MKOCT) -DZINT -c $<
	- $(MV) ../Source/umfpack_$*.o $@

#----------------------------------------
# Generic routines for OCTAVE
#----------------------------------------

umfpack_o_timer.o: ../Source/umfpack_timer.c $(INC)
	$(MKOCT) -c $<
	- $(MV) ../Source/umfpack_timer.o $@

umfpack_o_tictoc.o: ../Source/umfpack_tictoc.c $(INC)
	$(MKOCT) -c $<
	- $(MV) ../Source/umfpack_tictoc.o $@

#----------------------------------------
# umfpack oct-files
#----------------------------------------

umfpack: umfpack.cc $(OCTUMFPACK) $(OCTAMD)
	$(MKOCT) $(OCT_SPARSE_INC) umfpack.cc $(OCTUMFPACK) $(OCTAMD)  -o umfpack.oct 

luflop: luflop.cc
	$(MKOCT) $(OCT_SPARSE_INC) luflop.cc -o luflop.oct

#----------------------------------------
# umfpack library to link with octave
#----------------------------------------

octave: $(OCTUMFPACK) $(OCTAMD)
	ld -r $(OCTUMFPACK) $(OCTAMD) -o ../../../$(OCTUMFPACK_LIB)

#-------------------------------------------------------------------------------
# Remove all but the files in the original distribution
#-------------------------------------------------------------------------------

purge: clean
	- $(RM) *.oct *.so
