# %W% revision of %E%  %U%
# This is an unpublished work copyright (c) 1992 HELIOS Software GmbH
# 30159 Hannover, Germany

# Do not edit at HELIOS, edit GNUmakefile instead!

SHELL=/bin/sh
IPATH=
DEFS=
DEBUGFLAGS=-g -DDEBUG
NODEBUGFLAGS=-O
CFLAGS=$(IPATH) $(DEFS) $(NODEBUGFLAGS)
TARGET=libdl.a
DEST=/usr/local/lib
HDRS=dlfcn.h
SRCS=dlfcn.c
NOSTART=-e _nostart	# for AIX 3
#NOSTART=-bnoentry	# for AIX 4
OBJS=$(SRCS:%.c=%.o)

all:		$(TARGET) dlfcn.c

dlfcn.o:	dlfcn.h

$(TARGET):	$(OBJS) dl.exp
		$(CC) $(CFLAGS) -o $@ $(OBJS) -bE:dl.exp -bM:SRE $(NOSTART) -lld

lint:
		lint $(IPATH) $(DEFS) $(SRCS) >lintout

info:
		sccs info

clean:
		rm -f lintout a.out core *.o *-lg *% *~ tags deps%

clobber:	clean
		rm -f $(TARGET) deps

install:	all
		cp $(TARGETS) $(DEST)

shar:
		shar README Makefile dlfcn.h dlfcn.c dl.exp >dlfcn.shar
