#
# makefile
# created : ${current_time}
#
# timer function define macro :
#     PROF_TIMEOFDAY = gettimeofday
#     PROF_GETTICK = gettick
# running program define macro :
#     KSCOPE_KERNEL = only kernel program
#     KSCOPE_MPI    = only application program, when parallel running
# compiler option  :  () = running option
#                     FJ:fujitsu     GNU                      INTEL
#    Optimize         -O[0|1|2|3]    -O[0|1|2|3]              -O[0|1|2|3]
#                     -Kfast         -Ofast                   -fast
#    no-underscore     /             -fno-underscoring        -assume nounderscore
#    OpenMP            -Kopenmp      -fopenmp                 -openmp
#    Endian:little     (-Wl,-T)      -fconvert=little-endian  -convert little_endian
#           big                      -fconvert=big-endian     -convert big_endian
#

# for debug env in @hira
#export PATH := /usr/local/openmpi-1.8.4_gcc/bin:$(PATH)
#export PATH := /usr/local/openmpi-1.8.4_intel/bin:$(PATH)
#export LD_LIBRARY_PATH := /opt/intel/lib/intel64:$(LD_LIBRARY_PATH)

# kernel complier :
#    FJ    = fijitsu compiler in k computer
#    GNU   = gnu compliler
#    INTEL = intel compiler
KERNEL_COMP = FJ
#KERNEL_COMP = GNU
#KERNEL_COMP = INTEL

FFLAGS =
CFLAGS =
LIBS =
# select compiler
ifeq ($(KERNEL_COMP), FJ)    # for fujitsu compiler
    FC = mpifrtpx
    CC = mpifccpx
    FFLAGS += -Cpp
    FFLAGS += -Kfast,parallel
    FFLAGS += -Kopenmp,ocl -Qt
#    LIBS += -I/home/apps/fftw/3.3.3/include -L/home/apps/fftw/3.3.3/lib64
#    LIBS += -SCALAPACK -SSL2BLAMP -lfftw3_omp -lfftw3 -lm
    CFLAGS = -Xg
else ifeq  ($(KERNEL_COMP), GNU)    # for gnu compiler
    #FC = gfortran
    FC = mpif90
    CC = mpicc
    FFLAGS += -cpp
    FFLAGS += -O3
    FFLAGS += -fopenmp
    FFLAGS += -mcmodel=large
#    FFLAGS += -fno-underscoring
else ifeq  ($(KERNEL_COMP), INTEL)    # for intel compiler
    #FC = ifort
    FC = mpif90
    CC = mpicc
    FFLAGS += -cpp
    FFLAGS += -O3
    FFLAGS += -openmp
    FFLAGS += -mcmodel=large -shared-intel
    FFLAGS += -list
#    FFLAGS += -assume nounderscore
endif

# only kernel program
FFLAGS += -DKSCOPE_KERNEL
#
# timer function macro : PROF_TIMEOFDAY=gettimeofday | PROF_GETTICK=gettick
#
FFLAGS += -DPROF_TIMEOFDAY
#FFLAGS += -DPROF_GETTICK

# set CMP_FFLAGS for kscope_cmp program
ifeq ($(KERNEL_COMP), FJ)    # for fujitsu compiler
    CMP_FFLAGS = -Cpp -Kfast
else                         # for gnu and intel compiler
    CMP_FFLAGS = -cpp -O3
endif

# set CPPFLAGS for C language
CPPFLAGS = -E -std=gnu99 -Wall -Wpointer-arith

# program name
PROG = ./kernel.out
CMP_PROG = ./kscope_cmp.out

LINK  = $(LIBS)

SRCS_F77 =

SRCS  =  \
#foreach ( ${file} in ${module_files})
	${file} \
#end
	kscope_mod_bounds.f90 \
	kscope_mod_fileio.f90 \
	kscope_mod_cmp.f90 \
	kscope_mod_prof.f90 \
	kscope_mod_kernel.f90 \
	kscope_main.f90

CMP_SRCS  =  \
	kscope_mod_bounds.f90 \
	kscope_mod_fileio.f90 \
	kscope_mod_cmp.f90 \
	kscope_cmp.f90

C_SRCS = \
	get_time.c \
	get_timef.c

#OBJS = $(SRCS:.f90=.o) $(SRCS_F77:.f=.o) $(C_SRCS:.c=.o)
OBJS = $(patsubst %.f90,%.o,$(patsubst %.F90,%.o,$(SRCS)))
OBJS += $(patsubst %.f,%.o,$(patsubst %.F,%.o,$(SRCS_F77)))
OBJS += $(C_SRCS:.c=.o)
CMP_OBJS = $(CMP_SRCS:.f90=.cmp.o)

.SUFFIXES: .f90 .f .F90 .F .mod .o

# target: all - build all program
all: $(PROG) $(CMP_PROG)

$(PROG): $(OBJS)
	@ echo
	@ echo "Linking kernel program ...."
	@ echo
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LINK)
	@ echo
	@ echo "Done."
	@ echo

$(CMP_PROG): $(CMP_OBJS)
	@ echo
	@ echo "Linking compare program ...."
	@ echo
	$(FC) $(CMP_FFLAGS) -o $@ $(CMP_OBJS) $(LINK)
	@ echo
	@ echo "Done."
	@ echo

# target: help - Display callable targets.
help:
	@egrep "^# target:" [Mm]akefile

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@
%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@
%.o: %.f
	$(FC) $(FFLAGS) -c $< -o $@
%.o: %.F
	$(FC) $(FFLAGS) -c $< -o $@
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
%.cmp.o : %.f90
	$(FC) $(CMP_FFLAGS) -c $< -o $@

# target: clean - delete *.o, *.mod files
clean:
	rm -f $(OBJS)
	rm -f $(CMP_OBJS)
	rm -f *.mod
	rm -f *.lst
	rm -f $(PROG)
	rm -f $(CMP_PROG)

# target: allclean - delete *.o, *.mod and program file
allclean: clean

.PHONY : clean allclean all help

