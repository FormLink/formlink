# Makefile for gcc and  Mathematica 10.3 (default)
#
# run
# make clean && make && make install
#
######################################################################
#
#
# Compiling for Mathematica versions below 10.0
#
# Linux Mathematica 8 64-bits:
#
# make clean && make MLINKDIR=/usr/local/Wolfram/Mathematica/8.0/SystemFiles/Links/MathLink/DeveloperKit MLLIB=ML64i3 && make OUTPUT=FormLinkLegacy install
#
# Linux Mathematica 9 64-bits
#
# make clean && make MLINKDIR=/usr/local/Wolfram/Mathematica/9.0/SystemFiles/Links/MathLink/DeveloperKit MLLIB=ML64i3 && make OUTPUT=FormLinkLegacy install
#
# Linux Mathematica 10 32-bits (cross-compile)
#
# make clean && make ARCH=i686 && make ARCH=i686 install
#
# OSX Mathematica 8 64-bits
#
# make clean && make MLINKDIR=/Applications/Mathematica8.app/SystemFiles/Links/MathLink/DeveloperKit CADDSDIR=/Applications/Mathematica8.app/SystemFiles/Links/MathLink/DeveloperKit/CompilerAdditions CLIB=stdc++ MLLIB=MLi3 && make OUTPUT=FormLinkLegacy install
#
# OSX Mathematica 9 64-bits
#
# make clean && make OUTPUT=FormLinkLegacy MLINKDIR=/Applications/Mathematica9.app/SystemFiles/Links/MathLink/DeveloperKit CLIB=stdc++ MLLIB=MLi3 && make OUTPUT=FormLinkLegacy install
#
# OSX Mathematica 10.3 64-bits
#
# make clean && make MLINKDIR=/Applications/Mathematica10.3.app/Contents/SystemFiles/Links/MathLink/DeveloperKit CLIB=stdc++ && make install OUTPUT=FormLink103
#
# Windows Mathematica 8 32-bits
#
# make clean && make OUTPUT=FormLinkLegacy.exe MLINKDIR="/cygdrive/c/Program\ Files/Wolfram\ Research/Mathematica/8.0/SystemFiles/Links/MathLink/DeveloperKit" MLLIB=ml32i3m && make OUTPUT=FormLinkLegacy.exe install
#
# Windows Mathematica 9 32-bits
#
# make clean && make  OUTPUT=FormLinkLegacy.exe MLINKDIR="/cygdrive/c/Program\ Files/Wolfram\ Research/Mathematica/9.0/SystemFiles/Links/MathLink/DeveloperKit" MLLIB=ml32i3m && make OUTPUT=FormLinkLegacy.exe install



UNAME := $(shell uname)

ifneq ($(UNAME), Darwin)
	UNAMEWIN := $(shell uname -o)
endif
ARCH := $(shell uname -m)

ifeq ($(UNAME), Linux)
  # Linux specific settings
  ifeq ($(ARCH), x86_64)
    SYS = Linux-x86-64
    EXTRA_CFLAGS=-m64
    MLLIB = ML64i4
    INSTALLDIR = bin/linux64
  else
    ifeq ($(ARCH), i686)
      SYS=Linux
      EXTRA_CFLAGS=-m32 -L$(shell dirname $$(readlink -f $$(which math)))/../SystemFiles/Libraries/Linux
      MLLIB = ML32i4
      INSTALLDIR = bin/linux32
    endif
  endif
  CC=gcc
  MLINKDIR = $(shell dirname $$(readlink -f $$(which math)))/../SystemFiles/Links/MathLink/DeveloperKit
  BINARIES = FormLinkLinux
  CADDSDIR = ${MLINKDIR}/${SYS}/CompilerAdditions
  INCDIR = ${CADDSDIR}
  LIBDIR = ${CADDSDIR}
  MPREP=${CADDSDIR}/mprep
  OUTPUT=FormLink
else
  ifeq ($(UNAME), Darwin)
    # Mac OSX specific settings
    CC=/usr/local/bin/gcc-5
    SYS=MacOSX-x86-64
    MLLIB = MLi4
    CLIB = c++
    INSTALLDIR = bin/macosx64
    MLINKDIR= /Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit
    BINARIES = FormLinkOSX
    CADDSDIR = ${MLINKDIR}/${SYS}/CompilerAdditions
    INCDIR = ${CADDSDIR}
    LIBDIR = ${CADDSDIR}
    MPREP=${CADDSDIR}/mprep
    OUTPUT=FormLink
  else
    ifeq ($(UNAMEWIN), Cygwin)

		ifeq ($(ARCH), x86_64)
			# This doesn't seem to work...
			SYS=Windows-x86-64
			EXTRA_CFLAGS=-mwindows -c -mlarge
			MLDEV = mldev64
			MLLIB = ml64i4
			INSTALLDIR = bin/windows64
		else
			ifeq ($(ARCH), i686)
				SYS=Windows
				EXTRA_CFLAGS=-mwin32 -mwindows -c
				MLDEV = mldev32
				MLLIB = ml32i4
				INSTALLDIR = bin/windows
			endif
		endif

		MLINKDIR="/cygdrive/c/Program Files/Wolfram Research/Mathematica/10.3/SystemFiles/Links/MathLink/DeveloperKit"
		CADDSDIR = ${MLINKDIR}/${SYS}/CompilerAdditions/${MLDEV}
		INCDIR = ${CADDSDIR}/include
		LIBDIR = ${CADDSDIR}/lib
		MPREP=${CADDSDIR}/bin/mprep.exe
		#important:
		EXTRALIBS = -mwindows
		BINARIES = FormLinkWindows
		OUTPUT=FormLink.exe
	endif

  endif

endif

$(info  ARCH is $(ARCH))
$(info  SYS is ${SYS})
$(info  MathLink directory is ${MLINKDIR})

RM = rm
CCFLAGS=
all : $(BINARIES)

FormLinkLinux : FormLinktm.o FormLink.o
	${CC} ${EXTRA_CFLAGS} -I${INCDIR} FormLinktm.o FormLink.o -L${LIBDIR} -ldl -l:libuuid.so.1 -Wl,-Bstatic -l${MLLIB} -Wl,-Bdynamic -lm -lpthread -lrt -lstdc++  -o FormLinkLinux

FormLinkOSX : FormLinktm.o FormLink.o
	${CC} -I${INCDIR} FormLinktm.o FormLink.o -L${LIBDIR} -l${MLLIB} -l${CLIB} -framework Foundation -o FormLinkOSX

FormLinkWindows : FormLinktm.o FormLink.o
	${CC} -I${INCDIR} FormLinktm.o FormLink.o -L${LIBDIR} ${EXTRALIBS} -lgcc -l${MLLIB} -o $@

.c.o :
	${CC} -c ${EXTRA_CFLAGS} -I${INCDIR} $<

FormLinktm.c : FormLink.tm
	${MPREP} $? -o FormLinktm.c

clean :
	@rm -rf *.o *tm.c $(BINARIES)

install : $(BINARIES)
	install -m 0755 $(BINARIES) ../${INSTALLDIR}/${OUTPUT}

.PHONY : install clean
