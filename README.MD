# FormLink

[![license: LGPLv3](https://img.shields.io/badge/license-LGPLv3-brightgreen.svg)](https://github.com/FormLink/formlink/LICENSE.md)

FormLink is a Mathematica package to link Mathematica with FORM.

FormLink was developed by Feng Feng and Rolf Mertig. The code is maintained by Feng Feng, Rolf Mertig and Vladyslav Shtabovenko

# Installation

	Import["https://raw.githubusercontent.com/FormLink/formlink/master/install.m"]
	InstallFormLink[]


# Compilation

If you use the automatic installer via `InstallFormLink[]` there should be no necessity to recompile anything. However, if for some reasons you need to recompile FormLink binaries, following instructions should be useful.

## Linux

Apart from Mathematica, you need to have  __libdl__, __libuuid__, __libm__, __libpthread__ , __librt__, __libstdc++__ installed on your system.

### Mathematica 8 32-bits

	 make clean && make ARCH=i686 MLINKDIR=/usr/local/Wolfram/Mathematica/8.0/SystemFiles/Links/MathLink/DeveloperKit MLLIB=ML32i3 && make OUTPUT=FormLinkLegacy ARCH=i686 install


### Mathematica 8 64-bits

	 make clean && make MLINKDIR=/usr/local/Wolfram/Mathematica/8.0/SystemFiles/Links/MathLink/DeveloperKit MLLIB=ML64i3 && make OUTPUT=FormLinkLegacy install


### Mathematica 9 32-bits

	make clean && make ARCH=i686 MLINKDIR=/usr/local/Wolfram/Mathematica/9.0/SystemFiles/Links/MathLink/DeveloperKit MLLIB=ML32i3 && make OUTPUT=FormLinkLegacy ARCH=i686 install

### Mathematica 9 64-bits

	make clean && make MLINKDIR=/usr/local/Wolfram/Mathematica/9.0/SystemFiles/Links/MathLink/DeveloperKit MLLIB=ML64i3 && make OUTPUT=FormLinkLegacy install

### Mathematica 10 32-bits

	make clean && make ARCH=i686 && make ARCH=i686 install

### Mathematica 10 64-bits

	make clean && make && make install

## OS X

Apart from Mathematica, you need to have  __gcc5__ (from homebrew etc.) intalled on your system.

### Mathematica 8

	make clean && make MLINKDIR=/Applications/Mathematica8.app/SystemFiles/Links/MathLink/DeveloperKit CADDSDIR=/Applications/Mathematica8.app/SystemFiles/Links/MathLink/DeveloperKit/CompilerAdditions CLIB=stdc++ MLLIB=MLi3 && make OUTPUT=FormLinkLegacy install

### Mathematica 9

	make clean && make OUTPUT=FormLinkLegacy MLINKDIR=/Applications/Mathematica9.app/SystemFiles/Links/MathLink/DeveloperKit CLIB=stdc++ MLLIB=MLi3 && make OUTPUT=FormLinkLegacy install

### Mathematica 10.3

	make clean && make MLINKDIR=/Applications/Mathematica10.3.app/Contents/SystemFiles/Links/MathLink/DeveloperKit CLIB=stdc++ && make install OUTPUT=FormLink103

### Mathematica 10.4

	make clean && make && make install

## Windows

Apart from Mathematica, you need to have  __cygwin__ (32-bit version!) intalled on your system. The compilation is done in the cygwin terminal.

### Mathematica 8

	make clean && make OUTPUT=FormLinkLegacy.exe MLINKDIR="/cygdrive/c/Program\ Files/Wolfram\ Research/Mathematica/8.0/SystemFiles/Links/MathLink/DeveloperKit" MLLIB=ml32i3m && make OUTPUT=FormLinkLegacy.exe install

### Mathematica 9

	make clean && make  OUTPUT=FormLinkLegacy.exe MLINKDIR="/cygdrive/c/Program\ Files/Wolfram\ Research/Mathematica/9.0/SystemFiles/Links/MathLink/DeveloperKit" MLLIB=ml32i3m && make OUTPUT=FormLinkLegacy.exe install

### Mathematica 10

	make clean && make && make install

# License

FormLink is covered by the GNU Lesser General Public License 3.

Copyright (C) 2012-2016 Feng Feng 
Copyright (C) 2012-2016 Rolf Mertig 
Copyright (C) 2015-2016 Vladyslav Shtabovenko

FormLink is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

FormLink is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with FeynCalc.  If not, see <http://www.gnu.org/licenses/>.


The conditions for the use of FORM are laid out here: http://www.nikhef.nl/~form/license/license.html 
and should be followed of course also when using FormLink.

The license for using Mathematica is given here:
http://www.wolfram.com/legal/agreements/wolfram-mathematica.html
