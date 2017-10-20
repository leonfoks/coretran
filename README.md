<a name="top"></a>

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()

An easy to follow library to make Fortran easier in general with wrapped interfaces, sorting routines, kD-Trees, and other algorithms to handle scientific data and concepts. The library contains core fortran routines and object-oriented classes.

### Why coretran?

I used Fortran extensively during my PhD to solve very large systems of equations with application to the inversion of geophysical data for 3D subsurface physical property models. I developed algorithms that utilized random point clouds in space, structured rectilinear, triangular, and voronoi meshes, and the unstructured versions of these.  Fortran has relatively little modern and freely available source code, compared to other languages, which have easy to use libraries that perform these types of operations. 

I struggled as a beginner coming in to Fortran because the basic functions that handle numbers were not readily available. It was frustrating that I had to write (and then duplicate) my own error checking when allocating memory or opening a file.  What also frustrated me was when I had to write the same function/subroutine multiple times for  different input types like integers or real numbers.

**I wonder, how many people have written their own function as basic as computing a mean of some numbers?**

This was initially the driving motivation for me to develop this library.  The simple fact that Fortran does not have these basic functions readily available, and that any user starting from scratch would have to write their own. I humbly hope that this library will help to alleviate this issue, by providing functions/subroutines with complexities that range from the most basic to the more advanced, but all in pure Fortran.  The effect is hopefully similar to a Python user who has immediate access to amazing packages such as numpy and scipy.

The code comes with a complete set of source code documentation that is easily generated into html pages. These docs also contain working examples on how to run each function and subroutine within the library.  Also included in the docs are the references to papers or online material that I used.

This library is written using modern Fortran standards with modules, sub modules, and object oriented derived types.

The code can be compiled easily across platforms using [CMake](https://cmake.org/).


#### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-pass%20(v6.0.1+)-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-pass%20(v16.x+)-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()

---

[Main features](#main-features) | [Documentation](#docs) | [Compiling](#compile) | [An example of coretran](#example)

---

## Main features

All functions and subroutines are interfaces, they work no matter the input type whether it real, or integer etc. where it makes sense.


## Licensing

coretran is an open source project, it is distributed under the GPL v3 licensing system:
[GPL v3](http://www.gnu.org/licenses/gpl-3.0.html)

Go to [Top](#top)

## Getting Ready for Compiling and Creating the Documentation
There are three aspects that we need to address
1. Installing a fortran compiler to create the libraries
2. Installing a software build tool that compiles the codes in the correct order, so you don't have to!
3. Install Python and the software to generate the source code documentation

### Installing the GNU fortran compiler <a name="gfortran"></a>
To compile my library, I have been using gfortran version 6.3.0 because of the use of submodules and other 2008 standards. I you want to use Intel's fortran compiler you will need ifort version 16+ or 17+

#### * Linux 
If you work in Linux you should be well versed in installing packages on your system. At the time of writing this I am running on Ubuntu 16.04 LTS.

#### * Mac
On a Mac, I use Brew to manage my libraries/programs.  So type "brew install gfortran" and you should be golden.

#### * Windows
Windows is a little trickier but it is still easy! The easiest way I have found is to use [MinGW](http://mingw-w64.org/doku.php). There is a good tutorial on installing mingw [here](https://computingabdn.com/softech/mingw-howto-install-gcc-for-windows/). I will summarize below.

Go to MinGW - Downloads, and click on "Mingw-builds" in the table to redirect to sourceforge.  This will download the installer. 

When you run the installer, choose the following

* Version: 6.3.0 (minimum) you may choose higher 
* Architecture: x86_64
* Threads: posix
* Exception: sjlj
* Build revision: The highest number

You should change the Destination Folder so that the path does not contain any spaces.

Add an environment variable called "MinGW_Home" and point it to your chosen destination folder.

I don't require MSYS to build my library so you can stop here if you like. I do however suggest making an alias to the MinGW32-make executable, this is optional, so later in these instructions simply replace "make" with "mingw32-make" if you choose not to do this.

Go to the bin directory in your installation folder, on my system it is "C:\programs\mingw-w64\x86_64-6.3.0-posix-sjlj-rt_v5-rev1\mingw64\bin" and create a file called "make.bat"

Edit the file with a text editor and add the following single line "mingw32-make %\*"
This allows you to use "make" on windows which we will need later to build to library.

To compile my source code and generate the documentation for it, I use two packages, both written in Python. 

* Compilation :  [CMAKE: an open-source, cross-platform family of tools designed to build, test and package software](https://cmake.org/)
* Documentation :  [Ford - FORtran Documenter](https://github.com/cmacmackin/ford)

Cmake allows you to compile my library very easily without having to write your own build scripts. I already did the hard work so you don't have to.  Any good software library should come with documentation. In this library, I have extensive source code comments that you should be able to follow.  The source code comments also contain snippets of code that show how to use the functions I have written. In addition to this, the source code comments can be used to automatically generate html pages.  These pages are fully searchable and very nicely organized.  This is where Ford comes in.

In the next few sections I briefly discuss the installation of cmake.  Followed by the installation of Ford.  Since Ford is a python package, I will also discuss the installation of python too.

#### Installing Cmake ####

The installation of cmake should be quite easy. Similar to the section above on installing [gfortran](#gfortran).

On a Mac, I simply use brew again.  On windows, you should be able to download the installation binary from their website. 

#### Installing and Setting up Python <a name="python"></a>

I have so far found the [Anaconda Distribution of Python](https://www.continuum.io/downloads) to be the most friendly cross platform distribution.  Go ahead and install Anaconda on your system, if you are on Windows, allow the installer to modify your environment variables (so you don't have to later).  Ford uses python version 3+ so be sure to get the correct installer.

## Compiling the Fortran libraries

To compile the source codes I have chosen to use [CMAKE: an open-source, cross-platform family of tools designed to build, test and package software](https://cmake.org/). Cmake allows you to easily build the same source code on Windows, OSX, and Linux with no changes.  I always found make files cumbersome on Windows so Cmake made my life very easy.  The portability of Cmake to multiple architectures was the driving reason for me choosing it, plus Cmake can handle the newer, more modern, aspects of Fortran 2003, 2008+.
 

#### Compiling the Fortran Code







## Documentation <a name="docs"></a>

The html documentation for the source code be created locally using the [ford Fortran Documentation Tool](https://github.com/cmacmackin/ford). 

You can install Ford like any other Python module using "pip install ford".  

Ford can also generate a dependency graph of the fortran modules by using [Graphviz](http://www.graphviz.org/), you will need to install Graphviz separately.

* **Windows Graphviz Installation**

   I downloaded the .msi files from the Graphviz website, I then had to add the following path to my environment variables "C:\Program Files (x86)\Graphviz2.38\bin" so that Ford can use the executable.  (Locate the folder where you installed Graphviz, if it is different to my path, add that to your environment variables instead).

* **OSX Graphviz Installation**

  To manage my programs I use [Homebrew](http://brew.sh/). To install Graphviz, I simply used "brew install Graphviz"

Go to [Top](#top)

## An example of coretran <a name="example"></a>

Here is a small example of how this library makes Fortran easier to use (especially for a beginner), and how it can clean up your code. While this example is not extensive it should give you an idea.

### Fortran code to allocate an array, create some numbers, and compute their mean.

```fortran
program theMean
use, intrisic :: iso_fortran_env, only: real64, int32
implicit none
real(real64), allocatable :: a(:)
real(real64) :: theMean
! Some parameters to handle everything
integer(int32) :: i, istat, N
N=1000

! Allocate a and check for errors
allocate(a(N), stat=istat)
if (istat /= 0) then
  stop "Could not allocate a"
endif

! Create numbers from 1 to N
a = [(real(i,kind=real64), i=1,N)]

! Compute the mean
theMean = sum(a)/real(N,kind=real64)

! Deallocate memory
deallocate(a, stat=istat)
if (istat /= 0) then
  stop "Could not deallocate a"
endif
end program
```
### The same code using coretran
```Fortran
program theMean
use variableKind, only: r64,i32
use m_allocate, only: allocate
use m_deallocate, only: deallocate
use m_array1D, only: arange
use m_maths only: mean
implicit none
real(real64), allocatable :: a(:)
real(real64) :: theMean
integer(int32) :: N
N=1000

! Allocate a and check for errors
call allocate(a,N)

! Create numbers from 1 to N
call arange(a, 1.d0, 1000.d0)

! Compute the mean
theMean = mean(a)

! Deallocate memory
call deallocate(a)
end program
```

