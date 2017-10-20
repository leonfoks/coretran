<a name="top"></a>

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()

An easy to follow library to make Fortran easier in general with wrapped interfaces, sorting routines, kD-Trees, and other algorithms to handle scientific data and concepts. The library contains core fortran routines and object-oriented classes.

### Why coretran?

I used Fortran extensively during my PhD to solve very large systems of equations with application to the inversion of geophysical data for 3D subsurface physical property models. I developed algorithms that utilized random point clouds in space, structured rectilinear, triangular, and voronoi meshes, and the unstructured versions of these.  Fortran has relatively little modern and freely available source code, compared to other languages, which have easy to use libraries that perform these types of operations. 

I struggled as a beginner coming in to Fortran because the basic functions that handle numbers were not readily available. It was frustrating that I had to write (and then duplicate) my own error checking when allocating memory or opening a file.  What also frustrated me was when I had to write the same function/subroutine multiple times for  different input types like integers or real numbers.

**I wonder, how many people have written their own function as basic as computing a mean of some numbers?**

This was initially the driving motivation for me to develop this library.  The simple fact that Fortran does not have these basic functions readily available, and that any user starting from scratch would have to write their own. I humbly hope that this library will help to alleviate this issue, by providing functions/subroutines with complexities that range from the most basic to the more advanced, but all in pure Fortran.  The effect is hopefully similar to a Python user who has immediate access to amazing packages such as numpy and scipy.

The code comes with a complete set of [source code documentation](https://leonfoks.github.io/coretran/) that is easily generated into html pages. These docs also contain working examples on how to run each function and subroutine within the library.  Also included in the docs are the references to papers or online material that I used.

This library is written using modern Fortran standards with modules, sub modules, and object oriented derived types.

The code can be compiled easily across platforms using [CMake](https://cmake.org/).


#### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-pass%20(v6.0.1+)-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-pass%20(v16.x+)-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()

## [Read the source documentation](https://leonfoks.github.io/coretran)

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
1. Installing a Fortran compiler to create the libraries
2. Installing the software build tool that compiles the codes in the correct order, so you don't have to!
3. Install Python and the software to generate the source code documentation locally (optional! You could just go [here](https://leonfoks.github.io/coretran/))

### Installing the GNU fortran compiler <a name="gfortran"></a>
To compile my library, I have been using gfortran version 6.3.0 because of the use of submodules and other 2008 standards. I you want to use Intel's fortran compiler you will need ifort version 16+ or 17+

#### * Linux 
If you work in Linux you should be well versed in installing packages on your system. At the time of writing this I am running on Ubuntu 16.04 LTS.

#### * Mac
On a Mac, I use Brew to manage my libraries/programs.  So type "brew install gcc" and you should be golden.

#### * Windows
Windows is a little trickier but it is still easy! The easiest way I have found is to use [MinGW](http://mingw-w64.org/doku.php). There is a good tutorial on installing mingw [here](https://computingabdn.com/softech/mingw-howto-install-gcc-for-windows/). I will summarize below.

Go to [MinGW - Downloads](http://mingw-w64.org/doku.php/download), and click on "Mingw-builds" in the table to redirect to sourceforge.  This will download the installer. 

When you run the installer, choose the following

* Version: 6.3.0 (minimum) you may choose higher 
* Architecture: x86_64
* Threads: posix
* Exception: sjlj
* Build revision: The highest number

You should change the Destination Folder so that the path does not contain any spaces.

Add an environment variable called "MinGW\_Home" and point it to the bin directory in your chosen destination folder. e.g. "C:\programs\mingw-w64\x86_64-6.3.0-posix-sjlj-rt_v5-rev1\mingw64\bin"

I don't require MSYS to build my library so you can stop here if you like. I do however suggest making an alias to the MinGW32-make executable, this is optional, so later in these instructions simply replace "make" with "mingw32-make" if you choose not to do this.

Go to the bin directory in your installation folder, on my system it is "C:\programs\mingw-w64\x86_64-6.3.0-posix-sjlj-rt_v5-rev1\mingw64\bin" and create a file called "make.bat"

Edit the file with a text editor and add the following single line "mingw32-make %\*"
This allows you to use the command "make" on windows which we will need later to build to library.

## Compilation <a name="compile"></a>
The library can be compiled with any custom approach you choose, however to make life easier, I have included instructions on how to use cmake to generate makefiles. Cmake allows you to easily build the same source code on Windows, OSX, and Linux with no changes.  I always found make files cumbersome on Windows so Cmake made my life very easy.  The portability of Cmake to multiple architectures was the driving reason for me choosing it, plus Cmake can handle the newer, more modern, aspects of Fortran 2003, 2008+, and handles source code dependencies very well.

* Compilation :  [CMAKE: an open-source, cross-platform family of tools designed to build, test and package software](https://cmake.org/)

#### Installing Cmake

The installation of cmake should be quite easy. Similar to the section above on installing [gfortran](#gfortran).

On a Mac, I simply use brew again.  

On windows, you should be able to download the installation binary from their [website](https://cmake.org/download/). 


#### Compiling the coretran library

To compile the coretran library, navigate to the root directory and create a new folder "build".

Change directory to build and type 

```
cmake -DCMAKE_BUILD_TYPE=[DEBUG,RELEASE] -G "Insert type here" ../src
```

* -D
	* CMAKE\_BUILD\_TYPE 
		* DEBUG will add all debugging flags, tracebacks, and array bound checking.  This is great for development.
		* RELEASE will turn of the less efficient checks, and will compile with higher levels of optimization.  This is great for production runs once everything is debugged.
	* CMAKE\_Fortran\_COMPILER
		* If cmake does not use the compiler you need, you can specify the path to the compiler you want. I had some issues on OSX with the intel compiler and gfortran both installed.  Using -G "Unix Makefiles" would always detect the intel compiler.  To force cmake to use gfortran, I used -DCMAKE\_Fortran\_COMPILER=/usr/local/Cellar/gcc/6.2.0/bin/gfortran (Remember I used brew to install gfortran, and that the version number might change!) 

* -G 

	Cmake can generate makefiles for a multitude of different compilers.  The -G option specifies which compiler you wish to generate make files for.

	If you type 

	```
	cmake -h
	```

	You will see not only the man pages for cmake, but also a list of generators e.g.

	```
	Generators

	The following generators are available on this platform:
  	Unix Makefiles               = Generates standard UNIX makefiles.
	Ninja                        = Generates build.ninja files.
   Xcode                        = Generate Xcode project files.
   CodeBlocks - Ninja           = Generates CodeBlocks project files.
   CodeBlocks - Unix Makefiles  = Generates CodeBlocks project files.
   CodeLite - Ninja             = Generates CodeLite project files.
   CodeLite - Unix Makefiles    = Generates CodeLite project files.
   Sublime Text 2 - Ninja       = Generates Sublime Text 2 project files.
   Sublime Text 2 - Unix Makefiles
                               = Generates Sublime Text 2 project files.
   Kate - Ninja                 = Generates Kate project files.
   Kate - Unix Makefiles        = Generates Kate project files.
   Eclipse CDT4 - Ninja         = Generates Eclipse CDT 4.0 project files.
   Eclipse CDT4 - Unix Makefiles= Generates Eclipse CDT 4.0 project files.
   KDevelop3                    = Generates KDevelop 3 project files.
   KDevelop3 - Unix Makefiles   = Generates KDevelop 3 project files.
	```

	On Windows, you should also see a generator for "MinGW Makefiles"

	So choose which one you want.  
	
	On OSX and Linux I have always used \"Unix Makefiles\".
	
	On Windows, I have used \"MinGW Makefiles\" or \"NMake Makefile\" for gfortran or intel respectively.

#### Compiling the coretran test code

Most software libraries have a built in test build function when the library is built.  I have stayed away from this so that you can see how I build my test such that it links to the coretran library.  You can duplicate my process for your own programs.

Navigate to the test folder of coretran.

Create a new folder "build" and change directory to that folder.

Run the same cmake command you used to build the coretran library.

Run the test script!  It should show whether each function has passed or failed, and may show timings for the sorting routines.

#### How to use coretran in your library or program

Once the coretran library is compiled, you can easily use it in your own program.  When you compile your Fortran codes to object files, simply use -I/path/to/coretran/include.  When you link your objects, use -L/path/to/coretran/lib and -lcoretran.





## Documentation <a name="docs"></a>

Any good software library should come with documentation. In this library, I have extensive source code comments that you should be able to follow.  The source code comments also contain snippets of code that show how to use the functions I have written. In addition to this, the source code comments can be used to automatically generate html pages.  These pages are fully searchable and very nicely organized.  This is where Ford comes in.

The html documentation for the source code be created locally using the [ford Fortran Documentation Tool](https://github.com/cmacmackin/ford).  If you don't want to create it locally you can simply see it [here](https://leonfoks.github.io/coretran).

You can install Ford like any other Python module using "pip install ford".  

#### Installing and Setting up Python <a name="python"></a>

I have so far found the [Anaconda Distribution of Python](https://www.continuum.io/downloads) to be the most friendly cross platform distribution.  Go ahead and install Anaconda on your system, if you are on Windows, allow the installer to modify your environment variables (so you don't have to later).  Ford uses python version 3+ so be sure to get the correct installer.

Ford can also generate a dependency graph of the fortran modules by using [Graphviz](http://www.graphviz.org/), you will need to install Graphviz separately.

* **Windows Graphviz Installation**

   I downloaded the .msi files from the Graphviz website, I then had to add the following path to my environment variables "C:\Program Files (x86)\Graphviz2.38\bin" so that Ford can use the executable.  (Locate the folder where you installed Graphviz, if it is different to my path, add that to your environment variables instead).

* **OSX Graphviz Installation**

  To manage my programs I use [Homebrew](http://brew.sh/). To install Graphviz, I simply used "brew install Graphviz"

#### Generating the docs

Once Ford and perhaps Graphviz are installed, simply navigate to the root coretran folder in a terminal/command prompt and type "ford Docs.md". This will generate html pages under the docs folder.



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

