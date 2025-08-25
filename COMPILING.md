# Code compilation

**Note:** The recommended method for compiling SHETRAN is to use the automated CMake build system as described in `CMAKE_BUILD.md`. The following instructions for a manual Visual Studio project setup are provided for advanced users or for debugging purposes, but may be difficult to maintain due to the complexity of the source code dependencies.

## Windows
Tested using Intel Fortran Compiler with Visual Studio. Versions 2015-2020.

1. Create a Visual Fortran empty console application. File|New|Project search Fortran and select "Empty Project" ("A project for creating a command-line application"?)

	Project name 	= Shetran
	Location 		= location of the Shetran folder containing the src code folder
	Solution name 	= Shetran

	Select "Create"

2. In Project|Properties, select "Configuration Manager..." (at the top of the box).

	In Project contexts box, go to Platform, select "<New...>. Then under "New platform", select "x64". Select "OK". Close Configuration Manager.
	Still in Property Pages, under "Configuration:" select "All Configurations".
	Still in Property Pages, go to Configuration Properties|Fortran |Optimisation. Set the "Heap Array" to 0 (for ifort) to allow for dynamic array allocation on the heap. Select "OK" to close Property Pages

3. In Project|Add Existing Item... , add the following to the project:
	- **Important Pre-step**: The build system uses different versions of the `getdirqq.f90` utility for different platforms. For a manual Windows build with the Intel compiler, first navigate to the `src/util/` directory, make a copy of `getdirqq_winIntel.f90`, and rename the copy to `getdirqq.f90`.
	- Add all Fortran source files (`.f90`, `.F90`) from the `src` directory and all of its subdirectories (including `compute`, `io`, `modules`, `parameters`, `simulation`, `util`, and `visualisation`).
	- When adding files, ensure you **exclude the template files** `getdirqq_portable.f90` and `getdirqq_winIntel.f90` from the project to avoid compilation errors.
	- Add all library files (`.lib`) from the `external/library-files` folder. You may need to change the file filter to "All Files (*.*)".
	- In the Solution Explorer, right-click on "Resource Files", select "Add" -> "Existing Item...", and add `Resource1.rc` from the `src/resource` folder.

4. Copy the full path for the "inlcude" directory (e.g. C:\Users\sjbir_000\Documents\shetrn\external\Include), and enter in the following place:

	Project|Properties|Configuration Properties|Fortran|General|Additional Include Directories

5. In 	Project|Properties|Configuration Properties|Fortran|Libraries change runtime Library to "Multithreaded"

6. In 	Project|Properties|Configuration Properties|Fortran|Optimization change Optimization to "Minimum Size (/O1)"

7. Build the project using Build|Build Solution(either the debug or the release versions)