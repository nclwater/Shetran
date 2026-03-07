# Code compilation

## Windows
Tested using Intel Fortran Compiler with Visual Studio. Versions 2015-2020.

For a distributable executable that runs without the Intel oneAPI command environment, use static runtime linkage settings (Intel `/libs:static` and MSVC `/MT`) as described below.

1. Create a Visual Fortran empty console application. File|New|Project search Fortran and select "Empty Project" ("A project for creating a command-line application"?)

	Project name 	= Shetran
	Location 		= location of the Shetran folder containing the src code folder
	Solution name 	= Shetran

	Select "Create"


2. In Project|Properties, select "Configuration Manager..." (at the top of the box).

	In Project contexts box, go to Platform, select "<New...>. Then under "New platform", select "x64". Select "OK". Close Configuration Manager.
	Still in Property Pages, under "Configuration:" select "All Configurations".
	Still in Property Pages, go to Configuration Properties|Fortran |Optimisation. Set the "Heap Array" to 0. Select "OK" to close Property Pages

3. In Project|Add Existing Item... , add the following to the project:
	- Shetran.f90 files from the src folder
	- all the .f90 files from the src/modules folder
	- all the .f90 files from the src/parameters folder
	- mod_load_filedata.f90 from the src/util folder
	- all the .f90 files from the src/visualisation forlder (except include_extend_s.f90 and include_increment.f90 which must be present but not in the project)
	- all the .lib from the external/library-files folder. NB "All Files (*.*)"" must be searchable
	- in the Solution explorer (on the right), click on Resource files, then add in Project|Add Exisiting Item select resource1.rc from the src/resource folder

4. Copy the full path for the "inlcude" directory (e.g. C:\Users\sjbir_000\Documents\shetrn\external\Include), and enter in the following place:

	Project|Properties|Configuration Properties|Fortran|General|Additional Include Directories

5. In 	Project|Properties|Configuration Properties|Fortran|Libraries, set Runtime Library to "Multithreaded" (`/libs:static /threads`).

6. In 	Project|Properties|Configuration Properties|C/C++|Code Generation, set Runtime Library to "Multi-threaded (/MT)" for Release (and "Multi-threaded Debug (/MTd)" for Debug).

7. In 	Project|Properties|Configuration Properties|Fortran|Optimization change Optimization to "Minimum Size (/O1)"

8. Build the project using Build|Build Solution(either the debug or the release versions)

If runtime linkage is left as dynamic (`/libs:dll` or `/MD`), the executable will require Intel/MSVC runtime DLLs on PATH and may only run from a oneAPI-enabled environment.