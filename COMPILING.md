# Code compilation

## Windows update 29052026
Intel Fortran Compiler with Visual Studio. Intel ifx compiler 2025 and Visual Studio 2022


## Windows

1. Start Visual Studio 2022. Select "Continue without code".

2. Create a Visual Fortran empty console application. File|New|Project search Fortran and select "Empty Project" ("A project for creating a command-line application")

	Project name 	= SHETRAN
	Location 		= location of the SHETRAN folder containing the src code folder
	Solution name 	= SHETRAN
	
	Click "place a solution and project in the same directory"

	Select "Create"

3. In Project|Add Existing Item... , add the following to the project:
	- Shetran.f90 file from the src folder
	- all the .f90 files from the src/modules folder
	- all the .f90 files from the src/parameters folder
	- mod_load_filedata.f90 from the src/util folder
	- all the .f90 files from the src/visualisation folder (except include_extend_s.f90 and include_increment.f90 which must be present but not in the project)
	- all the .lib from the external/library-files folder. NB "All Files (*.*)"" must be searchable
	- in the Solution explorer (on the right), click on Resource files, then add in Project|Add Exisiting Item select resource1.rc from the src/resource folder

4. Copy the full path for the "include" directory (e.g. C:\Users\sjbir_000\Documents\shetrn\external\Include), and enter in the following place:

	Project|SHETRAN Properties|Configuration Properties|Fortran|General|Additional Include Directories
	
	Instead of "SHETRAN Properties" it might say "Properties" depending on if you have clicked on SHETRAN or a folder within SHETRAN in the Solution explorer.
	Also make sure for this change and the others that the "configuration" says "All Configurations" i.e both debug and release

5. In   Project|SHETRAN Properties|Configuration Properties|Fortran|Optimisation. Set the "Heap Array" to 0 (this always allocates temporary arrays onto the heap rather than onto the stack)

6. In 	Project|SHETRAN Properties|Configuration Properties|Fortran|Libraries change runtime Library to "Multithreaded". (The library code is baked into the .exe)

7. In 	Project|SHETRAN Properties|Configuration Properties|Fortran|Optimization change Optimization to "Maximum Speed" which corresponds to /O2

8. In 	Project|SHETRAN Properties|Configuration Properties|Fortran|Floating Point change Floating Point Model to "fp:precise"

9. In 	Project|SHETRAN Properties|Configuration Properties|Fortran|Preprocessor set Preprocess source File to Yes (/fpp)

10. Build the project using Build|Build Solution(either the debug or the release versions)