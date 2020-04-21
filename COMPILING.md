# Code compilation

## Windows
Tested using Intel Fortran Compiler with Visual Studio. Versions 2015-2020.

1. Create a Visual Fortran empty console application. File|New|Project search Fortran and select "Empty Project" ("A project for creating a command-line application"?)

	Project name 	= Shetran
	Location 		= location of the Shetran folder containing the source code folder
	Solution name 	= Shetran

	Select "Create"


2. In Project|Properties, select "Configuration Manager..." (at the top of the box).

	In Project contexts box, go to Platform, select "<New...>. Then under "New platform", select "x64". Select "OK". Close Configuration Manager.
	Still in Property Pages, under "Configuration:" select "All Configurations".
	Still in Property Pages, go to Configuration Properties|Linker|System. Set the "Stack Reserve Size" to 100,000,000 (i.e. 100 million). Select "OK" to close Property Pages

3. In Project|Add Existing Item... , add the following to the project:
	- all the .f90 files from the code directory
	- all the .f90 files from the visualisation directory
	- the shegraph2.3.lib from the visualisation directory. NB "All Files (*.*)"" must be searchable

4. Copy the full path for the "incl_mod_shegraph" directory (e.g. C:\Users\sjbir_000\Documents\sv4.4.5x64\sv4\incl_mod_shegraph), and enter in the following two places:

	Project|Properties|Configuration Properties|Linker|General|Additional Library Directories
	Project|Properties|Configuration Properties|Fortran|General|Additional Include Directories

5. In 	Project|Properties|Configuration Properties|Fortran|Libraries change runtime Library to "Multithreaded"

6. Build the project using Build|Build (either the debug or the release versions)