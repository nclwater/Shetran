SHETRAN version 4.4.5 05/05/16 - needs checking that this is correct
******************************

COMPILING
Do the following to compile in Intel parallel studio XE 2015:

1. Create a visual fortran empty console application

2. save everything file|save all. 

3. In project|properties got the  configuration Manager (at the top of the page)

    In Project contexts box. Goto Platform. Select New and then x64. Close this.
    Still in Property Pages under configuration select All configurations

4. In project|properties|linker|system set the stack reserve size to 100000000 (i.e. 100 million) 

5. In project|add existing items add the follwing to the project:
	- all the ..f90 files from the code directory
	- shegraph2.3.lib from the visualisation directory
	- all the .f90 files from the visualisation directory

6. Enter full path for "incl_mod_shegraph" directory in the following two places (e.g. C:\Users\sjbir_000\Documents\sv4.4.5x64\sv4\incl_mod_shegraph)

        project | properties | fortran | general |additional include direcories
        project | properties | linker  | general |additional library direcories

7. build the project build|build (either the debug or the release versions)

