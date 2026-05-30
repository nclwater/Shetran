# Code compilation

## Windows update 29052026
Intel Fortran Compiler with Visual Studio. Intel ifx compiler 2025 and Visual Studio 2022


## Windows

1. Start Visual Studio 2022. Select "Continue without code".

```text
    Project name    = Shetran
    Location        = location of the Shetran folder containing the src code folder
    Solution name   = Shetran

    Click "place a solution and project in the same directory"

    Select "Create"
```

2. In Project|Properties, select "Configuration Manager..." (at the top of the box).

   - In Project contexts box, go to Platform, select "<New...>. Then under "New platform", select "x64".
   - Select "OK".
   - Close Configuration Manager.
   - Still in Property Pages, under "Configuration:" select "All Configurations".
   - Still in Property Pages, go to Configuration Properties|Fortran|Optimisation. Set the "Heap Array" to 0.
   - Select "OK" to close Property Pages.

3. In Project|Add Existing Item... , add the following to the project:

   - Shetran.f90 files from the src folder
   - all the .f90 files from the src/modules folder
   - all the .f90 files from the src/parameters folder
   - mod_load_filedata.f90 from the src/util folder
   - all the .f90 files from the src/visualisation forlder (except include_extend_s.f90 and include_increment.f90 which must be present but not in the project)
   - all the .lib from the external/library-files folder. NB "All Files (*.*)"" must be searchable
   - in the Solution explorer (on the right), click on Resource files, then add in Project|Add Exisiting Item select resource1.rc from the src/resource folder

4. Copy the full path for the "include" directory (e.g. C:\Users\sjbir_000\Documents\shetran\external\Include), and enter in the following place:

```text
    Project|Properties|Configuration Properties|Fortran|General|Additional Include Directories
```

5. In Project|Properties|Configuration Properties|Fortran|Libraries change runtime Library to "Multithreaded"

6. In Project|Properties|Configuration Properties|Fortran|Optimization change Optimization to "Maximum Speed (/O2)"

7. In Project|Properties|Configuration Properties|Fortran|Floating Point change Floating Point Model to "fp:prcise"

8. Build the project using Build|Build Solution(either the debug or the release versions)

Before you can run the compiled Shetran executable, you need to copy the hdf5 dlls into the directory of the executable.
Take them from any of the Shetran release zip files.
