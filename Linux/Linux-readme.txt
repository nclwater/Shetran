Shetran Linux
*************
Instructions for Linux mint


Source Code
***********
As for PC expect getdirqq.f90. SHETRAN in Linux much be run from the console

Install gnu compilers
*********************
sudo apt install gofrtran
sudo apt install g++

Install Intel compilers
***********************
download linux version e.g. l_BaseKit_p_2021.4.0.3422_offline.sh and l_HPCKit_p_2021.4.0.3347_offline.sh
Install I installed to /opt/intel/oneapi (so needed sudo sh ./l_BaseKit_p_2021.4.0.3422_offline.sh and sudo sh ./l_HPCKit_p_2021.4.0.3347_offline.sh)
I needed to add the following lines to the .bash_login file
source /opt/intel/bin/ifortvars.sh intel64
source /opt/intel/bin/compilervars.sh intel64


Build hdf libraries and include files using Intel compiler
**********************************************************
download hdf files (e.g. hdf5-1.12.0.tar.gz)
uncompress
then change into directory (cd hdf-1.12.0) then compile using the article below. (compiling seems to be necessary for the Intel compiler as the binaries that can be downloaded are for the Gfortran compiler)
https://software.intel.com/content/www/us/en/develop/articles/performance-tools-for-software-developers-building-hdf5-with-intel-compilers.html

I needed the full pathnames
e.g
export CC=/opt/intel/bin/icc

I installed it locally in /home/steve/hdf5-1.12.0

in this compiled version are the .h and .mod files in the "include" directory and the .a files in the "lib" directory

Compiling Shetran
*****************

I tried to use Eclipse but the Makefile did not seem to work and I could not link the library files

I needed to change the following files:
getdirqq.f90  - remove IFWIN and IFQWIN and only have the -c and -f options for running Shetran (no pop up window)
visulisation_metadata.f90 - put the include_increment.f90 in 4 subroutines

For simplicity I have put all the source code in  a single directory

Goto the Shetran folder then the Release folder. run shetran-compile.bat (e.g. ./shetran-compile.bat) - see below. The compiles all the source code and proudces .o and .mod files in the Release folder. 
Visualisation_hdf5.f90 needs the link to the hdf include folder. Then it links everything using the hdf library files and produces the Shetran executable

When I have time I will create Makefile

I have set large heap arrays (the other option is on the command line to run "ulimit -s unlimted" before running Shetran) and also set -mcmodel=large

Running Shetran
***************

To run shetran you need the -f option (e.g. ./Shetran-linux -f rundata_Foston_Beck_at_Foston_Mill.txt)

Viewing HDF files
*****************

Download HDFView (e.g. HDFView-3.1.1-centros7_64.tar.gz) and uncomress. Follow the instructions in the Readme to install and run. Select the relevant .h5 file (e.g. output_Foston_Beck_at_Foston_Mill_shegrahp.h5)


shetran-compile.bat  (run from Release folder)
*******************
ifort -c -O1  ../src/is_cc.f90
ifort -c -O1  ../src/CONST_SY.F90
ifort -c -O1  ../src/visualisation_extras.f90
ifort -c -O1  ../src/visualisation_structure.f90
ifort -c -O1  ../src/colm_c1.f90
ifort -c -O1  ../src/visualisation_pass.f90
ifort -c -O1  ../src/visualisation_read.f90
ifort -c -O1  ../src/sglobal.f90
ifort -c -O1  ../src/mod_parameters.f90
ifort -c -O1  ../src/AL_G.F90
ifort -c -O1  ../src/mod_load_filedata.f90
ifort -c -O1  ../src/link_cc.f90
ifort -c -O1  ../src/CONT_CC.F90
ifort -c -O1  ../src/colm_cg.f90
ifort -c -O1  ../src/link_cc1.f90
ifort -c -O1  ../src/getdirqq.f90
ifort -c -O1  ../src/SED_CS.F90
ifort -c -O1  ../src/colm_co.f90
ifort -c -O1  ../src/colm_c2.f90
ifort -c -O1  ../src/colm_cc1.f90
ifort -c -O1  ../src/colm_cc.f90
ifort -c -O1  ../src/visualisation_metadata.f90
ifort -c -O1  ../src/plant_cc.f90
ifort -c -O1  ../src/AL_C.F90
ifort -c -O1  ../src/visualisation_map.f90
ifort -c -O1  ../src/bk_cw.f90
ifort -c -O1  ../src/sed_co.f90
ifort -c -O1  ../src/AL_D.f90
ifort -c -O1  ../src/link_cw.f90
ifort -c -O1  ../src/ZQmod.f90
ifort -c -O1  ../src/visualisation_hdf5.f90 -I/home/steve/hdf5-1.12.0/include
ifort -c -O1  ../src/SMmod.f90
ifort -c -O1  ../src/utilsmod.f90
ifort -c -O1  ../src/OCmod2.f90
ifort -c -O1  ../src/MNmod.f90
ifort -c -O1  ../src/CMmod.f90
ifort -c -O1  ../src/OCQDQMOD.F90
ifort -c -O1  ../src/SYmod.f90
ifort -c -O1  ../src/ETmod.f90
ifort -c -O1  ../src/visualisation_interface_far_right.f90
ifort -c -O1  ../src/visualisation_interface_left.f90
ifort -c -O1  ../src/visualisation_interface_centre.f90
ifort -c -O1  ../src/VSmod.f90
ifort -c -O1  ../src/OCmod.f90
ifort -c -O1  ../src/FRmod.f90
ifort -c -O1  ../src/visualisation_interface_right.f90
ifort -c -O1  ../src/rest.f90
ifort -c -O1  ../src/run_sim.f90
ifort -c -O1  ../src/Shetran.f90

ifort is_cc.o CONST_SY.o visualisation_extras.o visualisation_structure.o colm_c1.o visualisation_pass.o visualisation_read.o sglobal.o mod_parameters.o AL_G.o mod_load_filedata.o link_cc.o CONT_CC.o colm_cg.o link_cc1.o getdirqq.o SED_CS.o colm_co.o colm_c2.o colm_cc1.o colm_cc.o visualisation_metadata.o plant_cc.o AL_C.o visualisation_map.o bk_cw.o sed_co.o AL_D.o link_cw.o ZQmod.o visualisation_hdf5.o  SMmod.o utilsmod.o OCmod2.o MNmod.o CMmod.o OCQDQMOD.o SYmod.o ETmod.o visualisation_interface_far_right.o visualisation_interface_left.o visualisation_interface_centre.o VSmod.o OCmod.o FRmod.o visualisation_interface_right.o rest.o run_sim.o Shetran.o -L/home/steve/hdf5-1.12.0/lib /home/steve/hdf5-1.12.0/lib/libhdf5hl_fortran.a /home/steve/hdf5-1.12.0/lib/libhdf5_hl.a /home/steve/hdf5-1.12.0/lib/libhdf5_fortran.a /home/steve/hdf5-1.12.0/lib/libhdf5.a -o "SHETRAN-Linux"





libraries needed
****************

in Linux "ldd ./Shetran-Linux" for a list of libraries.

5 intel libraries: libifport.so.5, libifcoremet.so.5, libimf.so, libsvml.so, libintlc.so.5
5 gnu libraries: libm.so.6, libpthread.so.0,libc.so.6, libgcc_s.so.1,libdl.so.2

also linux-vdso.so.1, lb-linux-x86-64.so.2

