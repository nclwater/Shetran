# Shetran
Shetran Hydrological Model

Details can be found here:
https://research.ncl.ac.uk/shetran/

## Installation
Download the zip file from the latest release and then extract it.

## Sample Usage
Double click on exectuable, e.g. 'shetran.exe' in the extracted folder.
Then select a rundata file from one of the examples data sets.

## Compiling

A CMake script has been setup to automatically setup the necessary compiler flags and build Shetran as a project.
It is documented in the CMAKE_BUILD.md file.

**Key Features:**

- **Automatic compiler detection**: Prefers gfortran on Linux, Intel Fortran on Windows
- **Smart HDF5 handling**: Automatically finds system HDF5 or downloads and builds from source as needed
- **Cross-platform support**: Works on Linux, Unix, and Windows with multiple compilers

This has been tested in the following configurations and confirmed working:

- Linux, with GFortran, both with the build script and the cmake build.
- Windows, with ifx, the build script only.

Automatic integration of the cmake build in the Visual Studio and Visual Studio Code on Windows is currently very finicky.
An (slightly out-of-date) description of how to setup the corresponding Visual Studio project can be found in the file "COMPILING.md".

## Compiling Troubleshoot

Should the compile not work with the system HDF5 library, set the build script to build it locally.

## VS Code / IDE Integration

### VS Code Setup on Linux

For optimal VS Code integration with SHETRAN on Linux:

1. **Install Required Extensions:**
   - **CMake Tools** (by Microsoft) - Essential for CMake project management
   - **C/C++** (by Microsoft) - Provides IntelliSense and debugging support
   - **Modern Fortran** (by fortls-vscode) - Fortran language support

2. **Configure Build Presets:**
   - Open the project in VS Code
   - Press `Ctrl+Shift+P` and select **"CMake: Select Configure Preset"**
   - Choose from available presets:
     - `debug` - Debug build in `build/debug/`
     - `release` - Release build in `build/release/`
     - `gfortran` - Explicit GNU Fortran build
     - `linux-gfortran` - Linux-optimized GNU Fortran build

3. **Building:**
   - Use **"CMake: Build"** (`F7`) or click the Build button in the status bar
   - Build output appears in `build/<preset-name>/` directory

4. **Debugging Setup:**
   To use VS Code's integrated debugger with SHETRAN:

   a. **Create Launch Configuration:**
   Create or update `.vscode/launch.json`:
   ```json
   {
     "version": "0.2.0",
     "configurations": [
       {
         "name": "Debug SHETRAN",
         "type": "cppdbg",
         "request": "launch",
         "program": "${workspaceFolder}/build/debug/bin/shetran",
         "args": ["-f", "path/to/your/rundata.txt"],
         "stopAtEntry": false,
         "cwd": "${workspaceFolder}",
         "environment": [],
         "externalConsole": false,
         "MIMode": "gdb",
         "setupCommands": [
           {
             "description": "Enable pretty-printing for gdb",
             "text": "-enable-pretty-printing",
             "ignoreFailures": true
           }
         ],
         "preLaunchTask": "CMake: build"
       }
     ]
   }
   ```

   b. **Debug a Specific Case:**
   - Replace `"path/to/your/rundata.txt"` with your actual rundata file
   - Set breakpoints in Fortran source files (requires debug symbols)
   - Use `F5` to start debugging

5. **Tips:**
   - Use the **"CMake: Clean"** command to clean build directories
   - The status bar shows current build preset and target
   - Build problems appear in the Problems panel (`Ctrl+Shift+M`)

### Cross-Platform Notes

- **Windows**: Use the same extensions, but debugging may require additional configuration for Intel Fortran
- **HDF5 Integration**: No special VS Code setup needed - CMake handles dependencies automatically
- **Multiple Build Types**: Each preset creates its own directory for clean parallel builds

## Notes

- Currently the fallback function in the CMake build script is building _every_ file present in the source directory.
  Files which should not be build, should have a different file ending like .sav or similar.
