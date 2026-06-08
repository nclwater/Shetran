# Compiling SHETRAN

## Compiler Support

The following cases have been tested:

- Windows + Visual Studio 2022 + ifx 2026
- Windows + CMake + ifx 2026
- Linux + CMake + ifx 2026
- Linux + CMake + gfortran 16.1

With any other platform and compiler your MMV.

## Windows

The easiest way to build SHETRAN on Windows is by using the provided `build.bat` script, which automates the CMake configuration and NMake build process.

### CMake

#### Prerequisites

* **CMake** (version 3.20 or higher) added to your system PATH. Get it from the official [webpage](https://cmake.org/download/).
* **Intel oneAPI HPC Toolkit** installed (specifically providing the `ifx` Fortran compiler). Currently you can get it from [here](https://www.intel.com/content/www/us/en/developer/tools/oneapi/oneapi-toolkit-download.html).
* **Python tooling with `fypp` available on PATH**. `fypp` is required by the Fortran stdlib build.
* The **HDF5 1.14.6** source tarball (`hdf5-1.14.6.tar.gz`) placed in the `external/tarballs/` directory. If the repository is cloned, this file already exists.
* The **Fortran stdlib 0.8.1** source tarball (`stdlib-0.8.1.tar.gz`) placed in the `external/tarballs/` directory. If the repository is cloned, this file already exists.

#### Python Tooling Environment

The recommended setup is a small conda environment named `shetran`:

```cmd
conda create -n shetran python=3.12 -y
conda activate shetran
python -m pip install --upgrade pip
python -m pip install fypp ford pandas h5py numpy matplotlib
```

`fypp` is required for compiling Fortran stdlib. `ford` is only required when generating documentation, and `pandas`, `h5py`, and `numpy` are used by the integration-test scripts.

If you do not use conda, use a normal Python virtual environment instead:

```cmd
py -m venv .venv
.venv\Scripts\activate
python -m pip install --upgrade pip
python -m pip install fypp ford pandas h5py numpy matplotlib
```

Before building, confirm the tools are visible from the same shell:

```cmd
where fypp
where ford
```

#### Using `build.bat`

Open a command prompt (preferably an Intel oneAPI command prompt, though the script attempts to auto-detect and initialize the environment if installed in the default `C:\Program Files...` locations) and run:

```cmd
build.bat
```

This will configure and build a `Release` version of SHETRAN. The resulting executable will be located at `build\release\bin\shetran.exe`.

**Available Options:**

* `-t, --type TYPE` : Build type: `Debug`, `Release`, or `ReleaseNative` (default: `Release`).
* `--clean`         : Clean the entire build directory before building (rebuilds external libraries too).
* `--clean-app`     : Clean only SHETRAN build artifacts before building (keeps external libraries like HDF5).
* `-v, --verbose`   : Enable verbose build output.
* `--ford`          : Generate FORD documentation after a successful build.
* `--docs-only`     : Generate FORD documentation only (no compile).
* `-h, --help`      : Show help message.

**Examples:**

```cmd
build.bat -t Debug             :: Build Debug version
build.bat --clean              :: Clean and build Release version
build.bat -t Release --clean-app :: Rebuild SHETRAN only, keep external libs
build.bat -t Release -v        :: Verbose Release build
build.bat -t ReleaseNative     :: Max local optimization (may be non-portable)
build.bat --ford               :: Build and generate FORD docs
build.bat --docs-only          :: Generate FORD docs without compiling
```

`--clean` and `--clean-app` are mutually exclusive.

#### Manual CMake Build (NMake)

If you prefer to run CMake manually:

1. Open the "Intel oneAPI command prompt" (to ensure `ifx` and `nmake` are in your PATH).
2. Create a build directory and configure the project:

   ```cmd
   mkdir build\release
   cd build\release
   cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=ifx ..\..
   ```

3. Build the project:

   ```cmd
   nmake
   ```

**CMake Options:**

* `STATIC_RUNTIME` (Default: `ON`): Links Intel and MSVC runtimes statically (`/MT` and `/libs:static`). This generates a portable `.exe` that does not require Intel oneAPI to be in the PATH of the target machine. Set to `OFF` for dynamic linking.
* `ENABLE_QUICKWIN` (Default: `ON` on Windows with Intel Fortran): Builds the Windows/ifx executable with the interactive file picker used by no-argument and `-a` runs. This option is ignored on non-Windows or non-Intel builds.
* `Release` build intent: Uses `/O2` and `/fp:precise` for stable, safer numerics and good portability.
* `ReleaseNative` build intent: Uses `/O3` and `/QxHost` for maximum performance on the build machine CPU. This may reduce portability to older or different CPUs.
* `ENABLE_DEPENDENCY_ANALYSIS` (Default: `ON`): Must remain `ON` for proper automatic Fortran module dependency sorting.
* `ENABLE_FORD_DOCS` (Default: `OFF`): Adds a `ford_docs` build target. This target can be built independently and does not build `shetran.exe`.

### FORD Documentation

Activate the Python environment containing `ford` first:

```cmd
conda activate shetran
```

You can generate FORD docs directly from the build script:

```cmd
build.bat --docs-only
```

or after a successful compile:

```cmd
build.bat -t Release --ford
```

If you prefer CMake targets, enable the optional FORD target during configure:

```cmd
cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=ifx -DENABLE_FORD_DOCS=ON ..\..
nmake ford_docs
```

The generated entry point is `docs\ford\index.html`.

### Visual Studio

Because SHETRAN requires automatic Fortran module dependency sorting and builds HDF5 from source, manually configuring a Visual Studio project is highly discouraged. Instead, you should let CMake generate the Visual Studio Solution (`.sln`) for you.

#### Generating the Solution

1. Open a command prompt with CMake in your PATH.
2. Create a specific directory for your Visual Studio build:

   ```cmd
   mkdir build\vs
   cd build\vs
   ```

3. Generate the solution file. You must instruct Visual Studio to use the modern Intel LLVM compiler (`ifx`) by passing the `-T fortran=ifx` toolset argument:

   ```cmd
   cmake -G "Visual Studio 17 2022" -T "fortran=ifx" ..\..
   ```

   *(If you are using Visual Studio 2019, use `-G "Visual Studio 16 2019"` instead).*
4. Open the generated `SHETRAN.sln` file in Visual Studio.
5. In the **Solution Explorer**, right-click the **SHETRAN** project and select **Set as Startup Project**.
6. Select your desired configuration (e.g., `Release` or `Debug`) from the top toolbar and build the solution (**Build > Build Solution** or `Ctrl+Shift+B`).

#### Behind the Scenes: CMake to Visual Studio Mappings

If you inspect the Visual Studio property pages of the generated project, you will notice CMake has automatically applied the following essential settings:

* **Fortran > Libraries**: Static Intel runtime linking is enabled when `STATIC_RUNTIME=ON` (via `/libs:static`).
* **C/C++ > Code Generation > Runtime Library**: With `STATIC_RUNTIME=ON`, set to **Multi-threaded** (`/MT` for Release, `/MTd` for Debug).
  *(These static runtime linkage settings ensure the resulting `.exe` can be distributed and run on machines without Intel oneAPI installed).*
* **Linker & Includes**: Automatically pointed to the `hdf5-install` directory generated by the external build step.

#### Legacy Manual Visual Studio Project

If you cannot use CMake-generated Visual Studio solutions, the older manual project setup is below. This path is not recommended: you must build HDF5 and Fortran stdlib yourself, keep the include and library paths consistent for every configuration, and keep Visual Studio's source-file ordering compatible with Fortran module dependencies.

1. Start Visual Studio 2022 and select **Continue without code**.
2. Create a Visual Fortran empty console application: **File > New > Project**, search for Fortran, and select **Empty Project**.
3. Use these project settings:

   ```text
   Project name = SHETRAN
   Location     = location of the SHETRAN folder containing the src code folder
   Solution name = SHETRAN
   ```

   Select **Place solution and project in the same directory**, then select **Create**.
4. Open **Project > Properties > Configuration Manager...**. Under **Platform**, select **<New...>**, choose `x64`, then close Configuration Manager. In **Configuration**, select **All Configurations** before changing the properties below.
5. Build the external libraries manually before adding their include and library directories to Visual Studio. The examples below use command-line CMake and install into `external\manual-install`.

   HDF5 1.14.6:

   ```cmd
   mkdir external\src
   tar -xf external\tarballs\hdf5-1.14.6.tar.gz -C external\src

   cmake -S external\src\hdf5-1.14.6 -B build\manual-hdf5 ^
     -G "Visual Studio 17 2022" -A x64 -T "fortran=ifx" ^
     -DCMAKE_INSTALL_PREFIX=%CD%\external\manual-install\hdf5 ^
     -DHDF5_BUILD_FORTRAN=ON ^
     -DHDF5_BUILD_HL_LIB=ON ^
     -DHDF5_BUILD_EXAMPLES=OFF ^
     -DHDF5_BUILD_TESTING=OFF ^
     -DHDF5_BUILD_TOOLS=OFF ^
     -DHDF5_ENABLE_SHARED_LIB=OFF ^
     -DHDF5_ENABLE_STATIC_LIB=ON ^
     -DHDF5_ENABLE_Z_LIB_SUPPORT=OFF ^
     -DHDF5_ENABLE_SZIP_SUPPORT=OFF

   cmake --build build\manual-hdf5 --config Release --target INSTALL
   ```

   Fortran stdlib 0.8.1. Make sure the Python environment with `fypp` is active first:

   ```cmd
   conda activate shetran

   mkdir external\src
   tar -xf external\tarballs\stdlib-0.8.1.tar.gz -C external\src

   cmake -S external\src\stdlib-0.8.1 -B build\manual-stdlib ^
     -G "Visual Studio 17 2022" -A x64 -T "fortran=ifx" ^
     -DCMAKE_INSTALL_PREFIX=%CD%\external\manual-install\stdlib ^
     -DBUILD_SHARED_LIBS=OFF ^
     -DBUILD_TESTING=OFF ^
     -DFIND_BLAS=OFF ^
     -DSTDLIB_ANSI=OFF ^
     -DSTDLIB_BITSETS=OFF ^
     -DSTDLIB_HASHMAPS=OFF ^
     -DSTDLIB_IO=OFF ^
     -DSTDLIB_LINALG_ITERATIVE=OFF ^
     -DSTDLIB_LOGGER=OFF ^
     -DSTDLIB_QUADRATURE=OFF ^
     -DSTDLIB_SPECIALMATRICES=OFF ^
     -DSTDLIB_STRINGLIST=OFF ^
     -DSTDLIB_STATS=OFF ^
     -DSTDLIB_SYSTEM=ON

   cmake --build build\manual-stdlib --config Release --target INSTALL
   ```

   For Debug builds, repeat the build commands with `--config Debug` and keep Debug and Release library paths separate if the generated library names differ.

6. Use **Project > Add Existing Item...** to add:

   * `Shetran.f90` from the `src` folder.
   * All `.f90` files from `src/modules`.
   * All `.f90` files from `src/parameters`.
   * `mod_load_filedata.f90` from `src/util`.
   * All `.f90` files from `src/visualisation`, except `include_extend_s.f90` and `include_increment.f90`, which must be present but not included in the project.
   * The static HDF5 libraries from `external\manual-install\hdf5\lib`.
   * The static Fortran stdlib libraries from `external\manual-install\stdlib\lib`, including the `fortran_stdlib_system` library used by SHETRAN.
   * `src/resource/resource1.rc`, added under **Resource files**.

7. Add include directories under **Project > SHETRAN Properties > Configuration Properties > Fortran > General > Additional Include Directories**.
   Use **All Configurations** so this applies to both Debug and Release.

   Add at least:

   ```text
   external\manual-install\hdf5\include
   external\manual-install\stdlib\include
   src
   ```

   Depending on the stdlib install layout, the module files may be under a compiler-specific subdirectory below `external\manual-install\stdlib\include`; add that directory if Visual Studio cannot find `stdlib_system.mod`.

8. Add library directories under **Linker > General > Additional Library Directories**:

   ```text
   external\manual-install\hdf5\lib
   external\manual-install\stdlib\lib
   ```

9. Add the HDF5 and stdlib library names under **Linker > Input > Additional Dependencies**. The exact Debug suffixes can differ by HDF5 configuration; match the files in the install directory. For a Release build, expect names similar to:

   ```text
   libhdf5.lib
   libhdf5_f90cstub.lib
   libhdf5_fortran.lib
   libhdf5_hl.lib
   libhdf5_hl_f90cstub.lib
   libhdf5_hl_fortran.lib
   fortran_stdlib_system.lib
   shlwapi.lib
   user32.lib
   ```

10. Set **Fortran > General > Preprocess Source File** to **Yes** (`/fpp`).
11. Set **Fortran > Optimization > Heap Arrays** to `0`.
12. Set **Fortran > Libraries > Runtime Library** to **Multithreaded**.
13. Set **Fortran > Optimization > Optimization** to **Maximum Speed** (`/O2`) for Release.
14. Set **Fortran > Floating Point > Floating Point Model** to `fp:precise`.
15. If you want the Windows file-picker path in a manual Visual Studio project, define `SHETRAN_HAVE_QUICKWIN`, `SHETRAN_WINDOWS`, `SHETRAN_INTEL_FORTRAN`, and `SHETRAN_HAVE_STDLIB_SYSTEM` under **Fortran > Preprocessor > Preprocessor Definitions**, and add `/libs:qwin` under **Fortran > Command Line > Additional Options** or the Intel Fortran library options.
16. Build the project with **Build > Build Solution**.

## Linux

### Current Compiler Status

SHETRAN currently compiles and runs cleanly with both `ifx` and `gfortran` under Linux.

### Prerequisites

* **CMake** (version 3.20 or higher) available on PATH.
* **Python tooling with `fypp` available on PATH**. `fypp` is required by the Fortran stdlib build.
* A supported Fortran compiler:
   * **Intel oneAPI HPC Toolkit** (`ifx`, recommended and most reliable).
   * **gfortran** (supported, but currently considered experimental in this project).
* Standard build tools (for example GNU Make) installed.
* The **HDF5 1.14.6** source tarball (`hdf5-1.14.6.tar.gz`) placed in `external/tarballs/`.
* The **Fortran stdlib 0.8.1** source tarball (`stdlib-0.8.1.tar.gz`) placed in `external/tarballs/`.

### Using `build.sh` (recommended)

Activate the Python environment containing `fypp` first:

```bash
conda activate shetran
```

From the repository root:

```bash
./build.sh -c ifx
```

This configures and builds a `Release` build in `build/release` and places the executable at `build/release/bin/shetran`.

Important options:

* `-t, --type TYPE`: `Debug`, `Release`, or `ReleaseNative` (default: `Release`).
* `-c, --compiler COMPILER`: `ifx` or `gfortran` (use `ifx` for reliable builds at the moment).
* `--clean`: remove the whole build folder before building.
* `--clean-app`: rebuild SHETRAN only, keep external libraries.
* `-j, --jobs N`: parallel build jobs.
* `-v, --verbose`: verbose build output.
* `--ford`: generate FORD docs after a successful build.
* `--docs-only`: generate FORD docs only.

Examples:

```bash
./build.sh -c ifx
./build.sh -c ifx -t Debug
./build.sh -c ifx -t ReleaseNative --clean
./build.sh -c ifx --clean-app
```

### Manual CMake Build (Linux)

If you prefer to run CMake directly:

```bash
mkdir -p build/release
cd build/release
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=ifx ../..
cmake --build . --target SHETRAN --parallel
```

Notes:

* On Linux, if `-DCMAKE_Fortran_COMPILER` is not provided, CMake will try `ifx` first and then fall back to `gfortran`.
* The build pulls HDF5 from `external/tarballs/hdf5-1.14.6.tar.gz` and stages it in the project build tree under `build/<type>/hdf5-install`.
* `ReleaseNative` is intended for maximum local optimization and may reduce portability across different CPUs.
* Keep `ENABLE_DEPENDENCY_ANALYSIS=ON` for correct Fortran module dependency sorting.
* Platforms other than Windows and Linux are currently unsupported by this CMake configuration.

## Testing

### Integration Testing

To ensure that changes to the codebase do not inadvertently alter the simulation results, an integration testing script is provided. The script compares the output of your newly compiled SHETRAN executable against known "good" results from previous versions.

#### Python Prerequisites

The testing script is written in Python and requires a few external libraries. Ensure you have Python installed, then install the required dependencies:

```cmd
pip install pandas h5py numpy
```

#### Running the Tests

The testing script is located in the `examples` directory. 
By default, it expects the `Release` build of `shetran.exe` (Windows) or `shetran` (Linux) to be present in `..\build\release\bin\`.
This is automatically platform aware.

1. Open a command prompt and navigate to the `examples` directory:

   ```cmd
   cd examples
   python setup_results_check.py --use-release-exe
   ```

2. Run the test script once you've done changes to the code:

   ```cmd
   python check_results_consistency.py
   ```

   This creates an overall as well as per-model overview csv files.
   Additionally, any differences found will result in them being displayed in the `diff_delta/` subdirectory for each model.

#### How It Works

* **Test Execution**: The script loops through the subdirectories in the `examples` folder (e.g., `Cobres`, `Dunsop`, `Slapton`). For each model, it copies the configuration from `model/` into a temporary `test_compute` directory and runs the SHETRAN executable.
* **Comparison**: Once the simulation completes, it compares the newly generated output files against the reference files stored in the `output_should` directory.
* **Diff Analysis**:
  * Text and CSV files are compared using a tolerant line-by-line diff.
  * HDF5 binary files (`.h5`, `.hdf5`) are compared structure-by-structure and dataset-by-dataset with a predefined numeric tolerance.
* **Results**: If differences are found, the script generates diff reports inside a `diff_delta` directory within that specific example's folder. Finally, it generates a summary report named `results_consistency_check_overview.csv` in the `examples` directory.

*(Note: By default, the script skips very large/long-running models like `dano100m` to save time. To run all examples, you would need to modify the script to pass `-l long` or `-l all` to the `check_results_consistency()` call.)*
