# Compiling SHETRAN

## Windows update 23042026
Intel Fortran Compiler with Visual Studio. Intel ifx compiler 2025 and VS2022


## Windows

The easiest way to build SHETRAN on Windows is by using the provided `build.bat` script, which automates the CMake configuration and NMake build process.

### CMake

#### Prerequisites

* **CMake** (version 3.20 or higher) added to your system PATH.
* **Intel oneAPI HPC Toolkit** installed (specifically providing the `ifx` Fortran compiler).
* The **HDF5 1.14.6** source tarball (`hdf5-1.14.6.tar.gz`) placed in the `external/tarballs/` directory. If the repository is cloned, this file already exists.

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
* `Release` build intent: Uses `/O2` and `/fp:precise` for stable, safer numerics and good portability.
* `ReleaseNative` build intent: Uses `/O3` and `/QxHost` for maximum performance on the build machine CPU. This may reduce portability to older or different CPUs.
* `ENABLE_DEPENDENCY_ANALYSIS` (Default: `ON`): Must remain `ON` for proper automatic Fortran module dependency sorting.
* `ENABLE_FORD_DOCS` (Default: `OFF`): Adds a `ford_docs` build target. This target can be built independently and does not build `shetran.exe`.

### FORD Documentation

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

## Linux

### Current Compiler Status

SHETRAN currently only cleanly compiles on Linux with Intel `ifx`.
`gfortran` support is in progress and should currently be treated as experimental.

### Prerequisites

* **CMake** (version 3.20 or higher) available on PATH.
* A supported Fortran compiler:
   * **Intel oneAPI HPC Toolkit** (`ifx`, recommended and most reliable).
   * **gfortran** (supported, but currently considered experimental in this project).
* Standard build tools (for example GNU Make) installed.
* The **HDF5 1.14.6** source tarball (`hdf5-1.14.6.tar.gz`) placed in `external/tarballs/`.

### Using `build.sh` (recommended)

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
