# Crash Analysis: Access Violation (157) in `ifx` Debug Build

## 1. Context & Symptoms Summary
*   **Compiler/System:** `ifx` (LLVM-based Intel compiler) via CMake on Windows.
*   **Dependency:** HDF5 v1.14.6 built from source.
*   **Error:** `forrtl: severe (157): Program Exception - access violation`
*   **Traceback:** All routines and lines are `Unknown`.
*   **Execution State:** Fails shortly after start. Output files are created but remain empty.
*   **Baseline:** Legacy build (`ifort` + precompiled HDF5 1.12.0 + VS2019) works flawlessly.

---

## 2. Why is the Traceback "Unknown"?
In Windows environments using Intel compilers, an "Unknown" stack trace in a Debug build typically points to one of two issues:

1.  **Missing Debug Information (PDB):** While the Fortran compiler might have been passed `/Zi` and `/traceback`, the *linker* also needs to be told to generate the Program Database (.pdb) file. If CMake is missing `/debug` in `CMAKE_EXE_LINKER_FLAGS_DEBUG`, the executable has no symbols to resolve memory addresses to source lines.
2.  **Catastrophic Stack Corruption:** If the application triggers a massive stack overflow or wildly invalid pointer write, the call stack frame pointers can be completely overwritten, making it impossible for the Fortran runtime (`forrtl`) to unwind the stack, even if the `.pdb` file is present.

---

## 3. Core Hypotheses for the Crash

### Hypothesis A: Stack Overflow due to Missing `/heap-arrays0`
*Probability: Very High*
Legacy Fortran codes often define massive local arrays. By default, Fortran attempts to place these on the stack. The old VS2019 instructions (`COMPILING.md`) explicitly say to set **"Heap Array to 0"** (`/heap-arrays0`). This forces the compiler to allocate these large temporary/local arrays on the heap instead of the stack.
While your CMake plan mentions increasing the linker stack size (`/STACK:500000000`), a 500MB stack on Windows can still be problematic or insufficient depending on the array boundaries. Exceeding the stack triggers a guard page violation, which manifests exactly as an "Access Violation".

### Hypothesis B: C/C++ Runtime Library Mismatch (CRT)
*Probability: High*
On Windows, memory allocated by one CRT cannot be freed or managed by another. 
*   `COMPILING.md` specifies changing the Runtime Library to **Multithreaded** (`/MT`).
*   CMake's default for Windows is often **Multithreaded DLL** (`/MD`).
If the Fortran code is compiled with `/MD` but the HDF5 static library was built with `/MT` (or vice versa), the moment you pass a file handle or buffer across the HDF5 boundary, you will get an access violation. The fact that the output files are *created but empty* suggests the crash happens exactly at the boundary of the first major HDF5 write/flush operation.

### Hypothesis C: HDF5 1.12 vs 1.14 API/ABI Changes
*Probability: Medium*
You moved from a precompiled HDF5 (likely v1.12.0 based on the headers) to v1.14.6. Between 1.12 and 1.14, HDF5 has introduced API changes. If the Fortran interfaces (which are notoriously sensitive to `KIND` sizes like `hid_t` and `hsize_t`) mismatched between what `Shetran` expects and what HDF5 1.14 provides, the underlying C library will try to read a 64-bit integer where a 32-bit integer was passed (or vice versa), leading to a segmentation fault.

### Hypothesis D: `ifort` vs `ifx` strictness
*Probability: Low to Medium*
`ifx` is LLVM-based and generally stricter than the classic `ifort`. It may place variables differently in memory or initialize them differently. If the code relies on implicit initialization of a pointer or array bounds, `ifx` might expose an underlying latent bug.

---

## 4. Piecewise Debugging Plan (Next Steps)

To isolate the exact cause, execute the following steps one by one:

### Step 1: Fix the Debug Symbols
Before blindly guessing, we need the compiler to tell us *exactly* where it crashes.
*   **Action:** Update your `CMakeLists.txt` to ensure the linker generates debug info.
    ```cmake
    # Add to your CMakeLists.txt
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /debug")
    ```
*   **Verification:** Check that `shetran.exe` and `shetran.pdb` are in the same `bin/` directory. Run the code again. You should now get file names and line numbers in the traceback.

### Step 2: Apply the Heap Arrays Flag
If it's a stack overflow, we can bypass it by aligning the `ifx` build with the old `ifort` build.
*   **Action:** Add `/heap-arrays0` to your Fortran compiler flags.
    ```cmake
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /heap-arrays0")
    ```
*   **Verification:** Recompile and run. If the crash disappears, the issue was a stack overflow caused by massive local arrays.

### Step 3: Align the MSVC Runtime Libraries
Ensure HDF5 and Shetran are using the same Windows Runtime.
*   **Action:** Enforce a consistent CRT policy in your `CMakeLists.txt`. CMake 3.15+ supports this natively:
    ```cmake
    # Put this before project() or early in the CMakeLists.txt
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
    ```
    Ensure that the `ExternalProject_Add` for HDF5 also inherits this setting, or is explicitly configured to build with static CRTs (`-DCMAKE_MSVC_RUNTIME_LIBRARY=MultiThreaded`).

### Step 4: Run under the Visual Studio Debugger
If the traceback is still "Unknown" (due to stack corruption), we need an external observer.
*   **Action:** 
    1. Open an "x64 Native Tools Command Prompt".
    2. Run: `devenv /debugexe C:\Users\tolstoi\Documents\Code\shetran_rework\build\debug\bin\shetran.exe -f rundata_cob.txt`
    3. When Visual Studio opens, press `F5` to start. 
    4. When the Access Violation hits, VS will pause exactly on the line of code that triggered it, and you can inspect the Call Stack and variable values.

### Step 5: Isolate HDF5 Version
If the code crashes inside an HDF5 routine:
*   **Action:** Temporarily change the `ExternalProject_Add` in CMake to download and build HDF5 `1.12.0` instead of `1.14.6`. If 1.12.0 works but 1.14.6 crashes, we know code modifications are required in the SHETRAN source to comply with the 1.14 API.