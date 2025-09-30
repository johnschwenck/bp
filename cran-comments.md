## CRAN Submission Comments

This is a new version (e.g., v2.1.1) of the 'bp' package.

### Changes from the Previous Version

* **CRAN Check Fixes:** The build process was updated to resolve local environment issues (Rtools, temporary files) and package dependency issues (e.g., 'png' package).
* **Code Maintenance:** Addressed internal code issues that caused the syntax error at `R/fitbit_API.R:822` (the file was removed and excluded from the build).
* **Package Data Files:** No changes were made to the data files in the `data/` directory.

### Regarding Package Size

We note that the package installed size is approximately **4.4 MB**. This size is necessary to include the essential sample data files required for the vignettes and examples to run accurately and demonstrate the package's primary functionalities (such as detailed visualizations and statistical reports).

We confirm that the installed size remains **below the 5 MB limit** as stated in the CRAN Policy, and these data files are required for testing and demonstration.

### Test Environments

* Local R installation, R 4.5.1 (x86\_64-w64-mingw32)
* **R-hub v2 Checks (via GitHub Actions):**
    * All default CRAN-critical platforms (R-devel Linux, Windows, macOS) passed cleanly. See results at: `https://github.com/johnschwenck/bp/actions`

* **Win-builder:** Passed R-devel on Windows.

### R CMD Check Results

**0 errors | 0 warnings | 0 notes**
