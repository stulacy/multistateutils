# multistateutils 1.2.4

Bug fixes. Tests for object class now use `inherits` and clock is now `double` to allow for greater range of values.

# multistateutils 1.2.3

  - Added class `msdata` to the output from `msprep2` so that it is compatible with other `mstate` functions, such as `events`. 
  
# multistateutils 1.2.2

  - Fix for changes in data.table 1.12.1

# multistateutils 1.2.1

  - Added more examples
  - Cleaned up build process
  - Added GPL-3 licence and contributing information
  
# multistateutils 1.2.0

  - Added `cohort_simulation` function to run discrete event simulation over populations
  - Added `msprep2` that does the same role as `mstate::msprep` but takes a tidy data of state entry times as input
  - Added individual age limits for `cohort_simulation`, `predict_transitions` and `length_of_stay`. See vignette for futher details

# multistateutils 1.1.0

  - First CRAN release



