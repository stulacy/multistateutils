## Version update

I have fixed the bug arising from running conditional tests on the output of `class()` expecting a single value, which causes issues on r-devel.

## Test environments

* local Manjaro Linux, R 3.6.1
* win-builder (devel and release)

## R CMD check results

The note is: "Compilation used the following non-portable flag(s):
    ‘-march=x86-64’"
But I believe this to be a local issue and won't affect the install on other machines.

0 errors | 0 warnings | 1 note

## Reverse dependencies

There are no reverse dependencies.

