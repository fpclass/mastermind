# Mastermind

This repository contains the skeleton code for the "Mastermind" project, which comprises:

- Function stubs for all of the functions that need to be implemented as part of the project. The Mastermind program can be run with `stack run`. In order to complete the project, only `src/Game.hs` needs to be modified.
- A test suite for the project, which can be run with `stack test`. The source code for the test suite can be found in `test/Spec.hs`.
- A benchmark which can be run with `stack bench`. The source for the benchmarks can be found in `bench/Main.hs`.

## Notes on tests

The test suite comprises a combination of unit tests and property-based tests. Unit tests will always perform exactly the same test and test outcomes for them are deterministic. Property-based tests use randomised inputs and, therefore, test outcomes may be non-deterministic. The names of all property-based tests are pre-fixed with `prop_`, which will be shown in the test suite's standard output in the case of a failure. If successful, the test runner will report how many inputs were tried. Some tests may be skipped if the tests for functions they depend on do not yet pass.

## Notes on benchmarks

Running the benchmarks will output results to the standard output in textual form, as well as in visual form to `mastermind.html` which can be opened in your choice of web browser.
