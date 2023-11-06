## Test Environments

* GitHub Actions
  - macOS-latest (R release)
  - windows-latest (R release)
  - ubuntu-latest (R devel, R release, R oldrel-1)
* Local Windows 10, R 4.2.0

## R CMD Check Results

The package has been tested on multiple platforms and no errors or warnings were identified during the tests.

## Reviewer Feedback

Thank you for the feedback, and we've addressed the issues mentioned:

1. **References**: We've added references to the methods used in our package in the DESCRIPTION file in the required format: authors (year) <doi:...>. Note that this is a novel method for the detection of seasonal epidemic onsets and that there are currently no references available.

2. **Commented Code in Examples**: We've removed commented code lines in examples and have made sure that the examples can be executed without issues.

## Abbreviations

* 'SoC' is an abbreviation of sum of cases. This is explained in detail in both the README.md, DESCRIPTION file, and in the relevant functions.

## Resubmission

We've addressed the feedback provided, and the package is now ready for resubmission. We appreciate your review and are looking forward to having our package included on CRAN.

Best regards,

Kasper Schou Telkamp