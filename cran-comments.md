## Test Environments

* GitHub Actions
  - macOS-latest (R release)
  - windows-latest (R release)
  - ubuntu-latest (R devel, R release, R oldrel-1)
* Local Windows 10, R 4.2.0

## R CMD Check Results

The package has been tested on multiple platforms and no errors or warnings were identified during the tests.

## Reviewer Feedback

We have addressed the feedback from the previous review. Specifically:

1. **Removal of \dontrun{}**: We have removed the `\dontrun{}` wrappers in the examples as we confirmed that the examples run in less than 5 seconds. 

## Abbreviations

* 'SoC': An abbreviation for "Sum of Cases," which is thoroughly explained in the README.md, DESCRIPTION file, and relevant functions.
* 'Lipstich': The surname of one of the authors in the referenced paper.
* 'Obadia': The surname of one of the authors in the referenced paper.
* 'et al.': An abbreviation for "et alia" or "and others," which refers to multiple authors of the referenced paper.

## Resubmission

We've addressed the feedback provided, and the package is now ready for resubmission. We appreciate your review and are looking forward to having our package included on CRAN.

Best regards,

Kasper Schou Telkamp
