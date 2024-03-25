<!-- 
Maintainers: This issue template comes from https://github.com/getwilds/.github
You're encouraged to add your own that's optimized for your repo -->

# Contributing

This outlines how to propose a change to usethis. 
For more detailed info about contributing to this, and other WILDS projects, please see the
[**WILDS Contributing Guide**](https://getwilds.org/guide/). 

## Fixing typos/docs changes

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 

If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
reproducible example ([reprex](https://www.tidyverse.org/help/#reprex) for R, or [reprexpy](https://github.com/crew102/reprexpy) for Python).

### Pull request process

*   Fork the package and clone onto your computer
*   Create a Git branch for your pull request (PR)
*   Make your changes, commit to git, and then create a pull request.
    *   The title of your PR should briefly describe the change.
    *   The body of your PR should contain `Fixes #issue-number`.
*  For user-facing changes, add a bullet to the changelog file if there is one. This should be `NEWS.md` for an R package, and likely `Changelog.md` or `Changelog.rst` for a Python package.

### Code style

*   New code for R and Python packages should follow our [style guide](https://getwilds.org/guide/style.html). 

## Code of Conduct

Please note that this project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
