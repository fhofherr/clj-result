# Change Log

All notable changes to this project will be documented in this file.
This change log follows the conventions of
[keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

### Added

*   `attempt` and `attempt-v` macros. Attempt to perform the passed operations
     until are executed successfully or one returned an error.
*   `attempt-as->` and `attempt-v-as->` macros. Attempt to perform the passed
    operations until are executed successfully or one returned an error.
    The result value of the previous operation is bound to the passed
    symbol and passed on to the next.

[Unreleased]: https://github.com/fhofherr/clj-result/master
