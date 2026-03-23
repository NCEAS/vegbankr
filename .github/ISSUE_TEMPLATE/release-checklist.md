---
name: Release checklist
about: Plan final steps before software release
title: "Release checklist version: x.y.z"
type: "Task"
assignees: ''
---

## Release checklist: version *x.y.z*

- [ ] Close any issues assigned to this release
- [ ] Update version number, publish date, etc. in DESCRIPTION
- [ ] Mint new DOI for release if minor or major release
- [ ] Update DOI, version number, publish date, etc. in README.md
- [ ] Check and update contributors list and update CITATION if needed
- [ ] check that all new functions have unit tests
- [ ] update NEWS
- [ ] Check spelling
    - [ ] Check spelling (`spelling::spell_check_package()`)
    - [ ] Update wordlist (`spelling::update_wordlist()`)
- [ ] All tests pass with `devtools::test()` locally
- [ ] No errors, warnings, notes in `devtools::check(cran=TRUE, incoming=TRUE)` locally in R-release
- [ ] run revdepcheck::revdep_check()
- [ ] R CMD check passes under Github Actions
    - [ ] MacOS (latest) R-release
    - [ ] Windows (latest) R-release
    - [ ] Ubuntu (latest) R-release
    - [ ] Ubuntu (latest) R-devel
    - [ ] Ubuntu (latest) R-oldrel
- [ ] via win-builder
   - [ ] Windows: R release
   - [ ] Windows: R devel
- [ ] update cran_comments
- [ ] submit to CRAN

## Post CRAN approval

- [ ] create release tag (format `x.y.z`)
- [ ] write github release notes and publish release
