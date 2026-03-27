# Validate VegBank loader tables for new stratum methods

Performs validation checks on VegBank loader tables to ensure data
integrity before upload. Validates required fields, uniqueness
constraints, and referential integrity between related tables. Prints
validation errors and warnings. This validation tool is a first pass at
catching errors - full validation is only done at upload.

## Usage

``` r
vb_validate_stratum_methods(stratum_methods, references = NULL)
```

## Arguments

- stratum_methods:

  A data frame containing stratum methods and their associated component
  stratum types

- references:

  A data frame containing details about new references

## Value

A named list with one element per table, each containing a logical value
(TRUE if all validations passed for that table, FALSE otherwise). For
example:
`list(community_concepts = TRUE, community_names = FALSE, community_correlations = TRUE)`
