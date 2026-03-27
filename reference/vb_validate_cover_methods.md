# Validate VegBank loader tables for new cover methods

Performs validation checks on VegBank loader tables to ensure data
integrity before upload. Validates required fields, uniqueness
constraints, and referential integrity between related tables. Prints
validation errors and warnings. This validation tool is a first pass at
catching errors - full validation is only done at upload.

## Usage

``` r
vb_validate_cover_methods(cover_methods, references = NULL)
```

## Arguments

- cover_methods:

  A data frame containing cover methods and their associated component
  cover indexes

- references:

  A data frame containing details about new references

## Value

A named list with one element per table, each containing a logical value
(TRUE if all validations passed for that table, FALSE otherwise). For
example:
`list(community_concepts = TRUE, community_names = FALSE, community_correlations = TRUE)`
