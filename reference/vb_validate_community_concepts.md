# Validate VegBank loader tables for new community concepts

Performs validation checks on VegBank loader tables to ensure data
integrity before upload. Validates required fields, uniqueness
constraints, and referential integrity between related tables. Prints
validation errors and warnings. This validation tool is a first pass at
catching errors - full validation is only done at upload.

## Usage

``` r
vb_validate_community_concepts(
  community_concepts,
  community_names = NULL,
  community_correlations = NULL,
  parties = NULL,
  references = NULL
)
```

## Arguments

- community_concepts:

  A data frame containing community concepts as community names
  associated with references, along with with status details and
  taxonomic parents

- community_names:

  A data frame containing community name usages associated with specific
  classification systems for new community concepts

- community_correlations:

  A data frame defining correlations between community concepts

- parties:

  A data frame containing details about new parties

- references:

  A data frame containing details about new references

## Value

A named list with one element per table, each containing a logical value
(TRUE if all validations passed for that table, FALSE otherwise). For
example:
`list(community_concepts = TRUE, community_names = FALSE, community_correlations = TRUE)`
