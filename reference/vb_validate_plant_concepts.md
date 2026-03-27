# Validate VegBank loader tables for new plant concepts

Performs validation checks on VegBank loader tables to ensure data
integrity before upload. Validates required fields, uniqueness
constraints, and referential integrity between related tables. Prints
validation errors and warnings. This validation tool is a first pass at
catching errors - full validation is only done at upload.

## Usage

``` r
vb_validate_plant_concepts(
  plant_concepts,
  plant_names = NULL,
  plant_correlations = NULL,
  parties = NULL,
  references = NULL
)
```

## Arguments

- plant_concepts:

  A data frame containing plant concepts as plant names associated with
  references, along with with status details and taxonomic parents

- plant_names:

  A data frame containing plant name usages associated with specific
  classification systems for new plant concepts

- plant_correlations:

  A data frame defining correlations between plant concepts

- parties:

  A data frame containing details about new parties

- references:

  A data frame containing details about new references

## Value

A named list with one element per table, each containing a logical value
(TRUE if all validations passed for that table, FALSE otherwise). For
example:
`list(parties = TRUE, contributors = FALSE, plot_observations = TRUE)`
