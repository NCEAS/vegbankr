# Validate VegBank loader tables for plot observations and related data

Performs validation checks on VegBank loader tables to ensure data
integrity before upload. Validates required fields, uniqueness
constraints, and referential integrity between related tables. Prints
validation errors and warnings. This validation tool is a first pass at
catching errors - full validation is only done at upload.

## Usage

``` r
vb_validate_plot_observations(
  plot_observations,
  projects = NULL,
  parties = NULL,
  references = NULL,
  soils = NULL,
  disturbances = NULL,
  community_classifications = NULL,
  strata = NULL,
  strata_cover_data = NULL,
  stem_data = NULL,
  taxon_interpretations = NULL,
  contributors = NULL
)
```

## Arguments

- plot_observations:

  A data frame containing details about plot observations

- projects:

  A data frame containing details about new projects

- parties:

  A data frame containing details about new parties

- references:

  A data frame containing details about new references

- soils:

  A data frame containing details about observed soils

- disturbances:

  A data frame containing details about observed disturbances

- community_classifications:

  A data frame containing community classifications of observed plots

- strata:

  A data frame containing details about strata defined for plot
  observations

- strata_cover_data:

  A data frame containing details about plant cover as observed in
  different strata of a plot

- stem_data:

  A data frame containing details about counts and/or other details
  about stems observed on a plot

- taxon_interpretations:

  A data frame containing taxon interpretations of plants observed on a
  plot

- contributors:

  A data frame associating parties with their contributions to plot
  observations, projects, and/or community classifications

## Value

A named list with one element per table, each containing a logical value
(TRUE if all validations passed for that table, FALSE otherwise). For
example:
`list(parties = TRUE, contributors = FALSE, plot_observations = TRUE)`
