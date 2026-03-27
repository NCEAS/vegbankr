# Upload data to VegBank

Upload data to VegBank via the REST API. `vb_upload()` is the core
function that can upload to any of the supported `POST` endpoints.
Resource-specific functions like `vb_upload_plot_observations()` are
convenience wrappers that apply resource-specific validation. For
typical usage, the resource-specific functions should be preferred.

## Usage

``` r
vb_upload(resource, ..., query_params = NULL, dry_run = FALSE)

vb_upload_plot_observations(
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
  contributors = NULL,
  dry_run = FALSE
)

vb_upload_plant_concepts(
  plant_concepts,
  plant_names = NULL,
  plant_correlations = NULL,
  parties = NULL,
  references = NULL,
  what_to_deactivate = NULL,
  dry_run = FALSE
)

vb_upload_community_concepts(
  community_concepts,
  community_names = NULL,
  community_correlations = NULL,
  parties = NULL,
  references = NULL,
  what_to_deactivate = NULL,
  dry_run = FALSE
)

vb_upload_taxon_interpretations(
  taxon_interpretations,
  parties = NULL,
  references = NULL,
  dry_run = FALSE
)

vb_upload_community_classifications(
  community_classifications,
  parties = NULL,
  references = NULL,
  contributors = NULL,
  dry_run = FALSE
)

vb_upload_cover_methods(cover_methods, references = NULL, dry_run = FALSE)

vb_upload_stratum_methods(stratum_methods, references = NULL, dry_run = FALSE)
```

## Arguments

- resource:

  *Available only for `vb_upload()`.* Character string specifying the
  VegBank resource type to upload (e.g., "plot-observations",
  "projects").

- ...:

  Named data frames to upload. Each data frame should correspond to a
  table expected by the VegBank API for the specified resource. All
  arguments must be named, and at least one data frame must be provided.

- query_params:

  A named list of parameter names and values to pass to the API as query
  parameters. For example, `list(foo="bar")` will be treated as
  `?foo=bar` in the request URL.

- dry_run:

  Logical indicating whether to perform a dry run. If `TRUE`, the API
  will validate the data without committing changes to the database.
  Default is `FALSE`.

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

- plant_concepts:

  A data frame containing plant concepts as plant names associated with
  references, along with with status details and taxonomic parents

- plant_names:

  A data frame containing plant name usages associated with specific
  classification systems for new plant concepts

- plant_correlations:

  A data frame defining correlations between plant concepts

- what_to_deactivate:

  *Available only for `vb_upload_plant_concepts()` and
  `vb_upload_community_concepts()`.* Character string specifying what
  existing concepts to deactivate in VegBank. Supported values are
  "none" and "by_party", along with "by_party_below_order" for plant
  uploads only. VegBank default is `none`.

- community_concepts:

  A data frame containing community concepts as community names
  associated with references, along with with status details and
  taxonomic parents

- community_names:

  A data frame containing community name usages associated with specific
  classification systems for new community concepts

- community_correlations:

  A data frame defining correlations between community concepts

- cover_methods:

  A data frame containing cover methods and their associated component
  cover indexes

- stratum_methods:

  A data frame containing stratum methods and their associated component
  stratum types

## Value

The processed response object from the VegBank API documenting what (if
anything) was successfully uploaded to VegBank.

## Details

The `vb_upload*()` family of functions can all be used to upload data to
VegBank queries, with each one differing in terms of what input
dataframes are expected (and, in some case, required).

### Available resource-specific functions:

1.  `vb_upload_plot_observations()` - Plot observational data, including
    soil and disturbance observations, plant taxon observations and
    importance assessments (potentially including stem-level details)
    within any defined strata, and both individual plant taxon and
    overall vegetation community interpretation.

2.  `vb_upload_plant_concepts()` - Plant concepts linked to plant names
    through usages, with some status designation

3.  `vb_upload_community_concepts()` - Community concepts linked to
    community names through usages, with some status designation

4.  `vb_upload_taxon_interpretations()` - Re-interpretation of existing
    observation), associating them with one or more VegBank plant
    concepts

5.  `vb_upload_community_classifications()` - Re-interpretation of
    existing VegBank plot observations, associating them with one or
    more VegBank community concepts as part of a classification activity

6.  `vb_upload_cover_methods()` - New cover methods, including all
    component cover indexes defined by the method

7.  `vb_upload_stratum_methods()` - New stratum methods, including all
    component stratum types defined by the method

If
[`vb_debug()`](https://nceas.github.io/vegbankr/reference/vb_debug.md)
is enabled, additional debugging details will be reported to the
console, primarily focused on the data being uploaded.
