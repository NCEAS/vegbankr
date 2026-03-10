# Get record count for a VegBank resource

`vb_count()` and friends (implemented for each of the VegBank resource
types) are used to retrieve a record count from the VegBank API,
optionally constrained by filtering query parameters. The count is the
same as the `count_reported` attribute attached to API data responses
(see
[`get_page_details()`](https://nceas.github.io/vegbankr/reference/get_page_details.md)),
but is returned here without actually querying for any data.

## Usage

``` r
vb_count(resource, ...)

vb_count_community_classifications(...)

vb_count_community_concepts(...)

vb_count_cover_methods(...)

vb_count_named_places(...)

vb_count_parties(...)

vb_count_plant_concepts(...)

vb_count_plot_observations(...)

vb_count_projects(...)

vb_count_references(...)

vb_count_roles(...)

vb_count_stem_counts(...)

vb_count_taxon_importances(...)

vb_count_strata(...)

vb_count_stratum_methods(...)

vb_count_taxon_observations(...)

vb_count_user_datasets(...)
```

## Arguments

- resource:

  VegBank API resource (e.g., `plot-observation`)

- ...:

  Additional API query parameters

## Value

Integer count, or NULL if the count cannot be retrieved

## Examples

``` r
if (FALSE) { # \dontrun{
vb_count("projects")
vb_count_projects(search="california")
} # }
```
