# Retrieve data from VegBank

Retrieve data from the VegBank REST API. `vb_get()` is the core function
that can access any resource type. Resource-specific functions like
`vb_get_plot_observations()` and `vb_get_plant_concepts()` are
convenience wrappers that provide resource-appropriate defaults. For
typical usage, the resource-specific functions should be preferred, but
`vb_get()` may be useful when using `vegbankr` in a R package or
leveraging any experimental VegBank read API features that may not (yet)
supported by `vegbankr`.

## Usage

``` r
vb_get_projects(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  search = NULL,
  sort = NULL,
  ...
)

vb_get_parties(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  search = NULL,
  sort = NULL,
  ...
)

vb_get_roles(vb_code = NULL, limit = 100, offset = 0, parquet = NULL, ...)

vb_get_references(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  search = NULL,
  sort = NULL,
  ...
)

vb_get_named_places(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  ...
)

vb_get_cover_methods(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  search = NULL,
  with_nested = NULL,
  ...
)

vb_get_stratum_methods(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  search = NULL,
  with_nested = NULL,
  ...
)

vb_get_plant_concepts(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  search = NULL,
  status = NULL,
  with_nested = NULL,
  ...
)

vb_get_taxon_observations(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  search = NULL,
  detail = NULL,
  with_nested = NULL,
  ...
)

vb_get_taxon_importances(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  detail = NULL,
  with_nested = NULL,
  ...
)

vb_get_stem_counts(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  detail = NULL,
  ...
)

vb_get_strata(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  detail = NULL,
  ...
)

vb_get_taxon_interpretations(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  detail = NULL,
  ...
)

vb_get_community_concepts(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  search = NULL,
  status = NULL,
  with_nested = NULL,
  ...
)

vb_get_community_classifications(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  search = NULL,
  detail = NULL,
  with_nested = NULL,
  ...
)

vb_get_community_interpretations(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  detail = NULL,
  with_nested = NULL,
  ...
)

vb_get_plot_observations(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  search = NULL,
  status = NULL,
  detail = NULL,
  with_nested = NULL,
  num_taxa = NULL,
  num_comms = NULL,
  ...
)

vb_get_user_datasets(
  vb_code = NULL,
  limit = 100,
  offset = 0,
  parquet = NULL,
  ...
)

vb_get(
  resource,
  vb_code = NULL,
  by = NULL,
  parquet = TRUE,
  clean_names = FALSE,
  limit = 100,
  offset = 0,
  ...
)
```

## Arguments

- vb_code:

  Optional VegBank code to retrieve a specific record.

- limit:

  Integer specifying maximum number of records to return. Default
  is 100. Set to `NULL` to use API default.

- offset:

  Integer specifying the offset for pagination. Default is 0. Set to
  `NULL` to use API default.

- parquet:

  Logical indicating whether to request data in Parquet format instead
  of JSON. This is transparent to users insofar as data will be returned
  as a data frame in either case, but performance may differ between the
  two options. If not specified, defaults to `TRUE` for collection
  queries and `FALSE` for single-record queries.

- search:

  Optional search string for filtering results based on full-text search
  or the resource's vb_code (the latter of which is functionally
  equivalent to using the `vb_code` argument directly). Available for:
  plot-observations, plant-concepts, community-concepts, projects, and
  parties.

- sort:

  Optional string for sorting results. Prepend with "-" for descending
  order (e.g., "-obs_count"). Available for:

  - **plot-observations:** "default", "author_obs_name"

  - **plant-concepts:** "default", "plant_name", "obs_count"

  - **community-concepts:** "default", "comm_name", "obs_count"

  - **projects:** "default", "project_name", "obs_count"

  - **parties:** "default", "surname", "organization_name", "obs_count"

- ...:

  Additional query parameters passed to the API endpoint as key-value
  pairs. E.g., foo="bar" will add a URL query parameter "?foo=bar" to
  the API GET request.

- with_nested:

  Logical indicating whether to include nested data structures. All
  endpoints support `FALSE`. For those that support `TRUE`, this is the
  default for individual record queries, otherwise the default is
  `FALSE`. Set to `NULL` to use the API default.

- status:

  Optional string for limiting results by status, with default
  equivalent to 'any' if unset. Available for:

  - **plot-observations:** "any", "current"

  - **community-concepts:** "any", "current", "accepted",
    "current_accepted"

- detail:

  Character string specifying level of detail. All endpoints support
  "full" detail. For those that support "minimal" detail, this is the
  default for collection queries, otherwise the default is "full". Plot
  observations additionally support `detail="geo"`. In all cases, set to
  `NULL` to use the API default.

- num_taxa:

  *Available only for plot-observations.* Integer specifying maximum
  number of taxon observations to return as a nested array. If not
  specified, defaults to 5 for collection queries and 5000 for
  single-record queries. Set to `NULL` to use the API default.

- num_comms:

  *Available only for plot-observations.* Integer specifying maximum
  number of community classifications records to return as a nested
  array. If not specified, defaults to 5 for collection queries and 5000
  for single-record queries. Set to `NULL` to use the API default.

- resource:

  *Available only for `vb_get()`.* Character string specifying the
  VegBank resource type to retrieve (e.g., "plot-observations",
  "projects").

- by:

  *Available only for `vb_get()`.* Character string specifying the
  VegBank resource type associated with the provided `vb_code` (if
  specified). If omitted, this will be inferred based on the format of
  the `vb_code`, so in general it is not necessary. However, if using
  `vb_get()` in a function, specifying `by` may be useful to ensure that
  the correct resource is being queried.

- clean_names:

  *Available only for `vb_get()`.* Logical indicating whether to
  canonicalize column names to snake case. Default is `FALSE`.

## Value

A data frame containing the requested VegBank data.

## Details

The `vb_get*()` family of functions can all be used to perform three
types of queries, which differ in terms of the set of records returned
as well as the default query parameters applied.

### Available resource-specific functions:

- `vb_get_plot_observations()` - Plot observation data

- `vb_get_community_concepts()` - Community concepts (assertions) linked
  to community names through usages

- `vb_get_community_classifications()` - Community classification events
  wherein one or more community concepts were applied to a plot
  observation

- `vb_get_community_interpretations()` - Assignments of community names
  and authorities (i.e., community concepts) to specific plot
  observations, as part of a community classification event

- `vb_get_plant_concepts()` - Plant concepts (assertions) linked to
  plant names through usages

- `vb_get_taxon_observations()` - Data provider's determination of taxa
  observed on a plot, and the overall cover of those taxa

- `vb_get_taxon_importances()` - Cover/etc, and optionally stem counts,
  of taxa observed on a plot, overall and by stratum if so defined

- `vb_get_stem_counts()` - Stem counts of observed taxa, overall and by
  stratum if so defined

- `vb_get_strata()` - Defined strata in which taxon importance and/or
  stem counts have been recorded within a given plot observation,
  consistent with some stratum method

- `vb_get_taxon_interpretations()` - Assignments of taxon names and
  authorities (i.e., plant concepts) to specific taxon observations

- `vb_get_cover_methods()` - Information about registered coverclass
  methods

- `vb_get_stratum_methods()` - Information about registered strata
  sampling protocols

- `vb_get_named_places()` - Information about named places registered in
  VegBank

- `vb_get_references()` - Information about references cited within
  VegBank

- `vb_get_projects()` - Information about projects established to
  collect vegetation plot data

- `vb_get_parties()` - Information about people and organizations who
  have contributed to the collection or interpretation of a plot

- `vb_get_roles()` - VegBank-defined role types, which can be used to
  define the role of a party contributing to a plot observation,
  classification, etc

- `vb_get_user_datasets()` - Information about people and organizations
  who have contributed to the collection or interpretation of a plot

### Query types

#### Single-record queries

A query is considered a "single-record query" if a `vb_code` is provided
with prefix that matches the type of the target resource. For example,
`vb_get_plot_observations("ob.2948")` will return the single VegBank
plot observation record corresponding to "ob.2948", if one exists.

#### Cross-resource collection queries

A query is considered a "cross-resource collection query" if a `vb_code`
is provided with prefix matching a resource type that *differs* from the
target resource. For example, `vb_get_plot_observations("pj.340")`, will
return the collection of VegBank plot observation records corresponding
to project `pj.340`, if the project and corresponding plot observations
exist.

#### Full collection queries

A query is considered a "full collection query" if no `vb_code` is
provided. For example, `vb_get_plot_observations()` will return all plot
observations. Both collection query types are paginated via `limit` and
`offset`, and can be further filtered with parameters like `search` when
supported.

### Smart defaults:

Many parameters have "smart defaults" that change based on whether
you're querying a single record or a collection of records. In general,
single-record queries automatically use settings optimized for detailed
individual records (more nested data, higher limits for related
entities), whereas collection queries use leaner settings. For example,
`vb_get_plot_observations()` uses `detail = "minimal"` and
`with_nested = FALSE` for collection queries, but `detail = "full"` and
`with_nested = TRUE` for single records.

To use the API's own defaults instead of these smart defaults,
explicitly pass `NULL` for the parameter (e.g., `detail = NULL`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Collection query - uses minimal detail and excludes nested fields
plots <- vb_get_plot_observations(limit = 10)

# Single record - uses full detail with nested fields
plot <- vb_get_plot_observations(vb_code = "ob.12345")

# Override defaults explicitly
plots <- vb_get_plot_observations(limit = 10, detail = "full", num_taxa = 50)

# Use API defaults instead of smart defaults
plots <- vb_get_plot_observations(limit = NULL, detail = NULL, num_taxa = NULL)

# Get projects (different available parameters)
projects <- vb_get_projects(search = "heritage", limit = 20)
} # }
```
