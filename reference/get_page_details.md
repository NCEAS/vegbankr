# Get paging details for a VegBank dataframe

Reports paging details associated with a dataframe produced by querying
an VegBank API endpoint with limit/offset-based pagination.

## Usage

``` r
get_page_details(x)
```

## Arguments

- x:

  Dataframe, presumably returned by a VegBank getter

## Value

Named vector with the following elements, if available (any of these
values not attached to the data frame will simply be missing from the
returned vector):

- count_reported: the full record count reported by the API

- offset: the record offset used in the API query

- limit: the record limit used in the API query

- count_returned: the actual count of returned records

## Examples

``` r
if (FALSE) { # \dontrun{
parties <- get_all_parties(limit=10, offset=50)
get_page_details(parties)
} # }
```
