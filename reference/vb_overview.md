# Retrieve VegBank summary stats

Gets basic summary stats from VegBank as a list of data frames,
including a table of high level counts and a few tables with "top N" (by
count) or "latest N" (by upload date) reports.

## Usage

``` r
vb_overview(limit = 5)
```

## Arguments

- limit:

  Integer specifying maximum number of records to return in the "top_n"
  and "latest_n" tables. Default is 5. Set to `NULL` to use API default.

## Value

A list of data frames corresponding to the returned summary tables

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve summary stats
stats_list <- vb_overview(limit=5)
} # }
```
