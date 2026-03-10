# Canonicalize VegBank column names (i.e. convert to snake_case), using a package-provided lookup table by default

Takes a data frame of VegBank records, and canonicalizes the column
names by converting them to snake_case via a package-provided
lookup_table. If any column names in the input data frame are unmatched
in the lookup table, these are left unaltered in the output, and a
warning message is displayed.

## Usage

``` r
canonicalize_names(target_df, lookup_df)
```

## Arguments

- target_df:

  (dataframe)

- lookup_df:

  (dataframe) Optional custom names lookup table

## Value

A data frame matching the input data frame, but with canonicalized
column names

## Details

Callers may optionally provide a lookup table

## Examples

``` r
canonicalize_names(data.frame(
  stratum_ID = integer(),
  stratummethodname = character()
))
#> [1] stratum_id          stratum_method_name
#> <0 rows> (or 0-length row.names)
```
