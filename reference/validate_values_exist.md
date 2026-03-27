# Validate values exist in parent table

Validate values exist in parent table

## Usage

``` r
validate_values_exist(
  child_df,
  child_col,
  parent_df,
  parent_col,
  optional = FALSE
)
```

## Arguments

- child_df:

  Child data frame

- child_col:

  Column name in child data frame

- parent_df:

  Parent data frame

- parent_col:

  Column name in parent data frame

- optional:

  (Boolean) If FALSE, check will fail if parent column is not found.
  Otherwise, check will skip.

## Value

Logical. TRUE if validation passes, FALSE otherwise
