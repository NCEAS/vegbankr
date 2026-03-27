# Create a VegBank dataset

Use the VegBank API to create a user dataset, which defines a collection
of plot observations that can be cited in VegBank.

## Usage

``` r
vb_create_dataset(name, description, observations, dry_run = FALSE)
```

## Arguments

- name:

  A single character string giving the dataset name. Must be 100
  characters or fewer.

- description:

  A single character string describing the dataset.

- observations:

  A character vector of observation codes. Each element must match the
  pattern `"ob.<positive_integer>"` (e.g. `"ob.2948"`), and must refer
  to a plot observation in VegBank. The vector must not be empty.

- dry_run:

  Logical indicating whether to perform a dry run. If `TRUE`, the API
  will validate the data without committing changes to the database.
  Default is `FALSE`.

## Value

The processed response object from the VegBank API documenting what (if
anything) was successfully created in VegBank.

## Examples

``` r
if (FALSE) { # \dontrun{
vb_create_dataset(
  name = "Test Dataset 001",
  description = "A test dataset containing 10 observations",
  observations = paste0("ob.", 2948:2957)
)
} # }
```
