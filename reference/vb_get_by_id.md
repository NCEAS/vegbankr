# Retrieve a VegBank resource by identifier

Fetches a VegBank resource using any supported identifier. This function
first resolves the identifier to determine the resource type and
internal code, then retrieves the full resource data.

## Usage

``` r
vb_get_by_id(identifier, ..., verbose = FALSE)
```

## Arguments

- identifier:

  A character string specifying the VegBank identifier. This can be an
  accession code, DOI, or other supported identifier type.

- ...:

  Additional query parameters passed to
  [`vb_get()`](https://nceas.github.io/vegbankr/reference/vb_get.md) as
  key-value pairs. E.g., foo="bar" will add a URL query parameter
  "?foo=bar" to the API GET request.

- verbose:

  Logical. If `TRUE`, prints a message indicating which resource was
  retrieved. Default is `FALSE`.

## Value

A data frame containing the requested resource data. The structure
depends on the resource type.

## See also

[`vb_resolve()`](https://nceas.github.io/vegbankr/reference/vb_resolve.md)
for identifier resolution details

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve a dataset silently
data <- vb_get_by_id("VB.Ob.2948.ACAD143")

# Retrieve with informational message
data <- vb_get_by_id("VB.Ob.2948.ACAD143", verbose = TRUE)
} # }
```
