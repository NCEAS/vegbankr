# Get the currently configured base URL for the VegBank API

Gets the base URL for the VegBank API. If previously set by the user,
e.g. via
[`vb_set_base_url()`](https://nceas.github.io/vegbankr/reference/vb_set_base_url.md),
this will be pulled from the global option (`vegbank.base_api_url`). If
this option is unset, the package default value of
`https://api.vegbank.org` will be used.

## Usage

``` r
vb_get_base_url()
```

## Value

A length-one character vector containing the base URL string

## See also

[`vb_set_base_url()`](https://nceas.github.io/vegbankr/reference/vb_set_base_url.md)
