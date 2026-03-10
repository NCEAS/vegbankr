# Set a base URL for the VegBank API

Sets a global option specifying the base URL for the VegBank API using
the provided `vb_base_url` string, optionally adding a port as
"http(s)://vb_base_url:port".

## Usage

``` r
vb_set_base_url(vb_base_url, port)
```

## Arguments

- vb_base_url:

  (character) The base URL, including protocol and domain

- port:

  (numeric) Optional port value

## See also

[`vb_get_base_url()`](https://nceas.github.io/vegbankr/reference/vb_get_base_url.md)

## Examples

``` r
vb_set_base_url("https://api.vegbank.org")
#> Using https://api.vegbank.org as base URL
vb_set_base_url("http://localhost", port = 8080)
#> Using http://localhost:8080 as base URL
```
