# Refresh stored Bearer tokens

Calls the VegBank `/refresh` endpoint with the currently stored refresh
token to obtain a new access token and refresh token, then stores both
via
[`vb_set_token()`](https://nceas.github.io/vegbankr/reference/vb_set_token.md).

## Usage

``` r
vb_refresh_tokens()
```

## See also

[`vb_set_token()`](https://nceas.github.io/vegbankr/reference/vb_set_token.md),
[`vb_unset_token()`](https://nceas.github.io/vegbankr/reference/vb_unset_token.md)

## Examples

``` r
if (FALSE) { # \dontrun{
vb_refresh_tokens()
} # }
```
