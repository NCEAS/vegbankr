# Clear stored Bearer tokens

Removes the OAuth2 access token and refresh token previously stored by
[`vb_set_token()`](https://nceas.github.io/vegbankr/reference/vb_set_token.md).
After calling this function, subsequent API requests will be sent
without an `Authorization: Bearer` header.

## Usage

``` r
vb_unset_token()
```

## See also

[`vb_set_token()`](https://nceas.github.io/vegbankr/reference/vb_set_token.md)

## Examples

``` r
vb_unset_token()
#> VegBank token(s) cleared
```
