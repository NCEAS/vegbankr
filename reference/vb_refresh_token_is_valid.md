# Check whether the stored refresh token is present and not expired

Decodes the `exp` claim from the stored JWT refresh token and returns
`TRUE` if the token exists and has a valid expiry.

## Usage

``` r
vb_refresh_token_is_valid()
```

## Value

Logical `TRUE` if the refresh token is present and valid, `FALSE`
otherwise.

## See also

[`vb_set_token()`](https://nceas.github.io/vegbankr/reference/vb_set_token.md),
[`vb_refresh_tokens()`](https://nceas.github.io/vegbankr/reference/vb_refresh_tokens.md)

## Examples

``` r
if (FALSE) { # \dontrun{
vb_refresh_token_is_valid()
} # }
```
