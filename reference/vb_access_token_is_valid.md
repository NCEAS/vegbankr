# Check whether the stored access token is present and not expired

Decodes the `exp` claim from the stored JWT access token and returns
`TRUE` if the token exists and has a valid expiry.

## Usage

``` r
vb_access_token_is_valid()
```

## Value

Logical `TRUE` if the access token is present and valid, `FALSE`
otherwise.

## See also

[`vb_set_token()`](https://nceas.github.io/vegbankr/reference/vb_set_token.md),
[`vb_refresh_tokens()`](https://nceas.github.io/vegbankr/reference/vb_refresh_tokens.md)

## Examples

``` r
if (FALSE) { # \dontrun{
vb_access_token_is_valid()
} # }
```
