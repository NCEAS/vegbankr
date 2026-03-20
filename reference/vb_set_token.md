# Set Bearer token(s) for authenticated API requests

Stores an OAuth2 access token and/or refresh token in global options for
use in subsequent authenticated API requests. Use
[`vb_unset_token()`](https://nceas.github.io/vegbankr/reference/vb_unset_token.md)
to clear previously stored tokens.

## Usage

``` r
vb_set_token(access_token = NULL, refresh_token = NULL, tokens = NULL)
```

## Arguments

- access_token:

  (character) Access token string.

- refresh_token:

  (character) Refresh token string.

- tokens:

  (list \| character) Named list or JSON string containing
  `access_token` and/or `refresh_token`. Cannot be combined with
  `access_token` or `refresh_token`.

## Details

Two input modes are supported — use one or the other, not both:

- **Individual strings:** pass `access_token`, `refresh_token`, or both.

- **Token dict:** pass `tokens` as a named list or JSON string with
  `access_token` and/or `refresh_token` keys.

## See also

[`vb_unset_token()`](https://nceas.github.io/vegbankr/reference/vb_unset_token.md),
[`vb_refresh_tokens()`](https://nceas.github.io/vegbankr/reference/vb_refresh_tokens.md)

## Examples

``` r
vb_set_token(access_token = "eyJhbGciOiJIUzI1NiJ9...")
#> VegBank token(s) updated: access token
#> Note: no refresh token provided. Provide a refresh token via vb_set_token() to enable automatic renewal.
vb_set_token(access_token = "eyJ...", refresh_token = "eyJ...")
#> VegBank token(s) updated: access token and refresh token
vb_set_token(tokens = list(access_token = "eyJ...", refresh_token = "eyJ..."))
#> VegBank token(s) updated: access token and refresh token
vb_set_token(tokens = '{"access_token": "eyJ...", "refresh_token": "eyJ..."}')
#> VegBank token(s) updated: access token and refresh token
```
