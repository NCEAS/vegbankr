# Enable VegBank API debugging mode

Set VegBank debug level used when `send`ing API requests. This currently
controls two things:

1.  Verbosity of API requests, specifically as handled by
    `httr::req_perform()`

2.  Reporting of the elapsed time, as a console message

## Usage

``` r
vb_debug(verbosity = 1)
```

## Arguments

- verbosity:

  Integer between 0-3 (Default: 1). Using 0 is equivalent to setting
  [`vb_undebug()`](https://nceas.github.io/vegbankr/reference/vb_undebug.md),
  in which case no information is reported. Values between 1-3 control
  verbosity level passed to
  [`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html),
  and in all cases include display of API request time duration.

## See also

[`vb_undebug()`](https://nceas.github.io/vegbankr/reference/vb_undebug.md),
[`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
