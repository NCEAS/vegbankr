## vegbankr: An R client for the VegBank API
<!-- badges: start -->
[![R-CMD-check](https://github.com/NCEAS/vegbankr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NCEAS/vegbankr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

- **Authors**: Jim Regetz, Matthew B. Jones
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on GitHub](https://github.com/NCEAS/vegbankr)
- [**Submit Bugs and feature requests**](https://github.com/NCEAS/vegbankr/issues)
- Contact us: help@vegbank.org
- [VegBank discussions](https://github.com/NCEAS/vegbank2/discussions)

This package is an R client for VegBank, the vegetation plot database of
the Ecological Society of America's Panel on Vegetation Classification,
hosted by the [National Center for Ecological Analysis and
Synthesis](https://www.nceas.ucsb.edu) (NCEAS). VegBank contains
vegetation plot data, community types recognized by the U.S.  National
Vegetation Classification and others, and all ITIS/USDA plant taxa along
with other taxa recorded in plot records. As a VegBank API client, the
'vegbankr' package supports querying, downloading, validating, and
uploading vegetation plot records and other supporting information to
and from the VegBank database.

VegBank in general, and the vegbankr package in particular, are open
source, community projects. We [welcome contributions](./CONTRIBUTING.md)
in many forms, including code, data, documentation, bug reports,
testing, etc. Use the [VegBank
discussions](https://github.com/NCEAS/vegbank2/discussions) to discuss
these contributions with us.

## Documentation

Documentation is a work in progress, and is included in the package
using standard R package documentation mechanisms.

## Development build

This is an R package, and built using ...

To install locally, ...

To run tests, ...

## Usage Example

To view more details about the VegBank API ... you'll just have to wait.
It's still in development!

```r
# Don't try this at home yet
library(vegbankr)

plot_observation <- get_plot_observation("<ob_code>")
```

## License
```
Copyright [2025] [Regents of the University of California]

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

## Acknowledgements
Work on this package was supported by:

- California Department of Fish and Wildlife
- The ESA Panel on Vegetation Classification

Additional support was provided for collaboration by the National Center for Ecological Analysis and Synthesis, a Center funded by the University of California, Santa Barbara, and the State of California.

[![nceas_footer](https://www.nceas.ucsb.edu/sites/default/files/2020-03/NCEAS-full%20logo-4C.png)](https://www.nceas.ucsb.edu)
