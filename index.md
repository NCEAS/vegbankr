## vegbankr: An R client for the VegBank API

  
[![vegbank](reference/figures/logo_vegbank_high_res.svg)](https://nceas.github.io/vegbankr/)

[![R-CMD-check](https://github.com/NCEAS/vegbankr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NCEAS/vegbankr/actions/workflows/R-CMD-check.yaml)

------------------------------------------------------------------------

  

- **License**: [Apache 2](https://opensource.org/license/apache-2-0)
- [Package documentation](https://nceas.r-universe.dev/vegbankr)
- [Package source code on GitHub](https://github.com/NCEAS/vegbankr)
- [**Submit Bugs and feature
  requests**](https://github.com/NCEAS/vegbankr/issues)
- Contact us: <help@vegbank.org>
- [VegBank discussions](https://github.com/NCEAS/vegbank2/discussions)
- Citation:

> *Jim Regetz, Matthew B. Jones, Rushiraj Nenuji, Jeanette Clark, Maggie
> Klope*. 2026. **vegbankr: An R API package for the VegBank data
> system**. Version 1.0.0. VegBank.
> [`doi:10.82902/J1MW28`](https://doi.org/10.82902/J1MW28)

This package is an R client for VegBank, the vegetation plot database of
the Ecological Society of America’s [Panel on Vegetation
Classification](https://esa.org/vegpanel/), hosted by the [National
Center for Ecological Analysis and
Synthesis](https://www.nceas.ucsb.edu) (NCEAS). VegBank contains
vegetation plot data, community types recognized by the U.S. National
Vegetation Classification and others, and all ITIS/USDA plant taxa along
with other taxa recorded in plot records. As a VegBank API client, the
`vegbankr` package currently supports querying and downloading
vegetation plot records and other supporting information from the
VegBank database, and will soon support validating and uploading new
data to the VegBank database as well.

VegBank in general, and the `vegbankr` package in particular, are open
source, community projects. We [welcome
contributions](https://nceas.github.io/vegbankr/CONTRIBUTING.md) in many
forms, including code, data, documentation, bug reports, testing, etc.
Use the [VegBank
discussions](https://github.com/NCEAS/vegbank2/discussions) to discuss
these contributions with us.

## Installation

The `vegbankr` package is not yet available on CRAN, but you can install
it directly from this GitHub repository using either the
[`remotes`](https://github.com/r-lib/remotes) or
[`devtools`](https://github.com/r-lib/devtools/) package. First install
one of those packages, and then use it to install `vegbankr` as follows:

``` r
# or use `devtools::` if you prefer
remotes::install_github("nceas/vegbankr")
```

## Usage examples

To view more details about the VegBank API … you’ll have to be a little
patient. It’s still in development! Keep an eye on the core VegBank repo
at <https://github.com/NCEAS/vegbank2> for developments and
announcements.

On that note, prior to its production release, the main VegBank API
(<https://api.vegbank.org>) may or may not be available at any given
time. However, until then, you should be able to explore and prototype
against the development API (<https://api-dev.vegbank.org>). Configure
`vegbankr` to use the development API using the following expression:

``` r
library(vegbankr)

# the package default URL is https://api.vegbank.org
vb_set_base_url("https://api-dev.vegbank.org")
```

How many projects are currently registered in VegBank?

``` r
vb_count_projects()
```

Search for “*GAP*” related projects, returning them sorted in descending
order by observation count.

``` r
vb_get_projects(search = "GAP", sort = "-obs_count") |>
  dplyr::select(pj_code, project_name, obs_count)
```

Get the first 100 plot observations associated with project `pj.11044`
(*Pennsylvania HP Delaware Water Gap*), sorted by author_obs_code, then
check out where they are located.

``` r
obs <- vb_get_plot_observations("pj.11044", sort = "author_obs_code",
  limit = 100)
obs |> dplyr::count(state_province)
```

Grab a single plot observation record based on its “ob” code.

``` r
ob.135454 <- vb_get_plot_observations("ob.135454", detail = "full",
  with_nested = TRUE)
```

Get the taxon (plant) observations associated with this plot
observation, displaying them in order based on the plant code of the
current taxon interpretation.

``` r
vb_get_taxon_observations("ob.135454") |>
  dplyr::arrange(int_curr_plant_code) |>
  print(n = 35)
```

Now search for community concepts with the string `"_sequoiadendron_"`.

``` r
sequoia_communities <- vb_get_community_concepts(search = "sequoiadendron")
```

Determine which concept has the most plot observations, then retrieve
all of those plot observations from VegBank.

``` r
sequoia_plots <- sequoia_communities |>
  dplyr::arrange(-obs_count) |>
  dplyr::slice(1) |>
  dplyr::pull(cc_code) |>
  vb_get_plot_observations()
```

## Authentication

VegBank API `write` endpoints require a Bearer token. Set it once per
session and all subsequent API calls include it automatically.

### Step 1 — Obtain your tokens

Visit the VegBank [login page](https://api.vegbank.org/login) and
authenticate with your ORCID credentials. You will receive a JSON
response containing an `access_token` and a `refresh_token`:

``` json
{
  "token": {
    "access_token": "eyJhbGciOiJSUzI1NiIsInR5cCI...",
    "refresh_token": "eyJhbGciOiJSUzI1NiIsInR5cCI..."
  }
}
```

For more details see the [API authorization
docs](https://github.com/NCEAS/vegbank2/blob/develop/helm/docs/api-authorization.md).

### Step 2 — Set the tokens in R

You can provide individual token strings:

``` r
vb_set_token(
  access_token  = "eyJhbGciOiJSUzI1NiIsInR5cCI...",
  refresh_token = "eyJhbGciOiJSUzI1NiIsInR5cCI..."
)
```

Or pass the full JSON response or a named list directly:

``` r
# Named list
vb_set_token(tokens = list(
  access_token  = "eyJ...",
  refresh_token = "eyJ..."
))

# JSON string (e.g. copied from /authorize response)
vb_set_token(tokens = '{"access_token": "eyJ...", "refresh_token": "eyJ..."}')
```

### Step 3 — Upload data

``` r
# dry_run = TRUE validates the payload without committing to the database
vb_upload_plant_concepts(plant_concepts =
  data.frame(
    user_pc_code = "PC_1",
    name = "Test concept",
    vb_rf_code = "rf.33",
    vb_status_py_code = "py.511",
    plant_concept_status = "test status",
    start_date = "2026-03-01"
  ), dry_run = TRUE)
```

### Token lifecycle

**Automatic refresh.** When an API request is made and the access token
has expired, `vegbankr` will automatically use the stored refresh token
to obtain a new token pair — no manual intervention required. If the
refresh token has also expired, you will be prompted to re-authenticate.

**Check validity.** You can inspect token status at any time:

``` r
vb_access_token_is_valid()
vb_refresh_token_is_valid()
```

**Manual refresh.** To explicitly refresh the tokens before they expire:

``` r
vb_refresh_tokens()
```

**Clear tokens.** To remove all stored tokens from the session:

``` r
vb_unset_token()
```

A `401 Unauthorized` response means the token is missing or expired —
repeat Steps 1–2 to re-authenticate.

## License

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

## Acknowledgements

Work on this package was supported by:

- California Department of Fish and Wildlife
- The ESA Panel on Vegetation Classification

Additional support was provided for collaboration by the National Center
for Ecological Analysis and Synthesis, a Center funded by the University
of California, Santa Barbara, and the State of California.

[![nceas_footer](https://www.nceas.ucsb.edu/sites/default/files/2020-03/NCEAS-full%20logo-4C.png)](https://www.nceas.ucsb.edu)
