# Downloading data with vegbankr

## Introduction to `vegbankr`

This package is an R client for VegBank, the vegetation plot database of
the Ecological Society of America’s [Panel on Vegetation
Classification](https://esa.org/vegpanel/), hosted by the [National
Center for Ecological Analysis and
Synthesis](https://www.nceas.ucsb.edu) (NCEAS). VegBank contains
vegetation plot data, community types recognized by the U.S. National
Vegetation Classification and others, and all ITIS/USDA plant taxa along
with other taxa recorded in plot records.

As a VegBank API client, the `vegbankr` package currently supports
querying and downloading vegetation plot records, in addition to
validating and uploading new data to the VegBank database.

## Summary of Functions

| Function                                                                                     | Description                                                                                                                                          |
|----------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------|
| [`vb_get_plot_observations()`](https://nceas.github.io/vegbankr/reference/vb_get.md)         | Plot observation data                                                                                                                                |
| [`vb_get_community_concepts()`](https://nceas.github.io/vegbankr/reference/vb_get.md)        | Community concepts (assertions) linked to community names through usages                                                                             |
| [`vb_get_community_classifications()`](https://nceas.github.io/vegbankr/reference/vb_get.md) | Community classification events wherein one or more community concepts were applied to a plot observation                                            |
| [`vb_get_community_interpretations()`](https://nceas.github.io/vegbankr/reference/vb_get.md) | Assignments of community names and authorities (i.e., community concepts) to specific plot observations, as part of a community classification event |
| [`vb_get_plant_concepts()`](https://nceas.github.io/vegbankr/reference/vb_get.md)            | Plant concepts (assertions) linked to plant names through usages                                                                                     |
| [`vb_get_taxon_observations()`](https://nceas.github.io/vegbankr/reference/vb_get.md)        | Data provider’s determination of taxa observed on a plot, and the overall cover of those taxa                                                        |
| [`vb_get_taxon_interpretations()`](https://nceas.github.io/vegbankr/reference/vb_get.md)     | Assignments of taxon names and authorities (i.e., plant concepts) to specific taxon observations                                                     |
| [`vb_get_cover_methods()`](https://nceas.github.io/vegbankr/reference/vb_get.md)             | Information about registered coverclass methods                                                                                                      |
| [`vb_get_stratum_methods()`](https://nceas.github.io/vegbankr/reference/vb_get.md)           | Information about registered strata sampling protocols                                                                                               |
| [`vb_get_references()`](https://nceas.github.io/vegbankr/reference/vb_get.md)                | Information about references cited within VegBank                                                                                                    |
| [`vb_get_projects()`](https://nceas.github.io/vegbankr/reference/vb_get.md)                  | Information about projects established to collect vegetation plot data                                                                               |
| [`vb_get_parties()`](https://nceas.github.io/vegbankr/reference/vb_get.md)                   | Information about people and organizations who have contributed to the collection or interpretation of a plot                                        |

Many of the functions utilize the following arguments:

| Argument      | Description                                                                                            |
|---------------|--------------------------------------------------------------------------------------------------------|
| `vb_code`     | A VegBank code. For example, an observation code (`ob.*`) or project code (`pj.*`)                     |
| `detail`      | Level of detail returned: `"full"` includes all available fields; the default returns a summary subset |
| `with_nested` | If `TRUE`, nested child records (e.g. taxon observations, stratum data) are included as list columns   |
| `limit`       | Maximum number of records to return (default: 100)                                                     |
| `offset`      | Number of records to skip before returning results; useful for pagination                              |
| `sort`        | Field name to sort by; prefix with `-` for descending order (e.g. `"-obs_count"`)                      |
| `search`      | Optional search string for filtering results                                                           |

## Plot Observation Data

The
[`vb_get_plot_observations()`](https://nceas.github.io/vegbankr/reference/vb_get.md)
function allows users to download vegetation plot data from VegBank.

### Single Plot Observation by Plot Code

To retrieve a specific plot observation record using its “ob” code, use
the following command:

``` r
# Retrieve a specific plot observation
ob.135454 <- vb_get_plot_observations(vb_code = "ob.135454", detail = "full",
  with_nested = TRUE)

# Preview the downloaded data
head(ob.135454)
```

    ## # A tibble: 1 × 122
    ##    area author_datum   author_e       author_location author_n   author_obs_code
    ##   <dbl> <chr>          <chr>          <chr>           <chr>      <chr>          
    ## 1   100 <confidential> <confidential> <confidential>  <confiden… DEWA.1.2003    
    ## # ℹ 116 more variables: author_plot_code <chr>, author_zone <chr>,
    ## #   auto_taxon_cover <lgl>, azimuth <lgl>, basal_area <lgl>,
    ## #   bryophyte_quality <lgl>, cm_code <chr>, confidentiality_status <int>,
    ## #   country <chr>, cover_dispersion <lgl>, cover_method_name <chr>,
    ## #   date_accuracy <chr>, date_entered <chr>, disturbances <lgl>,
    ## #   dominant_stratum <lgl>, dsg_poly <lgl>, effort_level <lgl>,
    ## #   elevation <dbl>, elevation_accuracy <lgl>, elevation_range <lgl>, …

Omitting `detail = "full"` or setting `with_nested = FALSE` returns a
smaller summary better for quick browsing or larger downloads.

### Plot Observations for a Project

You can also download multiple plot observations for a specific project.
For example, to retrieve 100 plot observations from the Southwest GAP,
Nevada Project (`pj.10510`):

``` r
# Retrieve all plot observations for a specific project
pj.10510 <- vb_get_plot_observations(vb_code = "pj.10510")

# Preview the data
head(pj.10510)
```

    ## # A tibble: 6 × 12
    ##   ob_code  author_obs_code pl_code  author_plot_code has_observation_synonym
    ##   <chr>    <chr>           <chr>    <chr>            <lgl>                  
    ## 1 ob.51505 NV010603LH01    pl.51570 NV010603LH01     FALSE                  
    ## 2 ob.51506 NV010603LH02    pl.51571 NV010603LH02     FALSE                  
    ## 3 ob.51507 NV010603LH03    pl.51572 NV010603LH03     FALSE                  
    ## 4 ob.51508 NV010603LH04    pl.51573 NV010603LH04     FALSE                  
    ## 5 ob.51509 NV010603LH05    pl.51574 NV010603LH05     FALSE                  
    ## 6 ob.51510 NV010603LH06    pl.51575 NV010603LH06     FALSE                  
    ## # ℹ 7 more variables: latitude <dbl>, longitude <dbl>, area <int>,
    ## #   elevation <dbl>, country <int>, state_province <chr>, year <dbl>

## Project Data

[`vb_get_projects()`](https://nceas.github.io/vegbankr/reference/vb_get.md)
returns information about projects established to collect vegetation
plot data.

### Search by Project Name

This example retrieves all projects whose name contains “GAP”, sorted in
descending order by observation count so that the most data-rich
projects appear first.

``` r
vb_get_projects(search = "GAP", sort = "-obs_count")
```

    ## # A tibble: 6 × 8
    ##   pj_code  project_name       project_description start_date stop_date obs_count
    ##   <chr>    <chr>              <chr>                    <int>     <int>     <dbl>
    ## 1 pj.10510 Southwest GAP, Ne… http://earth.gis.u…         NA        NA     17326
    ## 2 pj.10507 Southwest GAP, Ar… http://earth.gis.u…         NA        NA     12082
    ## 3 pj.10511 Southwest GAP, Ut… http://earth.gis.u…         NA        NA      9781
    ## 4 pj.10509 Southwest GAP, Ne… http://earth.gis.u…         NA        NA      5693
    ## 5 pj.10508 Southwest GAP, Co… http://earth.gis.u…         NA        NA      5286
    ## 6 pj.11044 Pennsylvania HP D… Plant Community/As…         NA        NA       251
    ## # ℹ 2 more variables: last_plot_added_date <dttm>, search_rank <dbl>

### Plot Observations by Project Code

Once you have identified a project code, you can pass it directly to
`vb_get_plot_observations`. This example retrieves the first 100 records
of plot observations associated with the project `pj.11044`
(Pennsylvania HPD Delaware Water Gap), sorted by the author’s
observation code.

``` r
vb_get_plot_observations("pj.11044", sort = "author_obs_code")
```

    ## # A tibble: 100 × 12
    ##    ob_code   author_obs_code pl_code   author_plot_code has_observation_synonym
    ##    <chr>     <chr>           <chr>     <chr>            <lgl>                  
    ##  1 ob.135454 DEWA.1.2003     pl.136716 DEWA.1           FALSE                  
    ##  2 ob.135453 DEWA.10.2003    pl.136715 DEWA.10          FALSE                  
    ##  3 ob.135451 DEWA.100.2003   pl.136714 DEWA.100         FALSE                  
    ##  4 ob.135545 DEWA.101.2003   pl.136713 DEWA.101         FALSE                  
    ##  5 ob.135435 DEWA.102.2003   pl.136712 DEWA.102         FALSE                  
    ##  6 ob.135448 DEWA.103.2003   pl.136711 DEWA.103         FALSE                  
    ##  7 ob.135577 DEWA.104.2003   pl.136710 DEWA.104         FALSE                  
    ##  8 ob.134250 DEWA.105.2003   pl.136709 DEWA.105         FALSE                  
    ##  9 ob.135358 DEWA.106.2003   pl.136708 DEWA.106         FALSE                  
    ## 10 ob.135459 DEWA.107.2003   pl.136707 DEWA.107         FALSE                  
    ## # ℹ 90 more rows
    ## # ℹ 7 more variables: latitude <dbl>, longitude <dbl>, area <dbl>,
    ## #   elevation <dbl>, country <chr>, state_province <chr>, year <dbl>

## Party Data

[`vb_get_parties()`](https://nceas.github.io/vegbankr/reference/vb_get.md)
returns information about the people and organization who have
contributed to a plot, project, or plant/community interpretation

``` r
# get people associated with a project
vb_get_parties(vb_code = "pj.11044")
```

    ## # A tibble: 1 × 9
    ##   py_code  party_label        salutation given_name middle_name surname  
    ##   <chr>    <chr>                   <int> <chr>            <int> <chr>    
    ## 1 py.91547 Zimmerman, Ephraim         NA Ephraim             NA Zimmerman
    ## # ℹ 3 more variables: organization_name <chr>, contact_instructions <int>,
    ## #   obs_count <dbl>

``` r
# get people associated with a plot observation
vb_get_parties(vb_code = "ob.3298")
```

    ## # A tibble: 4 × 9
    ##   py_code party_label           salutation given_name middle_name surname       
    ##   <chr>   <chr>                      <int> <chr>            <int> <chr>         
    ## 1 py.1062 Reed, Cindy                   NA Cindy               NA Reed          
    ## 2 py.1245 Faber-Langendoen, Don         NA Don                 NA Faber-Langend…
    ## 3 py.1246 Von Loh, Jim                  NA Jim                 NA Von Loh       
    ## 4 py.1248 West, Keldyn                  NA Keldyn              NA West          
    ## # ℹ 3 more variables: organization_name <chr>, contact_instructions <int>,
    ## #   obs_count <dbl>

### Plot Observations by Party

Once you have identified a party `py` code, it can be used to return all
the plot observations associated with a person/organization:

``` r
vb_get_plot_observations(vb_code = "py.1062")
```

    ## # A tibble: 100 × 12
    ##    ob_code author_obs_code pl_code author_plot_code has_observation_synonym
    ##    <chr>   <chr>           <chr>   <chr>            <lgl>                  
    ##  1 ob.3062 FOLA.21         pl.3118 FOLA.21          FALSE                  
    ##  2 ob.3064 AGFO.2          pl.3120 AGFO.2           FALSE                  
    ##  3 ob.3065 AGFO.3          pl.3121 AGFO.3           FALSE                  
    ##  4 ob.3066 AGFO.4          pl.3122 AGFO.4           FALSE                  
    ##  5 ob.3079 AGFO.17         pl.3135 AGFO.17          FALSE                  
    ##  6 ob.3080 AGFO.18         pl.3136 AGFO.18          FALSE                  
    ##  7 ob.3081 AGFO.19         pl.3137 AGFO.19          FALSE                  
    ##  8 ob.3110 FOLA.25         pl.3166 FOLA.25          FALSE                  
    ##  9 ob.3111 FOLA.26         pl.3167 FOLA.26          FALSE                  
    ## 10 ob.3112 FOLA.27         pl.3168 FOLA.27          FALSE                  
    ## # ℹ 90 more rows
    ## # ℹ 7 more variables: latitude <dbl>, longitude <dbl>, area <dbl>,
    ## #   elevation <dbl>, country <chr>, state_province <chr>, year <dbl>

## Taxon Observations

[`vb_get_taxon_observations()`](https://nceas.github.io/vegbankr/reference/vb_get.md)
retrieves the individual plant taxon records associated with a given
plot observation. Each row represents one taxon recorded in the plot

### Taxon Observations by Plot

This example retrieves the taxon (plant) observations associated with
the plot `ob.135454`

``` r
vb_get_taxon_observations("ob.135454")
```

    ## # A tibble: 32 × 6
    ##    ob_code   to_code    int_curr_pc_code int_orig_pc_code taxon_inference_area
    ##    <chr>     <chr>      <chr>            <chr>                           <int>
    ##  1 ob.135454 to.2712789 pc.199973        pc.199973                          NA
    ##  2 ob.135454 to.2712790 pc.199285        pc.199285                          NA
    ##  3 ob.135454 to.2712791 pc.190695        pc.190695                          NA
    ##  4 ob.135454 to.2712792 pc.190492        pc.190492                          NA
    ##  5 ob.135454 to.2712793 pc.187519        pc.187519                          NA
    ##  6 ob.135454 to.2712794 pc.183905        pc.183905                          NA
    ##  7 ob.135454 to.2712795 pc.183661        pc.183661                          NA
    ##  8 ob.135454 to.2712796 pc.176794        pc.176794                          NA
    ##  9 ob.135454 to.2740072 pc.198464        pc.198464                          NA
    ## 10 ob.135454 to.2740073 pc.198388        pc.198388                          NA
    ## # ℹ 22 more rows
    ## # ℹ 1 more variable: rf_code <int>

## Plant Species Concepts

[`vb_get_plant_concepts()`](https://nceas.github.io/vegbankr/reference/vb_get.md)
can be used to retrieve the plant species concepts associated with plots
and projects.

``` r
vb_get_plant_concepts("ob.135454")
```

    ## # A tibble: 32 × 19
    ##    pc_code   plant_name             plant_code plant_description concept_rf_code
    ##    <chr>     <chr>                  <chr>                  <int> <chr>          
    ##  1 pc.111478 Acer rubrum L.         ACRU                      NA rf.37          
    ##  2 pc.111496 Acer saccharum Marsh.  ACSA3                     NA rf.37          
    ##  3 pc.111540 Achillea millefolium … ACMI2                     NA rf.37          
    ##  4 pc.115355 Anthoxanthum odoratum… ANOD                      NA rf.37          
    ##  5 pc.132596 Cornus racemosa Lam.   CORA6                     NA rf.37          
    ##  6 pc.145537 Euthamia graminifolia… EUGR5                     NA rf.37          
    ##  7 pc.146730 Fraxinus L.            FRAXI                     NA rf.37          
    ##  8 pc.147454 Galium mollugo L.      GAMO                      NA rf.37          
    ##  9 pc.151514 Hieracium L.           HIERA                     NA rf.37          
    ## 10 pc.152125 Houstonia caerulea L.  HOCA4                     NA rf.37          
    ## # ℹ 22 more rows
    ## # ℹ 14 more variables: concept_rf_label <chr>, status_rf_code <chr>,
    ## #   status_rf_label <chr>, obs_count <dbl>, plant_level <chr>, status <chr>,
    ## #   start_date <dttm>, stop_date <int>, current_accepted <lgl>, py_code <chr>,
    ## #   party_label <chr>, plant_party_comments <int>, parent_pc_code <chr>,
    ## #   parent_name <chr>

## Plant Community Concepts

The example below searches for community concepts that include the genus
`Sequoiadendron`.

``` r
sequoia_communities <- vb_get_community_concepts(search = "sequoiadendron")

# view the plots
head(sequoia_communities)
```

    ## # A tibble: 6 × 20
    ##   cc_code  comm_name comm_code comm_description concept_rf_code concept_rf_label
    ##   <chr>    <chr>     <chr>     <chr>            <chr>           <chr>           
    ## 1 cc.781   A.101     A.101     This forest all… rf.32           EcoArt 2002     
    ## 2 cc.5316  CEGL0031… CEGL0031… NA               rf.32           EcoArt 2002     
    ## 3 cc.7909  CEGL0086… CEGL0086… NA               rf.32           EcoArt 2002     
    ## 4 cc.21484 Sequoiad… CEGL0086… NA               rf.9844         Western Ecology…
    ## 5 cc.21485 Sequoiad… A.101     This forest all… rf.9844         Western Ecology…
    ## 6 cc.31147 Sequoiad… CEGL0031… These giant for… rf.9844         Western Ecology…
    ## # ℹ 14 more variables: status_rf_code <chr>, status_rf_label <chr>,
    ## #   obs_count <dbl>, comm_level <chr>, status <chr>, start_date <dttm>,
    ## #   stop_date <dttm>, current_accepted <lgl>, py_code <chr>, party_label <chr>,
    ## #   comm_party_comments <int>, parent_cc_code <chr>, parent_name <chr>,
    ## #   search_rank <dbl>

Then we can further determine which concept has the most plot
observations, then retrieve all of those plot observations from VegBank
by directly passing the community concepts codes into
[`vb_get_plot_observations()`](https://nceas.github.io/vegbankr/reference/vb_get.md)

``` r
sequoia_plots <- sequoia_communities |>
  dplyr::arrange(-obs_count) |>
  dplyr::slice(1) |>
  dplyr::pull(cc_code) |>
  vb_get_plot_observations()

head(sequoia_plots)
```

    ## # A tibble: 2 × 12
    ##   ob_code author_obs_code pl_code author_plot_code has_observation_synonym
    ##   <chr>   <chr>           <chr>   <chr>            <lgl>                  
    ## 1 ob.5363 YOSE.98M67      pl.5419 YOSE.98M67       FALSE                  
    ## 2 ob.5655 YOSE.99S104     pl.5711 YOSE.99S104      FALSE                  
    ## # ℹ 7 more variables: latitude <dbl>, longitude <dbl>, area <dbl>,
    ## #   elevation <dbl>, country <chr>, state_province <chr>, year <dbl>

## Other Function Options

### Changing Limit & Offset

To download more than 100 records, increase the `limit` argument. To
page through a large results in set chunks, combine `limit` and `offset`

``` r
# Download up to 500 records
vb_get_plot_observations(vb_code = "pj.10510", limit = 500)
```

    ## # A tibble: 500 × 12
    ##    ob_code  author_obs_code pl_code  author_plot_code has_observation_synonym
    ##    <chr>    <chr>           <chr>    <chr>            <lgl>                  
    ##  1 ob.51505 NV010603LH01    pl.51570 NV010603LH01     FALSE                  
    ##  2 ob.51506 NV010603LH02    pl.51571 NV010603LH02     FALSE                  
    ##  3 ob.51507 NV010603LH03    pl.51572 NV010603LH03     FALSE                  
    ##  4 ob.51508 NV010603LH04    pl.51573 NV010603LH04     FALSE                  
    ##  5 ob.51509 NV010603LH05    pl.51574 NV010603LH05     FALSE                  
    ##  6 ob.51510 NV010603LH06    pl.51575 NV010603LH06     FALSE                  
    ##  7 ob.51511 NV010603LH07    pl.51576 NV010603LH07     FALSE                  
    ##  8 ob.51512 NV010603LH08    pl.51577 NV010603LH08     FALSE                  
    ##  9 ob.51513 NV010603LH09    pl.51578 NV010603LH09     FALSE                  
    ## 10 ob.51514 NV010603LH10    pl.51579 NV010603LH10     FALSE                  
    ## # ℹ 490 more rows
    ## # ℹ 7 more variables: latitude <dbl>, longitude <dbl>, area <int>,
    ## #   elevation <dbl>, country <int>, state_province <chr>, year <dbl>

``` r
# Download the second page of 100 records
vb_get_plot_observations(vb_code = "pj.10510", limit = 100, offset = 100)
```

    ## # A tibble: 100 × 12
    ##    ob_code  author_obs_code pl_code  author_plot_code has_observation_synonym
    ##    <chr>    <chr>           <chr>    <chr>            <lgl>                  
    ##  1 ob.51605 NV011003JS20    pl.51670 NV011003JS20     FALSE                  
    ##  2 ob.51606 NV011003LH01    pl.51671 NV011003LH01     FALSE                  
    ##  3 ob.51607 NV011003LH02    pl.51672 NV011003LH02     FALSE                  
    ##  4 ob.51608 NV011003LH03    pl.51673 NV011003LH03     FALSE                  
    ##  5 ob.51609 NV011003LH04    pl.51674 NV011003LH04     FALSE                  
    ##  6 ob.51610 NV011003LH05    pl.51675 NV011003LH05     FALSE                  
    ##  7 ob.51611 NV011003LH06    pl.51676 NV011003LH06     FALSE                  
    ##  8 ob.51612 NV011003LH07    pl.51677 NV011003LH07     FALSE                  
    ##  9 ob.51613 NV011003LH08    pl.51678 NV011003LH08     FALSE                  
    ## 10 ob.51614 NV011003LH09    pl.51679 NV011003LH09     FALSE                  
    ## # ℹ 90 more rows
    ## # ℹ 7 more variables: latitude <dbl>, longitude <dbl>, area <int>,
    ## #   elevation <dbl>, country <int>, state_province <chr>, year <dbl>

### Sorting Results

Use the `sort` argument to order results by the following fields:

| Endpoint             | Sortable Fields                                        |
|----------------------|--------------------------------------------------------|
| `plot-observations`  | `default`, `author_obs_code`                           |
| `plant-concepts`     | `default`, `plant_name`, `obs_count`                   |
| `community-concepts` | `default`, `comm_name`, `obs_count`                    |
| `projects`           | `default`, `project_name`, `obs_count`                 |
| `parties`            | `default`, `surname`, `organization_name`, `obs_count` |

Prefix the field name with `-` for descending order. The example below
retrieves plot observations for projects containing by descending
`author_obs_name`

``` r
vb_get_plot_observations(vb_code = "pj.10510", sort = '-author_obs_code')
```

    ## # A tibble: 100 × 12
    ##    ob_code  author_obs_code pl_code  author_plot_code has_observation_synonym
    ##    <chr>    <chr>           <chr>    <chr>            <lgl>                  
    ##  1 ob.68830 NV121902JS30    pl.68895 NV121902JS30     FALSE                  
    ##  2 ob.68829 NV121902JS29    pl.68894 NV121902JS29     FALSE                  
    ##  3 ob.68828 NV121902JS28    pl.68893 NV121902JS28     FALSE                  
    ##  4 ob.68827 NV121902JS27    pl.68892 NV121902JS27     FALSE                  
    ##  5 ob.68826 NV121902JS26    pl.68891 NV121902JS26     FALSE                  
    ##  6 ob.68825 NV121902JS25    pl.68890 NV121902JS25     FALSE                  
    ##  7 ob.68824 NV121902JS24    pl.68889 NV121902JS24     FALSE                  
    ##  8 ob.68823 NV121902JS23    pl.68888 NV121902JS23     FALSE                  
    ##  9 ob.68822 NV121902JS22    pl.68887 NV121902JS22     FALSE                  
    ## 10 ob.68821 NV121802JS30    pl.68886 NV121802JS30     FALSE                  
    ## # ℹ 90 more rows
    ## # ℹ 7 more variables: latitude <dbl>, longitude <dbl>, area <int>,
    ## #   elevation <dbl>, country <int>, state_province <chr>, year <dbl>

## Manipulating Downloaded Data with `dplyr`

Because downloaded VegBank data is saved as dataframes, the data can be
manipulated using base R or `dplyr` functions. Below we highlight a few
possible data manipulations.

``` r
library(dplyr)
```

### Select a subset of Columns

This example retrieves the plot data for `ob.4577`, and then uses the
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
function to select only a subset of columns.

``` r
# Downloading plant concept data
plants <- vb_get_plant_concepts("ob.135454")

# Selecting only the plant_name. plant_code, and status columns
plants_small <- plants |> 
  dplyr::select(plant_name, plant_code, status)
```

### Filter Rows by a Condition

This example first retrieves the first 100 records for project
`pj.11044` (Pennsylvania HPD Delaware Water Gap), sorted by the author’s
observation code. Then it will filter out observations where the
elevation is greater than 250 meters

``` r
obs <- vb_get_plot_observations(
  vb_code = "pj.11044",
  sort    = "author_obs_code",
  limit   = 100
)

# filter where elevation column is greater than 250 meters
obs |>
  dplyr::filter(elevation > 250)
```

    ## # A tibble: 20 × 12
    ##    ob_code   author_obs_code pl_code   author_plot_code has_observation_synonym
    ##    <chr>     <chr>           <chr>     <chr>            <lgl>                  
    ##  1 ob.135454 DEWA.1.2003     pl.136716 DEWA.1           FALSE                  
    ##  2 ob.135422 DEWA.119.2003   pl.136694 DEWA.119         FALSE                  
    ##  3 ob.135421 DEWA.120.2003   pl.136692 DEWA.120         FALSE                  
    ##  4 ob.135591 DEWA.121.2003   pl.136691 DEWA.121         FALSE                  
    ##  5 ob.135592 DEWA.122.2003   pl.136690 DEWA.122         FALSE                  
    ##  6 ob.135443 DEWA.127.2003   pl.136733 DEWA.127         FALSE                  
    ##  7 ob.135442 DEWA.128.2003   pl.136732 DEWA.128         FALSE                  
    ##  8 ob.135432 DEWA.136.2003   pl.136723 DEWA.136         FALSE                  
    ##  9 ob.135430 DEWA.138.2003   pl.136721 DEWA.138         FALSE                  
    ## 10 ob.135357 DEWA.144.2003   pl.136685 DEWA.144         FALSE                  
    ## 11 ob.135449 DEWA.149.2003   pl.136669 DEWA.149         FALSE                  
    ## 12 ob.135383 DEWA.181.2004   pl.136633 DEWA.181         FALSE                  
    ## 13 ob.135382 DEWA.182.2004   pl.136632 DEWA.182         FALSE                  
    ## 14 ob.135381 DEWA.183.2004   pl.136631 DEWA.183         FALSE                  
    ## 15 ob.135380 DEWA.184.2004   pl.136630 DEWA.184         FALSE                  
    ## 16 ob.135377 DEWA.185.2004   pl.136629 DEWA.185         FALSE                  
    ## 17 ob.135447 DEWA.186.2004   pl.136628 DEWA.186         FALSE                  
    ## 18 ob.135376 DEWA.187.2004   pl.136627 DEWA.187         FALSE                  
    ## 19 ob.135375 DEWA.188.2004   pl.136626 DEWA.188         FALSE                  
    ## 20 ob.135373 DEWA.189.2004   pl.136625 DEWA.189         FALSE                  
    ## # ℹ 7 more variables: latitude <dbl>, longitude <dbl>, area <dbl>,
    ## #   elevation <dbl>, country <chr>, state_province <chr>, year <dbl>

### Summarize Numeric Variables

This example downloads the full plot observation records for project
`pj.10510` and computes the mean, minimum and maximum slope gradient
across all plots.

``` r
plot_data <- vb_get_plot_observations(vb_code = "pj.10510", detail = "full")

avg_slope <- plot_data |>
  dplyr::summarise(
    slope_mean = mean(slope_gradient, na.rm = TRUE),
    slope_min  = min(slope_gradient,  na.rm = TRUE),
    slope_max  = max(slope_gradient,  na.rm = TRUE)
  )

head(avg_slope)
```

    ## # A tibble: 1 × 3
    ##   slope_mean slope_min slope_max
    ##        <dbl>     <dbl>     <dbl>
    ## 1       11.1         0        70
