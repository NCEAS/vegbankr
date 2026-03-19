# Uploading plot data with vegbankr

``` r
library(dplyr)
library(DT)

docs <- read.csv("../inst/loader-table-fields.csv")
```

## Introduction to `vegbankr`

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

To use `vegbankr` to upload data, there are 3 key steps:

1.  Model and transform your data to the vegbank loader table format
2.  Validate your data
3.  Upload your data using `vb_upload_plot_observations(...)`

This vignette will walk through these 3 steps, with an emphasis on
modeling and validating data.

## Vegbank Loader Tables

Loader tables are the data format that is used to upload data into
VegBank. In order to publish your data to VegBank, the first step is to
model whatever format your data is in to the loader table format, and
then transform the data into that format. Modeling your data means
identifying how each piece of information in your original dataset (like
species names, plot locations, survey dates) corresponds to specific
fields in the VegBank loader table format. There are a total of 12
loader tables that can be used for plot observation data, though not all
are required for data ingest. In this section, the loader tables and
their fields will be described, in the order in which it is recommended
to prepare them.

Each loader table you create is an R `data.frame` that is eventually
passed to a VegBank upload function. In the documentation below,
interactive tables display the allowed `field` (column names), whether
the field is required, best practice, commonly used, or sometimes used,
and a description of the field.

There are number of fields that act as codes which are used primary and
secondary keys to link loader tables together. Codes that begin with
`user_` are supplied by the data uploader. Codes that begin with `vb_`
are created by the database upon data upload.

### Projects

This table stores information about a project established to collect
vegetation plot data. The `user_pj_code` is the project code primary
key, and is found as a foreign key in several other tables. An example
project code might be `MOJA` with project name “Mojave Desert Vegetation
Surveys.”

### Parties

The Parties loader table is used to upload new parties (people)
associated with plots, projects, taxa, and classifications. The primary
key is `user_py_code` which is used as a foreign key in the Contributors
loader table. Once uploaded, VegBank will create a `vb_py_code` for each
party to be used in the Contributors table.

### Contributors

The contributors loader table is fairly code heavy, but is closely
linked to both the Projects table and the Parties table, and is used to
link parties (people) with their contributions to plots, projects, taxa,
and classifications.

### Plot Observations

The Plot Observations loader table contains all data that is consistent
across a plot. This includes information on the plot name, location,
physical features, non-vegetation cover, etc. This table has many
optional fields that may or may not be applicable to your project.

### Community Classifications

### Strat Cover

### Strata

### Taxon Interpretations

### Distrubances

### Soils

The Soils loader table is used to describe soils collected from a plot.
This includes information on soil horizons, texture, and chemical
properties.

### Stem Data

The Stem Data loader table is used to describe individual plant stems
measured at a plot.

### References
