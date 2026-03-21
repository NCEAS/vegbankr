# Uploading plot data with vegbankr

``` r
library(dplyr)
library(DT)

docs <- read.csv("../inst/loader-table-fields.csv")

docs$required <- factor(docs$required, levels = c("required", "best practice", "commonly used", "sometimes used"))

docs <- docs %>% 
  arrange(required)
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

There are a number of fields that act as codes and that are used as
primary and secondary keys to link loader tables together. Codes that
begin with `user_` are supplied by the data contributor. Codes that
begin with `vb_` are created by the database upon data upload.

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

`user_py_code` is a foreign key to the `parties` table, so any values
present in this field must be present there as well. Optionally, instead
of `user_py_code`, `vb_py_code` can be used if the party is already in
vegbank. One of `user_py_code` or `vb_py_code` must be present, but
having both in the same row is disallowed.

`vb_ar_code` is the vegbank role code - a code in the format `ar.{nn}`.
A table of allowed values and their meanings is listed below the loader
table variables.

Finally, the `contributor_type` indicates whether this contributor is
linked to an Observation, Project, or Classification. The
`record_identifier` value will depend on which of these three types the
contributor is associated with. If the contributor should be associated
with a project with `user_pj_code` `MOJA`, the `record_identifier` for
that contributor should be `MOJA` and the contributor type should be
Project. This transitively will also associate the contributor with all
plots associated with that project. If the contributor should only be
associated with a particular observation with `user_ob_code`
`MOJA_0214`, the record identifier is that observation identifier, and
the `contributor_type` should be Observation.

### Plot Observations

The plot observations loader table contains all data that is consistent
across a plot. This includes information on the plot name, location,
physical features, non-vegetation cover, etc. This table has many
optional fields that may or may not be applicable to your project.

Similar to the pattern described in contributors, one of `vb_pl_code` or
`user_pl_code` is required, and only one of those two fields may be used
for each row. `vb_pl_code` would only be used if the intention is to add
a new observation record to the same plot. These codes are used as
foreign keys in various other tables. `author_plot_code` is often the
same as these codes (e.g., `MOJA_0214`) but there they could be
different for valid reasons. `author_plot_code` is prominently displayed
in the user VegBank interface as the plot identifier.

`user_ob_code` is the primary key for an observation on a plot, and may
be the same as the plot code if there is only one observation of each
plot. If there are multiple observations on the same plot, however,
`user_ob_code` must be unique for each observation.

`user/vb_pj_code` can link a plot and it’s observation back to a
project. Values in these fields must be present either in the `projects`
loader table (`user_pj_code`) or VegBank if the project is already
uploaded (`vb_pj_code`).

### Community Classifications

The community classifications loader table contains the community
classification of an observation.

The primary key is `user_cl_code`. The foreign key `user_ob_code`
corresponds to the key present in the plot observations loader table,
and is required. All values in this field must also be present in the
plot observations table.

`vb_cc_code` is the VegBank community concept identifier, a required
field, the value of which must already be present in VegBank. To
retrieve a list of possible `vb_cc_code` values, use the `vegbankr`
function `vb_get_community_concepts`.

### Strata Cover

This loader table contains data from a plot observation of the plant
names, cover, and strata in a given plot. The `user_ob_code` is a
required foreign key that links the plant to a plot observation. All
values in this field must be present in the plot observations loader
table. `user_tm_code` is a key that is unique for each combination of
`user_ob_code`, plant name, and strata in this table. `user_to_code` is
a key that is unique for each combination of `user_ob_code` and plant
name - so `user_to_code` may be repeated in this table if a plant exists
in multiple strata in a plot observation. `user_sr_code` is a foreign
key that corresponds to the `strata` loader table, described below.

### Strata Methods

Each strata value must also have a VegBank strata method associated with
it. This is represented by `vb_sy_code`. To see available codes, see the
code snippet below the table:

``` r
vb_strata <- vb_get_stratum_methods(with_nested = TRUE) %>% 
  unnest(stratum_types) %>% 
  mutate(stratum_index = tolower(stratum_index)) %>% 
  rename(Stratum = stratum_index)
```

### Taxon Interpretations

Taxon interpretations associates the plants in the strata cover table
with an existing VegBank plant concept code. To get a list of existing
plant concepts, use the `vb_get_plant_concepts` function. Note that a
person with a role is also required for this table, so one of
`user_py_code` (present in the `parties` loader table) or `vb_py_code`
(an existing VegBank party) must exist, along with their role, in
`vb_ar_code`.

### Disturbances

The disturbances loader table contains information about disturbances
observed at a plot, such as fire, grazing, logging, or other events that
have impacted the vegetation.

The primary key is `user_do_code`. The foreign key `user_ob_code` links
each disturbance record to a specific plot observation and is required.

`type` describes the kind of disturbance and is a required field.
`comment` is a best practice field for providing text details about the
disturbance and its impacts. `intensity` describes the degree or
severity of the disturbance, `age` records the estimated time in years
since the disturbance event occurred, and `extent` captures the percent
of the plot that experienced the disturbance event.

### Soils

The Soils loader table is used to describe soils collected from a plot.
This includes information on soil horizons, texture, color, depth, and
chemical properties.

The primary key is `user_so_code`, which can be a simple row number. The
foreign key `user_ob_code` links each soil record to a specific plot
observation when used.

`horizon` is a required field that identifies the soil horizon being
described. `depth_top` and `depth_bottom` define the vertical extent of
each horizon. `color` records soil color following USDA guidelines.

Soil texture can be described using `texture` class and/or by recording
the percent composition. Chemical properties include `organic` matter
content, `ph`, `exchange_capacity`, and `base_saturation`. Methods for
chemical analyses should be documented in the plot observation’s
`methodsNarrative` field.

A `description` field shows additional text details about the soil
characteristics.

### Stem Data

The Stem Data loader table is used to describe individual plant stems
measured at a plot. This table supports detailed tree/shrub demographic
data collection, including stem diameter, height, location, and health
status.

The primary key is `user_sc_code`, which is the stem count identifier.
The required foreign key `user_tm_code` links each stem record to a
specific taxon observation in the strata cover table, associating stems
with their species identification.

`stem_count` is a required field recording the number of stems of a
single species that share the same diameter and height characteristics.
`stem_diameter` records stem diameter in centimeters. When diameter
classes are used, this value represents the class midpoint, with
`stem_diameter_accuracy` storing the offset to the class endpoint.
Similarly, `stem_height` records height in meters, with
`stem_height_accuracy` capturing measurement precision when height
classes are used.

Individual stems can be tracked with `user_sl_code` (stem location
identifier), `stem_code` (field label or tag number), and precise
positions via `stem_x_position` and `stem_y_position` coordinates in
meters relative to the plot origin, with the x-axis defined by the plot
azimuth.

Additional fields include `stem_health` for recording stem condition and
`stem_taxon_area` for expert users to record the sampling area used to
infer species presence.
