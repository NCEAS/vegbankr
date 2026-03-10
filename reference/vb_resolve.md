# Resolve a VegBank identifier

Queries the VegBank API to resolve a public identifier (such as an
accession code or DOI) to its internal resource details.

## Usage

``` r
vb_resolve(identifier)
```

## Arguments

- identifier:

  A character string specifying the VegBank identifier to resolve. This
  can be an accession code (e.g., "VB.Ob.2948.ACAD143") or other
  supported identifier type.

## Value

A list containing the resolved identifier details with the following
components:

- identifier_value:

  The original identifier value provided

- identifier_type:

  Type of identifier (e.g., "accession_code")

- vb_code:

  VegBank code for the resource

- vb_resource_type:

  VegBank resource type

## Examples

``` r
if (FALSE) { # \dontrun{
# Resolve an accession code
result <- vb_resolve("VB.Ob.2948.ACAD143")
result$vb_code  # "ob.2948"
} # }
```
