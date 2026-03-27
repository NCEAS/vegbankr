# Changelog

## vegbankr 1.0.0

Production release of the VegBank API client, version 1.0.0 on
2026-03-26.

New features:

- write validation function
  [\#78](https://github.com/NCEAS/vegbankr/issues/78)
- Support new “minimal” detail when getting various entities
  [\#88](https://github.com/NCEAS/vegbankr/issues/88)
- Document and test updated search behavior
  [\#90](https://github.com/NCEAS/vegbankr/issues/90)
- Fully support the VegBank 2.0.0 write API
  [\#92](https://github.com/NCEAS/vegbankr/issues/92)
- Add functions for remaining VegBank upload endpoints
  [\#93](https://github.com/NCEAS/vegbankr/issues/93)
- Support creation of user datasets
  [\#94](https://github.com/NCEAS/vegbankr/issues/94)
- Add user-friendly upload data validation
  [\#79](https://github.com/NCEAS/vegbankr/issues/79)
- Support status parameter in concept/observation getters
  [\#99](https://github.com/NCEAS/vegbankr/issues/99)
- make upload output more easily understandable without lots of
  scrolling [\#100](https://github.com/NCEAS/vegbankr/issues/100)
- Add upload sanity tests that can be run locally
  [\#102](https://github.com/NCEAS/vegbankr/issues/102)

Bug fixes:

- logical bug when vb_refresh_tokens() fails to get a token
  [\#81](https://github.com/NCEAS/vegbankr/issues/81)

## vegbankr 0.9.0

First production release of the VegBank API client, version 0.9.0 on
2026-03-20.

- Initial API development release
- Complete coverage of read API endpoints
- Nearly complete coverage of write API endpoints
