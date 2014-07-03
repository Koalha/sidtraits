# sidtraits

`sidtraits` is a package to scrape the Royal Botanical Gardens Kew Seed Information Database (SID, http://data.kew.org/sid/) using plant binomial names.

Currently, the only trait returned is 1000 seed weight, but more will be added soon.

`sidtraits` is still very much a work in progress.

Author: [Konsta Happonen](https://github.com/Koalha/)

### Installing
Install `sidtraits` with devtools:
```
install.packages("devtools")
library(devtools)
install_github("sidtraits", "Koalha")
library(sidtraits)
```
### Functions

`sidtraits` has one function: `sidseeds(sciname, sepa = " ", single = FALSE)`, which scrapes seed trait information from SID.

`sciname` is a vector of one or more plant binomial names, eg. `c("Betula pendula", "Betula pubescens")`.

`sepa` is the separator between genus and species names. For example: `sidseeds(sciname = "Betula_pendula", sepa = "_")`.

`single` is a logical vector indicating, if the results of `sidseeds()` should be restricted to one species per entry in the `sciname`. If `single = FALSE`, `sidseeds()` returns traits for all taxons that partially match the queried binomial name, including variations and subspecies.
`sidseeds()` has some rudimentary rules for identifying main level species with `single = TRUE` (has two spaces OR has '&' and four spaces), but you should always double-check the results.