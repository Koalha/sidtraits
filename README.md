
# sidtraits

`sidtraits` is a package to scrape the Royal Botanical Gardens Kew's Seed Information Database (SID, http://data.kew.org/sid/) using plant binomial names.

Currently `sidtraits` fetches information related to seed weight and dispersal.

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

With `single = TRUE` it will return only one result per binomial name queried. In this case all subspecies and variations are ignored unless there is no seed trait information for a higher taxon.

### Examples

A normal call returns a data frame for all matches to the specified species.


```r
species = c("Betula pendula", "Impatiens parviflora", "Carex nigra")
sidseeds(species)
```

```
##                   call                 speciesname seed1000 zoochory
## 1       Betula pendula Betula pendula var. pendula     0.49       NA
## 2       Betula pendula         Betula pendula Roth     0.27       NA
## 3 Impatiens parviflora    Impatiens parviflora DC.     8.00       NA
## 4          Carex nigra   Carex nigra (L.) Reichard     0.69       NA
##   anemochory hydrochory autochory barochory atelochory other_dispersal
## 1         NA         NA        NA        NA         NA              NA
## 2          1         NA        NA        NA         NA              NA
## 3         NA         NA         1        NA         NA              NA
## 4         NA         NA        NA        NA         NA              NA
##   desc_dispersal
## 1             NA
## 2   Wind; Di....
## 3   Methods ....
## 4             NA
```

The dispersal categories are Principal Dispersal Agents (PDAs). If the PDA is other than animal, wind, water, self-assisted, unassisted or prevented dispersal (for example a combination of some of the above), it is marked as `other_disperal`. `desc_dispersal` is a list of Kew's description of the PDA's, such as which animal disperses a zoochorous plant's seeds. It also lists references to literature. For more detailed info, visit [Kew's webpage](http://data.kew.org/sid/dispersal.html).

Specifying `single = TRUE` tries to skip all subspecies and variations, and returns only one result per species queried.


```r
result = sidseeds(species, single = TRUE)
result
```

```
##                   call               speciesname seed1000 zoochory
## 1       Betula pendula       Betula pendula Roth     0.27       NA
## 2 Impatiens parviflora  Impatiens parviflora DC.     8.00       NA
## 3          Carex nigra Carex nigra (L.) Reichard     0.69       NA
##   anemochory hydrochory autochory barochory atelochory other_dispersal
## 1          1         NA        NA        NA         NA              NA
## 2         NA         NA         1        NA         NA              NA
## 3         NA         NA        NA        NA         NA              NA
##   desc_dispersal
## 1   Wind; Di....
## 2   Methods ....
## 3             NA
```

```r
result$desc_dispersal[2]
```

```
## [[1]]
## [1] "Methods originating from parent plant or diaspore; Explosive mechanism; Direct or experimental observation; (Kozlowski, 1972); Diaspore=seed. Seeds are expelled from the fruit via a turgor mechanism."
## [2] "Methods originating from parent plant or diaspore; Method not stated; (Bouman et al., 2000)"
```

The second species *Impatiens parviflora* has 2 sources for its PDA. These are included in a list in the column `desc_dispersal`
