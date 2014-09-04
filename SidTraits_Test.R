library(devtools)
install_github("sidtraits", "Koalha")
library(sidtraits)

## Readme 
## https://github.com/ajlyons/sidtraits

species = c("Betula pendula", "Impatiens parviflora", "Carex nigra")
sidseeds(species)

library(dplyr)
library(stringr)
library(XML)

x <- sidseeds("Festuca")
