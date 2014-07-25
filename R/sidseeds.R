#' Check URLs for binomial names in SID
#'
#' This function checks if Seed Information Database has entries for a given
#' binomial name, and returns the URL if an entry exists.
#' @param sciname A binomial name
#' @param sepa Character separating genus and species names. Defaults to " "
#' @param single Should the result be restricted to length one? Defaults to FALSE
#' @import XML
#' @import stringr

sidspecurl = function(sciname, sepa = " ", single = FALSE){

Sys.sleep(0.5)

old.options = options()
on.exit(options(old.options))
options(stringsAsFactors = FALSE)

species = strsplit(sciname, sepa)
url = paste0("http://data.kew.org/sid/SidServlet?Clade=&Order=&Family=&APG=off&Genus=", species[[1]][1], "&Species=", species[[1]][2], "&StorBehav=0")
script = htmlTreeParse(url, useInternalNodes = TRUE)

# if species is found
if(length(xpathSApply(script, paste0("//div[@id = 'sid']//a[contains(.,'", gsub(sepa, " ", sciname), "')]"), xmlValue)) != 0) {

out = data.frame(
call = sciname,
speciesname = xpathSApply(script, paste0("//div[@id = 'sid']//a[contains(.,'", gsub(sepa, " ", sciname), "')]"), xmlValue),
href = paste0("http://data.kew.org/sid/", xpathSApply(script, paste0("//div[@id = 'sid']//a[contains(.,'", gsub(sepa, " ", sciname), "')]"), xmlAttrs))
)

if(single == TRUE) {
    out = single_me(out)
}
} else {	# end if species is found, begin if species not found
out = data.frame(
call = sciname,
speciesname = as.character(NA),
href = as.character(NA)
)
}		# end if species not found
rownames(out) = NULL
return(out)
}



#' Check URLs for binomial names in SID
#'
#' This function checks if Seed Information Database has entries for a given
#' vector of binomial names, and returns the URLs for any existing entries.

#' @param sciname A vector of binomial names
#' @param sepa Character separating genus and species names. Defaults to " "
#' @param single Should the resulting data frame have only one row for each name queried? Defaults to FALSE

sidspecurls = function(sciname, sepa = " ", single = FALSE){
out = do.call(rbind, lapply(sciname,function(x){sidspecurl(x, sepa, single)}))
row.names(out) = NULL
return(out)
}

#' Extract seed traits from SID
#'
#' This function checks a sidspecurl result and returns traits, if the queried
#' species has them listed in the database. Intended for use with multiextract.
#' @param queryresult A single row result from the function sidspecurl or sidspecurls
#' @import XML

traitextract = function(queryresult){
Sys.sleep(0.5)

seed1000 = numeric(1)
zoochory = numeric(1)
anemochory = numeric(1)
hydrochory = numeric(1)
autochory = numeric(1)
barochory = numeric(1)
atelochory = numeric(1)
other_dispersal = numeric(1)
desc_dispersal = character(1)

if(is.na(queryresult$href)){
seed1000 = as.numeric(NA)
zoochory = as.numeric(NA)
anemochory = as.numeric(NA)
hydrochory = as.numeric(NA)
autochory = as.numeric(NA)
barochory = as.numeric(NA)
atelochory = as.numeric(NA)
other_dispersal = as.numeric(NA)
desc_dispersal = as.character(NA)
} else { script = htmlTreeParse(queryresult$href, useInternalNodes = TRUE)	# end if, begin else
seed1000 = as.numeric(xpathSApply(script, "//div[@id = 'sid']//text()[preceding-sibling::*[1][contains(.,'Average 1000 Seed Weight(g)')]]", xmlValue))
dispersal = unique(xpathSApply(script, "//ol[preceding-sibling::span[1][child::b[contains(.,'Seed Dispersal')]]]/li/b", xmlValue))

         disptest = function(input,output){
             ifelse(any(dispersal == input),
                    assign(output, 1, inherits = TRUE),
                    assign(output, as.numeric(NA), inherits = TRUE))
         }

         disptest("Animal", "zoochory")
         disptest("Wind", "anemochory")
         disptest("Water", "hydrochory")
         disptest("Methods originating from parent plant or diaspore", "autochory")
         disptest("Unassisted", "barochory")
         disptest("Dispersal prevented", "atelochory")

         if(length(dispersal) > 0){
             ifelse(!(any(dispersal %in% c("Animal", "Wind", "Water", "Methods originating from parent plant or diaspore", "Unassisted", "Dispersal prevented"))),
                    assign("other_dispersal", 1, inherits = TRUE),
                    assign("other_dispersal", as.numeric(NA), inherits = TRUE))
         } else {assign("other_dispersal", as.numeric(NA), inherits = TRUE)}

desc_dispersal = xpathSApply(script, "//ol[preceding-sibling::span[1][child::b[contains(.,'Seed Dispersal')]]]/li", xmlValue)
     } # end else(that means url was found)
if(length(seed1000) == 0){seed1000 = as.numeric(NA)}
# Dispersal is covered in the former code block
if(length(desc_dispersal) == 0){desc_dispersal = as.character(NA)}

out = cbind(queryresult[1:2], seed1000, zoochory, anemochory, hydrochory, autochory, barochory, atelochory, other_dispersal)
out$desc_dispersal = I(list(desc_dispersal)) # injecting lists to a data frame requires this
return(out)
}



#' Extract seed traits from SID
#'
#' This function checks a sidspecurls result and returns traits, if the queried
#' species has them listed in the database.
#' @param queryresult A result from the function sidspecurl or sidspecurls
#' @import dplyr

multiextract = function(queryresult){
dd = queryresult %>%
    group_by(1:n()) %>%
    do(traits = traitextract(.))

dd = do.call(rbind,dd$traits)

return(dd)
}



#' Extract seed traits from SID using binomial names
#'
#' This function queries the Royal Botanic Gardens Kew Seed Information Database (SID, http://data.kew.org/sid/)
#' for seed traits using plant species binomial names. Currently the function returns recognised names, average
#' 1000 seed masses and principal dispersal agents.
#' @param sciname A vector of one or more binomial names
#' @param sepa Character separating genus and species names. Defaults to " "
#' @param single Should the result be restricted to one species / queried name? Defaults to FALSE
#' @examples
#' sidseeds("Betula pendula")
#' sidseeds("Betula pendula", single = TRUE)
#' sidseeds("Betula_pendula", sepa = "_")
#' sidseeds(c("Betula pendula", "Anemone nemorosa"))
#' @export

sidseeds = function(sciname, sepa = " ", single = FALSE){
urls = sidspecurls(sciname, sepa, single)
result = multiextract(urls)
return(result)
}

#' Functionality for the option single = TRUE
#'
#' This function is responsible for the proper functioning of single = TRUE
#'
#' @param x A sidspecurl result
#' @import dplyr

single_me = function(x){
midtest1 = x[!str_detect(x$speciesname, "( var. )|( subsp. )"),] # remove rows that are variations or subspecies
if(NROW(midtest1) == 0){midtest1 = x} # If all are var.s or subsp.s, revert
if(NROW(midtest1) == 1){
    return(midtest1)
} else {
    warning(paste0("More than one entry for ",x$call[1], " found with single = TRUE, using the first one\n Recommend using single = FALSE"))
    return(midtest1[1,])
}
}
