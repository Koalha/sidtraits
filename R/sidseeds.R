#' Check URLs for binomial names in SID
#'
#' This function checks if Seed Information Database has entries for a given
#' binomial name, and returns the URL if an entry exists.
#' @param sciname A binomial name
#' @param sepa Character separating genus and species names. Defaults to " "
#' @param single Should the result be restricted to length one? Defaults to FALSE
#' @import XML
#' @import stringr


## Later on ... make complete rules to identify different expressions after the
## species name, and use them to restrict the results accurately to the species desired
## Currently, the option 'single' searches for names that have 2 spaces (After genus
## and before author), or with 2 authors.

sidspecurl = function(sciname, sepa = " ", single = FALSE){

Sys.sleep(1)

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

if(single == TRUE){
test = gsub("[(].*[)] ", "", out$species)
out = suppressWarnings(out[sapply(test, str_count, " ") == 2 | (sapply(test, str_detect, "&") == TRUE & sapply(test, str_count, " ") == 4),])
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
Sys.sleep(1)

if(is.na(queryresult$href)){
seed1000 = as.numeric(NA)
} else { script = htmlTreeParse(queryresult$href, useInternalNodes = TRUE)	# end if, begin else
seed1000 = as.numeric(xpathSApply(script, "//div[@id = 'sid']//text()[preceding-sibling::*[1][contains(.,'Average 1000 Seed Weight(g)')]]", xmlValue))
} # end else

out = cbind(queryresult[, 1:2], seed1000)
return(out)
}

#' Extract seed traits from SID
#'
#' This function checks a sidspecurls result and returns traits, if the queried
#' species has them listed in the database.
#' @param queryresult A result from the function sidspecurl or sidspecurls
#' @import dplyr

multiextract = function(queryresult){
dd = queryresult %.%
group_by(1:nrow(queryresult)) %.%
do(traitextract) %.%
rbind_all()
return(dd)
}



#' Extract seed traits from SID using binomial names
#'
#' This function queries the Royal Botanic Gardens Kew Seed Information Database (SID, http://data.kew.org/sid/)
#' for seed traits using plant species binomial names. Currently the function returns recognised names and average
#' 1000 seed masses.
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