## citation2bib.R
## Date: May 12, 2017
## Author: Jason Karl
## Description:
##   Function to write out a BibTex file of the citations for a list of R packages, and optionally, R itself.
##   The BibTex file can be imported into a reference manager app like Mendeley, Zotero, or EndNote, usually 
##   by just double-clicking the output file. Presents an easy way to get the official citations for R and/or
##   R packages for papers/reports.
##
## Usage: citation2bib(pkg.list="",core=TRUE,outfile="PackageList.bib")
##
## Arguments: 
##    pkg.list - list. A list of names of packages. If a package is not found by the name given, it is skipped.
##    core - boolean. True/False, should the citation for Base R be included?
##    outfile - character string. Name of the output BibTex file to write. If no path included, defaults to the current
##              working directory.
##
## Example: citation2bib(c("rgeos","ggplot2","raster","dplyr"), core=T, outfile="PackageCitations.bib")
##
##########################################################################################################################

citation2bib <- function(pkg.list="",core=T,outfile="PackageList.bib") {
  bib.list <- list()
  for (pkg in pkg.list) {
    pkg.cit <- tryCatch(citation(pkg), error=function(e) e)
    if (length(pkg.cit)==2){ # denotes an error
      print(paste("No package found called",pkg))
      next
    }
    bib.list[[length(bib.list)+1]] <- toBibtex(pkg.cit)
  }
  if (core==T) bib.list[[length(bib.list)+1]] <- toBibtex(citation())
  
  f <- file(outfile,"w")
  cat(unlist(bib.list),file=f,sep="\n")
  close(f)
  return(bib.list)
}