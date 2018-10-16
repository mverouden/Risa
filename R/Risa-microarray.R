#' Listing filenames of microarray-based asssay from an ISAtab dataset.
#'
#' Given an object of class \linkS4class{ISATab}, this method retrieves the list
#' of asssay filenames in an ISAtab dataset, which correspond to
#' microarray-based assays.
#'
#' @param isa An object of the \code{\link{ISATab-class}}.
#' 
#' @return a character vector with microarray-based assay filenames.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @seealso \code{\link{isMicroarrayAssay}}
#'
#' @examples
#' armstrongISAzip <- system.file("extdata", "ARMSTRONG-S-3-metadata.zip",
#'                                package = "Risa")
#' armstrongISA <- readISAtab(path = file.path(system.file("extdata",
#'                                                         package = "Risa"),
#'                                             "armstrongISA"),
#'                            zipfile = armstrongISAzip,
#'                            verbose = TRUE)
#' getMicroarrayAssayFilenames(armstrongISA)
#'
#' @export
getMicroarrayAssayFilenames <- function(isa) {
  assay.filenames <- isa["assay.filenames"]
  assay.files <- isa["assay.files"]
  microarray.assay.filenames <- assay.filenames[sapply(X = assay.files,
                                                       FUN = function(x) {
                                                         isatab.syntax$hybridization.assay.name %in% names(x)
                                                       })]
  return(microarray.assay.filenames)
}

#' Determines if an assay is microarray-based or not.
#'
#' Retrieves a boolean value indicating wether the assay filename given as
#' parameter corresponds to a microarray-based assay or not.
#'
#' @param isa an object of class \linkS4class{ISATab}.
#' @param assay.filename A string indicating the name of an assay file. It must
#'                       correspond to a microarray-based assay for the function
#'                       to return a valid value.
#' 
#' @return Retrieves a boolean value.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @seealso \code{\link{getMIAMEMetadata}}
#'
#' @examples
#' data.dir <- system.file("extdata", package = "Risa")
#' isazip <- system.file("extdata", "ARMSTRONG-S-3-metadata.zip",
#'                       package = "Risa")
#' isa <- readISAtab(path = file.path(data.dir, "ARMSTRONG_ISA"),
#'                   zipfile = isazip,
#'                   verbose = TRUE)
#' assay.filename <- isa@assay.filenames[[1]]
#' isMicroarrayAssay(isa, assay.filename)
#'
#' @export
isMicroarrayAssay <- function(isa, assay.filename) {
  microarray.assay.filenames <- getMicroarrayAssayFilenames(isa)
  return(assay.filename %in% microarray.assay.filenames)
}

#' Retrieves the MIAME information about a microarray experiment.
#'
#' If the assay.filename uses the microarray technology, it retrieves an object
#' of the class MIAME with the minimum information about the experiment. Otherwise,
#' it outputs a message indicating that the method is not valid for assays not
#' using the microarray technology.
#'
#' @param isa an object of class \linkS4class{ISATab}.
#' @param assay.filename A string indicating the name of an assay file. It must
#'                       correspond to a microarray-based assay for the function
#'                       to return a valid value.
#' 
#' @return It retrieves an object from the MIAME class or an error message.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @seealso \code{\link{isMicroarrayAssay}}
#'
#' @examples
#' armstrongISAzip <- system.file("extdata", "ARMSTRONG-S-3-metadata.zip",
#'                                package = "Risa")
#' armstrongISA <- readISAtab(path = file.path(system.file("extdata",
#'                                             package = "Risa"),
#'                                             "armstrongISA"),
#'                            zipfile = armstrongISAzip,
#'                            verbose = TRUE)
#' assay.filename <- armstrongISA@assay.filenames[[1]]
#' miame <- getMIAMEMetadata(armstrongISA, assay.filename)
#' str(miame)
#'
#' @export
getMIAMEMetadata <- function(isa, assay.filename) {
  if (isMicroarrayAssay(isa, assay.filename)) {
    ## Determine which study in the investigation contains th MicroarrayAssay
    j <- which(lapply(X = isa["assay.filenames.per.study"],
                      FUN = function(x) {
                        (assay.filename %in% x)
                      }) == TRUE)
    ## Create the MIAME-class object
    my.desc <- new(Class = "MIAME",
                   name = isa@study.identifiers[[j]],
                   lab = isa["study.contacts.affiliations"][isa["study.identifiers"][j], ][!is.na(isa["study.contacts.affiliations"][isa["study.identifiers"][j], ])],
                   contact = isa["study.contacts"][isa["study.identifiers"][j], ][!is.na(isa["study.contacts"][isa["study.identifiers"][j], ])],
                   title = isa["study.titles"][j],
                   abstract = isa["study.descriptions"][j],
                   samples = isa["samples.per.assay.filename"][assay.filename])
    return(my.desc)
  } else {
    message("The assay is not a Microarray assay, so the MIAME metadata cannot be built")
  }
}

getMicroarrayDerivedDataFilenames <- function(isa, full.path = TRUE) {
  microarray.assay.filenames <- getMicroarrayAssayFilenames(isa)
  microarray.files <- lapply(X = isa["data.filenames"][microarray.assay.filenames],
                             FUN = function(x) {
                               x[isatab.syntax$derived.array.data.file]
                             })
  if (full.path) {
    microarray.files <- sapply(X = microarray.files,
                               FUN = function(x) {
                                 sapply(X = x,
                                        FUN = function(y) {
                                          paste(isa["path"],
                                                y,
                                                sep = .Platform$file.sep)
                                        })
                               })
  }
  return(microarray.files)
}

getMicroarrayDerivedDataFilenamesAssay <- function(isa, assay.filename, full.path = TRUE) {
  if (!isMicroarrayAssay(isa, assay.filename)) {
    stop("The ", assay.filename, " is not a microarray assay")
  }
  microarray.files <-  isa["data.filenames"][[assay.filename]][isatab.syntax$derived.array.data.file]
  if (full.path) {
    microarray.files <- sapply(X = microarray.files,
                               FUN = function(x) {
                                 sapply(X = x,
                                        FUN = function(y) {
                                          paste(isa["path"],
                                                y,
                                                sep = .Platform$file.sep)
                                        })
                               })
  }
  return(microarray.files)
}

#' Retrieves an ExpressionSet for a DNA microarray assay.
#'
#' Retrieves an ExpressionSet for a DNA microarray assay.
#'
#' @param isa an object of class \linkS4class{ISATab}.
#' @param assay.filename A character vector with the assay filename of the
#'                       microarray assay.
#' 
#' @return It returns an ExpressionSet.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @references \code{affy}
#' 
#' @seealso \code{\link{ISATab-class}}
#'
#' @examples
#' data.dir <- system.file("extdata",
#'                         package = "Risa")
#' isazip <- system.file("extdata", "ARMSTRONG-S-3-metadata.zip",
#'                       package = "Risa")
#' path <- file.path(data.dir, "ARMSTRONG_ISA")
#' isa <- readISAtab(path = path,
#'                   zipfile = isazip,
#'                   verbose = TRUE)
#' path <- file.path(data.dir, "ARMSTRONG_ISA/")
#' cel.files <- c("GSM510377_SA2006121413.CEL", "GSM510378_SA2006121414.CEL",
#'                "GSM510379_SA2006121415.CEL", "GSM510380_SA2006121416.CEL",
#'                "GSM510381_SA2006121417.CEL", "GSM510382_SA2006121418.CEL",
#'                "GSM510383_SA2006121419.CEL", "GSM510384_SA2006121420.CEL",
#'                "GSM510385_SA2006121421.CEL", "GSM510386_SA2006121427.CEL",
#'                "GSM510387_SA2006121428.CEL", "GSM510388_SA2006121429.CEL",
#'                "GSM510389_SA2006121440.CEL", "GSM510390_SA2006121441.CEL",
#'                "GSM510391_SA2006121442.CEL")
#'
#' \dontrun{
#' 
#' ### Webadress not active anymore!!
#' base.url <- paste("http://perdera.sph.harvard.edu",
#'                   "biidata/microarray",
#'                   "study_ARMSTRONG-S-3_3Zd4KeJNJ7/raw_data/",
#'                   sep = "/")
#'
#' ### download all the cel files
#' ### this is commented out, uncomment the following two lines if you want to build the expression set
#' lapply(X = cel.files,
#'        FUN = function(x) {
#'          download.file(url = paste(base.url, x, sep = ""),
#'                        destfile = paste(path, x, sep = ""))
#'        })
#' getExpressionSet(isa, isa@assay.filenames[[1]])
#' }
#'
#' @export
getExpressionSet <- function(isa, assay.filename) {
  if (!isMicroarrayAssay(isa, assay.filename)) {
    stop("The ", assay.filename, " is not a microarray assay")
  }
# suppressPackageStartupMessages(require("affy"))
  pd <- getAnnotatedDataFrameAssay(isa, assay.filename)
  miame <- getMIAMEMetadata(isa, assay.filename)
  i <- which(names(isa["assay.files"]) == assay.filename)
  cel.files <- getAssayRawDataFilenames(isa@assay.tabs[[i]],
                                        full.path = FALSE)
  fnames <- as.vector(cel.files[[1]])
  current.path <- getwd()
  setwd(isa@path)
  eset <- affy::justRMA(filenames = fnames,
                        phenoData = pd,
                        description = miame)
  setwd(current.path) 
  return(eset)
}
