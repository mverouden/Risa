### Methods to deal with assays whose technology type is mass spectrometry
#' Retrieves a vector with the assay filenames corresponding to mass spectrometry.
#'
#' Given an object of class \linkS4class{ISATab}, this method retrieves a vector
#' with the assay filenames corresponding to mass spectrometry (identified by
#' having a column called 'Raw Spectral Data File').
#'
#' @param isa An object of the \code{\link{ISATab-class}}.
#' 
#' @return A character vector with the assay filenames corresponding to mass
#'         spectrometry.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @examples
#' data.dir <- system.file("extdata", package="Risa")
#' isazip <- system.file("extdata","faahKO-metadata.zip", package="Risa")
#' faahkoISA <- readISAtab(zipfile = isazip, path = file.path(data.dir,"faahKOISA"), verbose =TRUE)
#' ms.assay.filenames <- getMSAssayFilenames(faahkoISA)
#'
#' @export
getMSAssayFilenames <- function(isa) {
  data.filenames <- isa["data.filenames"]
  assay.filenames <- isa["assay.filenames"]
  ms.assay.filenames <- assay.filenames[sapply(X = data.filenames,
                                               FUN = function(x) {
                                                 isatab.syntax$raw.spectral.data.file %in% names(x)
                                               })]
  return(ms.assay.filenames)
}

#' Indicates if an assay filename corresponds to a mass spectrometry assay.
#'
#' Indicates if an assay filename corresponds to a mass spectrometry assay.
#'
#' @param isa an object of class \linkS4class{ISATab}.
#' @param assay.filename the filename of an assay file.
#' 
#' @return It retrieves a boolean value indicating wether the assay is a mass
#'         spectrometry assay or not.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @examples
#' faahkoISA <- readISAtab(find.package("faahKO"))
#' assay.filename <- faahkoISA["assay.filenames"][[1]]
#' isMSAssay(faahkoISA, assay.filename)
#'
#' @export
isMSAssay <- function(isa, assay.filename) {
  ms.assay.filenames <- getMSAssayFilenames(isa)
  return(assay.filename %in% ms.assay.filenames)
}

# ### retrieves a list of the raw data files per assay file from the ISAtab object,
# ### with full path
# getMSRawDataFilenames <- function(isa, full.path = TRUE) {
#   ms.assay.filenames <- getMSAssayFilenames(isa)
#   msfiles <- lapply(X = isa["data.filenames"][ms.assay.filenames],
#                     FUN = function(x) {
#                       x[isatab.syntax$raw.spectral.data.file]
#                     })
#   ## msfiles is a list with one element per assay file, and each element is a
#   ## list with the 'Raw Spectral Data File's
#   if (full.path) {
#     msfiles <- sapply(X = msfiles,
#                       FUN = function(x) {
#                         sapply(X = x,
#                                FUN = function(y) {
#                                  paste(isa["path"],
#                                        y,
#                                        sep = .Platform$file.sep)
#                                })
#                       })
#   }
#   return(msfiles)
# }

# getMSRawDataFilenamesAssay <- function(isa, assay.filename, full.path = TRUE) {
#   msfiles <- lapply(X = isa["data.filenames"][assay.filename],
#                     FUN = function(x) {
#                       x[isatab.syntax$raw.spectral.data.file]
#                     })
#   if (full.path) {
#     msfiles <- sapply(X = msfiles,
#                       FUN = function(x) {
#                         sapply(X = x,
#                                FUN = function(y) {
#                                  paste(isa["path"],
#                                        y,
#                                        sep = .Platform$file.sep)
#                                })
#                       })
#   }
#   return(msfiles)
# }


### specific function to deal with assays whose technology type is mass
### spectrometry using the xcms package it returns an xcmsSet
#' Build an xcmsSet object given a mass spectrometry assay and considering the
#' first factor defined
#'
#' \code{processAssayXcmsSet.1factor} retrieves an xcmsSet object given an object
#' of class \linkS4class{ISATab} and one of its assay filenames.
#'
#' @param isa an object of class \linkS4class{ISATab}, as retrieved by the
#'            function \code{\link{readISAtab}}.
#' @param assay.filename a boolean indicating to show messages for the different
#'                       steps, if TRUE, or not to show them, if FALSE.
#' @param ... extra arguments that can be pass down to the xcmsSet function from
#'            the xcms package.
#'
#' @return the xcmsSet object built from the assay metadata, considering all the
#'         factors defined, and data files.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @seealso \code{\link{readISAtab}}
#' 
#' @examples
#' faahkoISA <- readISAtab(path = find.package("faahKO"),
#'                         verbose = TRUE)
#' assay.filename <- faahkoISA["assay.filenames"][[1]]
#' xset <- processAssayXcmsSet.1factor(isa = faahkoISA,
#'                                     assay.filename = assay.filename)
#'
#' @export
processAssayXcmsSet.1factor <- function(isa, assay.filename, ...) {
  i <- which(isa["assay.filenames"] == assay.filename)
  ## if 'Raw Spectral Data File' is one of the columns in the assay file = it is
  ## a mass spectrometry assay
  if (isatab.syntax$raw.spectral.data.file %in% colnames(isa["data.filenames"][[i]])) {
    ## mass spectrometry files
    msfiles <- isa["data.filenames"][[i]][[isatab.syntax$raw.spectral.data.file]]
    # the assay file as an AnnotatedDataFrame
    pd <- try(Biobase::read.AnnotatedDataFrame(filename = file.path(isa["path"],
                                                                    isa["assay.filenames"][i]),
                                               row.names = NULL,
                                               blank.lines.skip = TRUE,
                                               fill = TRUE,
                                               varMetadata.char = "$",
                                               quote = "\""))
    ## Adding the raw spectral data files as the row names
    sampleNames(pd) = pd$Raw.Spectral.Data.File
    if (length(grep(pattern = isatab.syntax$factor.value,
                    x = colnames(isa["assay.files"][[i]]))) != 0) {
      ## If there are explicit factors, use the first one of them
      sclass <- isa["assay.files"][[i]][which(isa["assay.files"][[i]][[isatab.syntax$sample.name]] %in% pd$Sample.Name),
                                        grep(pattern = isatab.syntax$factor.value,
                                             x = colnames(isa["assay.files"][[i]]))[1]]
      wd <- getwd()
      setwd(isa["path"])
      xset <- xcms::xcmsSet(files = msfiles,
                            sclass = sclass,
                            ...)
      setwd(wd)
    } else {
      wd <- getwd()
      setwd(isa["path"])
      ## Otherwise just use what was there
      xset <- try(xcms::xcmsSet(files = msfiles,
                                phenoData = pData(pd),
                                ...))
      setwd(wd)
    }
    return(xset)
  }# end if
}# end processAssayXcmsSet.1factor function

### specific function to deal with assays whose technology type is mass
### spectrometry using the xcms package it returns an xcmsSet, 
#' Build an xcmsSet object given a mass spectrometry assay and considering all
#' factors defined.
#'
#' \code{processAssayXcmsSet} retrieves an xcmsSet object given an ISA-tab
#' object and one of its assay filenames.
#'
#' @param isa an object of class \linkS4class{ISATab}, as retrieved by the
#'            function \code{\link{readISAtab}}.
#' @param assay.filename a boolean indicating to show messages for the different
#'                       steps, if TRUE, or not to show them, if FALSE.
#' @param ... extra arguments that can be pass down to the xcmsSet function from
#'            the xcms package.
#'
#' @return the xcmsSet object built from the assay metadata, considering all the
#'         factors defined, and data files.
#'
#' @author Steffen Neumann, 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @seealso \code{\link{readISAtab}}
#' 
#' @examples
#' faahkoISA <- readISAtab(path = find.package("faahKO"),
#'                         verbose = TRUE)
#' assay.filename <- faahkoISA["assay.filenames"][[1]]
#' xset <- processAssayXcmsSet(isa = faahkoISA,
#'                             assay.filename = assay.filename)
#'
#' @export
processAssayXcmsSet <- function(isa, assay.filename, ...) {
  i <- which(isa["assay.filenames"] == assay.filename)
  if (isatab.syntax$raw.spectral.data.file %in% colnames(isa["data.filenames"][[i]])) {
    ## mass spectrometry files
    msfiles <- isa["data.filenames"][[i]][[isatab.syntax$raw.spectral.data.file]]
    pd <- try(Biobase::read.AnnotatedDataFrame(filename = file.path(isa["path"], isa["assay.filenames"][i]),
                                               row.names = NULL,
                                               blank.lines.skip = TRUE,
                                               fill = TRUE,
                                               varMetadata.char = "$",
                                               quote = "\""))
    sampleNames(pd) <- pd$Raw.Spectral.Data.File
    if (length(grep(pattern = isatab.syntax$factor.value,
                    x = colnames(isa["assay.files"][[i]]))) != 0) {
      ## If there are explicit factors, use them
      sclass <- as.data.frame(isa["factors"])
      wd <- getwd()
      setwd(isa["path"])
      xset <- xcms::xcmsSet(files = msfiles,
                            sclass = sclass,
                            ...)
      setwd(wd)
    } else {
      wd <- getwd()
      setwd(isa["path"])
      ## Otherwise just use what was there
      xset <- try(xcms::xcmsSet(files = msfiles,
                                phenoData = pData(pd),
                                ...))
      setwd(wd)
    }
    return(xset)
  }# end if statement
}# end processAssayXcmsSet function
