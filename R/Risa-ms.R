### Methods to deal with assays whose technology type is mass spectrometry 
getMSAssayFilenames <- function(isa) {
  data.filenames <- isa["data.filenames"]
  assay.filenames <- isa["assay.filenames"]
  ms.assay.filenames <- assay.filenames[sapply(X = data.filenames,
                                               FUN = function(x) {
                                                 Risa:::isatab.syntax$raw.spectral.data.file %in% names(x)
                                               })]
  return(ms.assay.filenames)
}
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
#                       x[Risa:::isatab.syntax$raw.spectral.data.file]
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
processAssayXcmsSet.1factor <- function(isa, assay.filename, ...) {
  i <- which(isa["assay.filenames"] == assay.filename)
  ## if 'Raw Spectral Data File' is one of the columns in the assay file = it is
  ## a mass spectrometry assay
  if (Risa:::isatab.syntax$raw.spectral.data.file %in% colnames(isa["data.filenames"][[i]])) {
    ## mass spectrometry files
    msfiles <- isa["data.filenames"][[i]][[Risa:::isatab.syntax$raw.spectral.data.file]]
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
    if (length(grep(pattern = Risa:::isatab.syntax$factor.value,
                    x = colnames(isa["assay.files"][[i]]))) != 0) {
      ## If there are explicit factors, use the first one of them
      sclass <- isa["assay.files"][[i]][which(isa["assay.files"][[i]][[Risa:::isatab.syntax$sample.name]] %in% pd$Sample.Name),
                                        grep(pattern = Risa:::isatab.syntax$factor.value,
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
processAssayXcmsSet <- function(isa, assay.filename, ...) {
  i <- which(isa["assay.filenames"] == assay.filename)
  if (Risa:::isatab.syntax$raw.spectral.data.file %in% colnames(isa["data.filenames"][[i]])) {
    ## mass spectrometry files
    msfiles <- isa["data.filenames"][[i]][[Risa:::isatab.syntax$raw.spectral.data.file]]
    pd <- try(Biobase::read.AnnotatedDataFrame(filename = file.path(isa["path"], isa["assay.filenames"][i]),
                                               row.names = NULL,
                                               blank.lines.skip = TRUE,
                                               fill = TRUE,
                                               varMetadata.char = "$",
                                               quote = "\""))
    sampleNames(pd) <- pd$Raw.Spectral.Data.File
    if (length(grep(pattern = Risa:::isatab.syntax$factor.value,
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
