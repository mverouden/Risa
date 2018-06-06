## ISATab-class
## get
#' @title 
#' Extract methods 
#'
#' @description 
#' extract slots from an object of class \linkS4class{ISATab}.
#'
#' @name [
#' @aliases [,ISATab-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  f = "[",
  signature = "ISATab",
  definition = function(x, i, j, drop) {
    if (i == "path") return(x@path)
    if (i == "investigation.filename") return(x@investigation.filename)
    if (i == "investigation.file") return(x@investigation.file)
    if (i == "investigation.identifier") return(x@investigation.identifier)
    if (i == "study.identifiers") return(x@study.identifiers)
    if (i == "study.titles") return(x@study.titles)
    if (i == "study.descriptions") return(x@study.descriptions)
    if (i == "study.contacts") return(x@study.contacts)
    if (i == "study.contacts.affiliations") return(x@study.contacts.affiliations)
    if (i == "study.filenames") return(x@study.filenames)
    if (i == "study.files") return(x@study.files)
    if (i == "assay.filenames") return(x@assay.filenames)
    if (i == "assay.filenames.per.study") return(x@assay.filenames.per.study)
    if (i == "assay.files") return(x@assay.files)
    if (i == "assay.names") return(x@assay.names)
    if (i == "assay.files.per.study") return(x@assay.files.per.study)
    if (i == "assay.technology.types") return(x@assay.technology.types)
    if (i == "assay.technology.types.per.study") return(x@assay.technology.types.per.study)
    if (i == "assay.measurement.types") return(x@assay.measurement.types)
    if (i == "assay.measurement.types.per.study") return(x@assay.measurement.types.per.study)
    if (i == "data.filenames") return(x@data.filenames)
    if (i == "samples") return(x@samples)
    if (i == "samples.per.study") return(x@samples.per.study)
    if (i == "samples.per.assay.filename") return(x@samples.per.assay.filename)
    if (i == "assay.filenames.per.sample") return(x@assay.filenames.per.sample)
    if (i == "sample.to.rawdatafile") return(x@sample.to.rawdatafile)
    if (i == "sample.to.assayname") return(x@sample.to.assayname)
    if (i == "rawdatafile.to.sample") return(x@rawdatafile.to.sample)
    if (i == "assayname.to.sample") return(x@assayname.to.sample)
    if (i == "factors") return(x@factors)
    if (i == "treatments") return(x@treatments)
    if (i == "groups") return(x@groups)
    if (i == "assay.tabs") return(x@assay.tabs)
  }) 
## set
#' @name [
#' @aliases [<-,ISATab-method
#' @docType methods
#' @rdname extract-methods
setReplaceMethod(
  f = "[",
  signature = "ISATab",
  definition = function(x, i, j, value) {
    if (i == "path") x@path <- value
    if (i == "investigation.filename") x@investigation.filename <- value
    if (i == "investigation.file") x@investigation.file <- value
    if (i == "investigation.identifier") x@investigation.identifier <- value
    if (i == "study.identifiers") x@study.identifiers <- value
    if (i == "study.titles") x@study.titles <- value
    if (i == "study.descriptions") x@study.descriptions <- value
    if (i == "study.contacts") x@study.contacts <- value
    if (i == "study.contacts.affiliations") x@study.contacts.affiliations <- value
    if (i == "study.filenames") x@study.filenames <- value
    if (i == "study.files") x@study.files <- value
    if (i == "assay.filenames") x@assay.filenames <- value
    if (i == "assay.filenames.per.study") x@assay.filenames.per.study <- value
    if (i == "assay.files") x@assay.files <- value
    if (i == "assay.names") x@assay.names <- value
    if (i == "assay.files.per.study") x@assay.files.per.study <- value
    if (i == "assay.technology.types") x@assay.technology.types <- value
    if (i == "assay.technology.types.per.study") x@assay.technology.types.per.study <- value
    if (i == "assay.measurement.types") x@assay.measurement.types <- value
    if (i == "assay.measurement.types.per.study") x@assay.measurement.types.per.study <- value
    if (i == "data.filenames") x@data.filenames <- value
    if (i == "samples") x@samples <- value
    if (i == "samples.per.study") x@samples.per.study <- value
    if (i == "samples.per.assay.filename") x@samples.per.assay.filename <- value
    if (i == "assay.filenames.per.sample") x@assay.filenames.per.sample <- value
    if (i == "sample.to.rawdatafile") x@sample.to.rawdatafile <- value
    if (i == "sample.to.assayname") x@sample.to.assayname <- value
    if (i == "rawdatafile.to.sample") x@rawdatafile.to.sample <- value
    if (i == "assayname.to.sample") x@assayname.to.sample <- value
    if (i == "factors") x@factors <- value
    if (i == "treatments") x@treatments <- value
    if (i == "groups") x@groups <- value
    if (i == "assay.tabs") x@assay.tabs <- value
    return(x)
  })

## AssayTab-class
## get
#' extract slots from an object of class \linkS4class{AssayTab}.
#'
#' @name [
#' @aliases [,AssayTab-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  f = "[",
  signature = "AssayTab",
  definition = function(x, i, j, drop) {
    if (i == "path") return(x@path)
    if (i == "data.filenames") return(x@data.filenames)
  }) 

## set
#' @name [
#' @aliases [<-,AssayTab-method
#' @docType methods
#' @rdname extract-methods
setReplaceMethod(
  f = "[",
  signature = "AssayTab",
  definition = function(x, i, j, value) {
    if (i == "path") x@path <- value
    if (i == "data.filenames") x@data.filenames <- value
    return(x)
  })

setMethod(
  f = "initialize",
  signature = "ISATab",
  definition = function(.Object, path) {
    ## Assignment of the slots
    .Object["path"] <- path
    ## Parse ISATab files
    d <- dir(path)
    ## Investigation filename, and avoid editor autosave-files
    ifilename <- grep(pattern = paste("^",
                                      isatab.syntax$investigation.prefix,
                                      ".*[a-zA-Z]$",
                                      sep = ""),
                      x = d,
                      value = TRUE,
                      perl = TRUE)
    if (length(ifilename) == 0) {
      stop("Did not find any investigation file at folder ", path)
    } else if (length(ifilename) > 1) {
      stop("Found too many possible investigation files: ", ifilename)
    } else if (!file.exists(file.path(path, ifilename))) {
      stop("Did not find investigation file: ", ifilename)
    }
    .Object["investigation.filename"] <- ifilename
    ## Reading in investigation file into a data frame
    number.columns <- max(count.fields(file = file.path(path, ifilename),
                                       sep = "\t",
                                       quote = "\"",
                                       blank.lines.skip = TRUE,
                                       comment.char = "#"),
                          na.rm = TRUE)
    ifile <- read.table(file = file.path(path, ifilename),
                        sep = "\t",
                        fill = TRUE,
                        na.strings = "", # was "NA",
                        comment.char = "#",
                        blank.lines.skip = TRUE ,
                        stringsAsFactors = FALSE,
                        col.names = paste0("V", seq_len(number.columns)))
    .Object["investigation.file"] <- ifile
    iidentifier <- as.character(
      ifile[grep(pattern = isatab.syntax$investigation.identifier,
                 x = ifile[, 1],
                 useBytes = TRUE), ][2][[1]])
    .Object["investigation.identifier"] <- iidentifier
    ## Study Identifiers  - as a list of strings
    sidentifiers <- ifile[grep(pattern = isatab.syntax$study.identifier,
                               x = ifile[, 1],
                               useBytes = TRUE), ][2][[1]]
    .Object["study.identifiers"] <- as.character(sidentifiers)
    stitles <- ifile[grep(pattern = isatab.syntax$study.title,
                          x = ifile[, 1],
                          useBytes = TRUE), ][2][[1]]
    .Object["study.titles"] <- as.character(stitles)
    sdescriptions <- ifile[grep(pattern = isatab.syntax$study.description,
                                x = ifile[, 1],
                                useBytes = TRUE), ][2][[1]]
    .Object["study.descriptions"] <- as.character(sdescriptions)
    spersonfirstnames <- ifile[grep(pattern = isatab.syntax$study.person.first.name,
                                    x = ifile[, 1],
                                    useBytes = TRUE), -c(1)]
    spersonmidinitials <- ifile[grep(pattern = isatab.syntax$study.person.mid.initial,
                                     x = ifile[, 1],
                                     useBytes = TRUE), -c(1)]
    spersonlastnames <- ifile[grep(pattern = isatab.syntax$study.person.last.name,
                                   x = ifile[, 1],
                                   useBytes = TRUE), -c(1)]
    nospersonfirstnames <- apply(X = !is.na(spersonfirstnames),
                                 MARGIN = 1,
                                 FUN = sum)
    nospersonmidinitials <- apply(X = !is.na(spersonmidinitials),
                                  MARGIN = 1,
                                  FUN = sum)
    nospersonlastnames <- apply(X = !is.na(spersonlastnames),
                                MARGIN = 1,
                                FUN = sum)
    spersonfirstnames[is.na(spersonfirstnames)] <- ""
    spersonmidinitials[is.na(spersonmidinitials)] <- ""
    spersonlastnames[is.na(spersonlastnames)] <- ""
    studyContacts <- matrix(data = NA,
                            nrow = length(sidentifiers),
                            ncol = ncol(ifile) - 1)
    for (i in seq(1:nrow(studyContacts))) {
      maxStudyContacts <- max(nospersonfirstnames[i],
                              nospersonmidinitials[i],
                              nospersonlastnames[i])
      studyContacts[i, c(1:maxStudyContacts)] <- apply(
        X = rbind(spersonfirstnames[i, c(1:maxStudyContacts)],
                  spersonmidinitials[i, c(1:maxStudyContacts)],
                  spersonlastnames[i, c(1:maxStudyContacts)]),
        MARGIN = 2,
        FUN = paste,
        collapse = " ")
      studyContacts[i, ] <- trim(replaceExcess(studyContacts[i, ]))
    }
    rownames(studyContacts) <- sidentifiers
    colnames(studyContacts) <- seq(1:ncol(studyContacts))
    .Object["study.contacts"] <- studyContacts
    spersonaffiliations <- as.matrix(ifile[grep(pattern = isatab.syntax$study.person.affiliation,
                                                x = ifile[, 1],
                                                useBytes = TRUE), -c(1)])
    spersonaffiliations <- trim(replaceExcess(spersonaffiliations))
    rownames(spersonaffiliations) <- sidentifiers
    colnames(spersonaffiliations) <- seq(1:ncol(spersonaffiliations))
    .Object["study.contacts.affiliations"] <- spersonaffiliations
    ## Study filenames (one or more)
    sfilenames <- unlist(
      sapply(X = ifile[grep(pattern = isatab.syntax$study.file.name,
                            x = ifile[, 1],
                            useBytes = TRUE), ],
             FUN = function(i) {
               grep(pattern = isatab.syntax$study.prefix,
                    x = i,
                    value = TRUE,
                    useBytes = TRUE)
             }))
    if (length(sidentifiers) != length(sfilenames)) {
      stop("There are study files with no identifier assigned")
    }
    ## Assign sidentifiers as names of the character vector sfilenames
    names(sfilenames) <- sidentifiers
    .Object["study.filenames"] <- sfilenames
    ## Validation of existance of study files
    if (!all(sapply(X = sfilenames,
                    FUN = function(i) {
                      file.exists(file.path(path, i))
                    }))) {
      stop("Did not find some of the study files: ",
           paste(sfilenames, collapse = ", "),
           ".")
    }
    ## Reading study files into a list of data frames
    sfiles <- lapply(X = sfilenames,
                     FUN = function(i) {
                       read.table(file = file.path(path, i),
                                  header = TRUE,
                                  sep = "\t",
                                  na.strings = "NA",
                                  stringsAsFactors = FALSE,
                                  check.names = FALSE)
                     })
    .Object["study.files"] <- sfiles
    ## List of assay filenames 
    # afilenames is a list with all the assay filenames (without association to studies)
    afilenames <- unlist(
      sapply(X = ifile[grep(pattern = isatab.syntax$study.assay.file.name,
                            x = ifile[, 1],
                            useBytes = TRUE), ],
             FUN = function(i) {
               grep(pattern = isatab.syntax$assay.prefix,
                    x = i,
                    value = TRUE,
                    useBytes = TRUE)
             }))
    names(afilenames) <- NULL
    .Object["assay.filenames"] <- afilenames
    # getting afilenames associated with studies
    afilenames.df <- ifile[grep(pattern = isatab.syntax$study.assay.file.name,
                                x = ifile[, 1],
                                useBytes = TRUE), -c(1)]
    afilenames.list <- split(x = t(unlist(t(afilenames.df))),
                              f = row(x = t(unlist(t(afilenames.df))),
                                      as.factor = TRUE))
    afilenames.per.study <- lapply(X = seq_len(length(afilenames.list)),
                                   FUN = function(i) {
                                     Filter(f = function(j) {
                                       !is.na(j)
                                     },
                                     x = afilenames.list[[i]])
                                   })
    names(afilenames.per.study) <- sidentifiers
    .Object["assay.filenames.per.study"] <- afilenames.per.study
    ## Reading in assay files
    # afiles is a list of data frames (containing all the assay files)
    afiles <- lapply(X = afilenames,
                     FUN = function(i) {
                       read.table(file = file.path(path, i),
                                  sep = "\t",
                                  header = TRUE,
                                  na.strings = "NA",
                                  stringsAsFactors = FALSE,
                                  check.names = FALSE)
                     })
    names(afiles) <- afilenames
    .Object["assay.files"] <- afiles
    # afiles.per.study is a list (one element per study) of lists (one element per assay)
    afiles.per.study <- lapply(
      X = seq_len(length(afilenames.per.study)),
      FUN = function(j) {
        lapply(X = seq_len(length(afilenames.per.study[[j]])),
               FUN = function(i) {
                 read.table(file = file.path(path, afilenames.per.study[[j]][i]),
                            sep = "\t",
                            header = TRUE,
                            na.strings = "NA",
                            stringsAsFactors = FALSE,
                            check.names = FALSE)
               })
      })
    names(afiles.per.study) <- sidentifiers
    for (i in seq_len(length(afiles.per.study))) {
      names(afiles.per.study[[i]]) <- afilenames.per.study[[i]]
    }
    .Object["assay.files.per.study"] <- afiles.per.study
    ## assay.names
    assay.names <- lapply(X = afiles,
                          FUN = function(i) {
                            i[grep(pattern = isatab.syntax$assay.name,
                                   x = colnames(i))]
                          })
    .Object["assay.names"] <- assay.names
    ## Assay technology types
    # data frame with types
    assay.tech.types <- ifile[which(ifile[[1]] == isatab.syntax$study.assay.technology.type), -c(1)]
    # remove empty types - results in a list of types
    assay.tech.types <- na.omit(assay.tech.types[assay.tech.types != ""])
    # strip attributes
    attributes(assay.tech.types) <- NULL
    ## Validate number of assay technology types == number of afiles
    if (length(assay.tech.types) != length(afiles)) {
      message("The number of assay files mismatches the number of assay technology types")
    }    
    .Object["assay.technology.types"] <- assay.tech.types
    ## Assay technology types per study
    assay.tech.types.df <- ifile[which(ifile[[1]] == isatab.syntax$study.assay.technology.type), -c(1)]
    assay.tech.types.list <- split(x = t(unlist(t(assay.tech.types.df))),
                                   f = row(x = t(unlist(t(assay.tech.types.df))),
                                           as.factor = TRUE))
    assay.tech.types.per.study <- lapply(X = seq_len(length(assay.tech.types.list)),
                                   FUN = function(i) {
                                     Filter(
                                       f = function(j) {
                                         !is.na(j)
                                       },
                                       x = assay.tech.types.list[[i]])
                                   })
    names(assay.tech.types.per.study) <- sidentifiers
    .Object["assay.technology.types.per.study"] <- assay.tech.types.per.study
    ## Assay measurement types
    assay.meas.types <- ifile[which(ifile[[1]] == isatab.syntax$study.assay.measurement.type), -c(1)]
    assay.meas.types <- na.omit(assay.meas.types[assay.meas.types != ""])
    attributes(assay.meas.types) <- NULL
    .Object["assay.measurement.types"] <- assay.meas.types
    ## Assay measurement types per study
    assay.meas.types.df <- ifile[which(ifile[[1]] == isatab.syntax$study.assay.measurement.type), -c(1)]
    assay.meas.types.list <- split(x = t(unlist(t(assay.meas.types.df))),
                                   f = row(x = t(unlist(t(assay.meas.types.df))),
                                           as.factor = TRUE))
    assay.meas.types.per.study <- lapply(X = seq_len(length(assay.meas.types.list)),
                                         FUN = function(i) {
                                           Filter(
                                             f = function(j) {
                                               !is.na(j)
                                             },
                                             x = assay.meas.types.list[[i]])
                                         })
    names(assay.meas.types.per.study) <- sidentifiers
    .Object["assay.measurement.types.per.study"] <- assay.meas.types.per.study
    ## Identifying what sample is studied in which assay
    # ## assays is a list of data frames (one for each assay file)
    # assays <- lapply(X = seq_len(length(sfiles)),
    #                  FUN = function(j) {
    #                    lapply(X = seq_len(length(afiles)),
    #                           FUN = function(i) {
    #                             sfiles[[j]]$Sample.Name %in% afiles[[i]]$Sample.Name
    #                           })
    #                  })
    samples <- unique(unlist(lapply(X = sfiles,
                                    FUN = function(i) {
                                      i[, grep(pattern = isatab.syntax$sample.name,
                                               x = colnames(i))]
                                    })))
    .Object["samples"] <- samples
    samples.per.assay.filename <- lapply(X = seq_len(length(afiles)),
                                         FUN = function(i) {
                                           afiles[[i]][[isatab.syntax$sample.name]]
                                         })
    names(samples.per.assay.filename) <- afilenames
    .Object["samples.per.assay.filename"] <- samples.per.assay.filename
    samples.per.study <- lapply(X = seq_len(length(sfiles)),
                                FUN = function(i) {
                                  sfiles[[i]][[isatab.syntax$sample.name]]
                                })
    names(samples.per.study) <- sidentifiers
    .Object["samples.per.study"] <- samples.per.study
    assay.filenames.per.sample <- list()
    for (k in seq_len(length(samples))) {
      assay.filenames.per.sample[[k]] <- vector(mode = "character", length = 0)
      for (j in seq_len(length(afilenames))) {
        if (samples[k] %in% afiles[[j]][[isatab.syntax$sample.name]]) {
          assay.filenames.per.sample[[k]] <- c(assay.filenames.per.sample[[k]],
                                               afilenames[j])
        }
      }
      if (length(assay.filenames.per.sample[[k]]) == 0) {
        assay.filenames.per.sample[[k]] <- NA
      }
    }
    names(assay.filenames.per.sample) <- samples
    if (all(is.na(assay.filenames.per.sample))) {
     message("assay.filenames.per.sample not assigned") 
    } else {
      .Object["assay.filenames.per.sample"] <- assay.filenames.per.sample
    }
    .Object <- setAssayDependentSlots(.Object)
    .Object <- setFactors(.Object)
    .Object <- setTreatments(.Object)
    .Object <- setGroups(.Object)
    .Object <- setAssayTabs(.Object)
    return(.Object)
  })

setGeneric(name = "setAssayFile",
           def = function(.Object, assay.filename, assay.file) {
             standardGeneric("setAssayFile")
           })
setMethod(
  f = "setAssayFile",
  signature = c(.Object = "ISATab",
                assay.filename = "character",
                assay.file = "data.frame"),
  definition = function(.Object, assay.filename, assay.file) {
    .Object["assay.files"][[assay.filename]] <- assay.file
    .Object <- setAssayDependentSlots(.Object)
    return(.Object)
  })

setGeneric(name = "setAssayDependentSlots",
           def = function(.Object) {
             standardGeneric("setAssayDependentSlots")
           })
setMethod(
  f = "setAssayDependentSlots",
  signature = c(.Object = "ISATab"),
  definition =  function(.Object) {
    afiles <- .Object["assay.files"]
    ## List of data filenames with assay filenames as keys
    dfilenames.per.assay <- lapply(X = afiles,
                                   FUN = function(i) {
                                     temp <- as.data.frame(
                                       i[, grep(pattern = isatab.syntax$data.file,
                                                x = colnames(i))])
                                     colnames(temp) <- colnames(i)[
                                       c(grep(pattern = isatab.syntax$data.file,
                                              x = colnames(i)))]
                                     return(temp)
                                   })
    .Object["data.filenames"] <- dfilenames.per.assay
    data.col.names <- lapply(X = seq_len(length(afiles)),
                             FUN = function(i) {
                               if (isatab.syntax$raw.data.file %in% colnames(afiles[[i]])) {
                                 isatab.syntax$raw.data.file
                               } else if (isatab.syntax$free.induction.decay.data.file %in% colnames(afiles[[i]])) {
                                isatab.syntax$free.induction.decay.data.file
                               } else if (isatab.syntax$array.data.file %in% colnames(afiles[[i]])) {
                                isatab.syntax$array.data.file
                               } else if (isatab.syntax$raw.spectral.data.file %in% colnames(afiles[[i]])) {
                                isatab.syntax$raw.spectral.data.file
                               }
                             })
    names(data.col.names) <- names(afiles)
    sample.to.rawdatafile <- lapply(X = seq_len(length(afiles)),
                                    FUN = function(i) {
                                      afiles[[i]][, c(isatab.syntax$sample.name, data.col.names[[i]])]
                                    })
    sample.to.rawdatafile <- lapply(X = seq_len(length(afiles)),
                                    FUN = function(i) {
                                      merge(x = sample.to.rawdatafile[[i]][!duplicated(sample.to.rawdatafile[[i]][[isatab.syntax$sample.name]]), ],
                                            y = sample.to.rawdatafile[[i]][duplicated(sample.to.rawdatafile[[i]][[isatab.syntax$sample.name]]), ],
                                            all = TRUE)
                                    })
    names(sample.to.rawdatafile) <- names(afiles)
    .Object["sample.to.rawdatafile"] <- sample.to.rawdatafile
    sample.to.assayname <- lapply(X = afiles,
                                  FUN = function(i) {
                                    i[, c(isatab.syntax$sample.name,
                                          grep(pattern = isatab.syntax$assay.name,
                                               x = colnames(i),
                                               value = TRUE))]
                                  })
    sample.to.assayname <- lapply(X = seq_len(length(afiles)),
                                  FUN = function(i) {
                                    merge(x = sample.to.assayname[[i]][!duplicated(sample.to.assayname[[i]][[isatab.syntax$sample.name]]), ],
                                          y = sample.to.assayname[[i]][duplicated(sample.to.assayname[[i]][[isatab.syntax$sample.name]]), ],
                                          all = TRUE)
                                  })
    names(sample.to.assayname) <- names(afiles)
    .Object["sample.to.assayname"] <- sample.to.assayname
    rawdatafile.to.sample <- lapply(X = seq_len(length(afiles)),
                                    FUN = function(i) {
                                      afiles[[i]][, c(data.col.names[[i]], isatab.syntax$sample.name)]
                                    })
    rawdatafile.to.sample <- lapply(X = seq_len(length(afiles)),
                                    FUN = function(i) {
                                      merge(x = rawdatafile.to.sample[[i]][!duplicated(rawdatafile.to.sample[[i]][[data.col.names[[i]]]]), ],
                                            y = rawdatafile.to.sample[[i]][duplicated(rawdatafile.to.sample[[i]][[data.col.names[[i]]]]), ],
                                            all = TRUE)
                                    })
    names(rawdatafile.to.sample) <- names(afiles)
    .Object["rawdatafile.to.sample"] <- rawdatafile.to.sample
    assayname.to.sample <- lapply(X = afiles, 
                                  FUN = function(i) {
                                    i[, c(grep(pattern = isatab.syntax$assay.name,
                                               x = colnames(i),
                                               value = TRUE),
                                          isatab.syntax$sample.name)]
                                  })
    assayname.to.sample <- lapply(X = seq_len(length(afiles)),
                                  FUN =  function(i) {
                                    merge(x = assayname.to.sample[[i]][!duplicated(assayname.to.sample[[i]][, c(grep(pattern = isatab.syntax$assay.name,
                                                                                                                     x = colnames(assayname.to.sample[[i]]),
                                                                                                                     value = TRUE))]), ],
                                          y = assayname.to.sample[[i]][duplicated(assayname.to.sample[[i]][, c(grep(pattern = isatab.syntax$assay.name,
                                                                                                                    x = colnames(assayname.to.sample[[i]]),
                                                                                                                    value = TRUE))]), ],
                                          all = TRUE)
                                  })
    names(assayname.to.sample) <- names(afiles)
    .Object["assayname.to.sample"] <- assayname.to.sample
    return(.Object)
  })

setGeneric(name = "setFactors",
           def = function(.Object) {
             standardGeneric("setFactors")
           })
setMethod(
  f = "setFactors",
  signature = c(.Object = "ISATab"),
  definition = function(.Object) {
    study.files <- .Object["study.files"]
    factors.list <- list()
    for (i in seq_len(length(study.files))) {
      if (length(grep(pattern = isatab.syntax$factor.value,
                      x = colnames(study.files[[i]]))) != 0) {
        factor.values  <-  study.files[[i]][grep(pattern = isatab.syntax$factor.value,
                                                 x = colnames(study.files[[i]]))]
        factors.list[[i]] <- lapply(X = factor.values,
                                    FUN = factor)
      } else {
        study.filenames <- .Object["study.filenames"]
        factors.list[[i]] <- list()
        message("No 'Factor Value' column defined in study file ",
                study.filenames[[i]],
                ". Factors slot will be an empty list for that study.")
      }
    }
    names(factors.list) <- names(study.files)
    .Object["factors"] <- factors.list
    return(.Object)
  })

setGeneric(name = "setTreatments",
           def = function(.Object){
             standardGeneric("setTreatments")
           })
setMethod(
  f = "setTreatments",
  signature = c(.Object = "ISATab"),
  definition = function(.Object) {
    factors <- .Object["factors"]
    if (length(factors) == 0) {
      treatments <- list()
      message("Treatments slot will be an empty list")
    } else {
      factors.df.list <- lapply(X = factors,
                                FUN = as.data.frame)
      for (i in seq(factors.df.list)) {
        colnames(factors.df.list[[i]]) <- names(factors[[i]])
      }
      treatments <- lapply(X = factors.df.list,
                           FUN = function(factors.df) {
                             factors.df[!duplicated(factors.df), ]
                           })
    }
    .Object["treatments"] <- treatments
    return(.Object)
  })

setGeneric(name = "setGroups",
           def = function(.Object){standardGeneric("setGroups")})
setMethod(
  f = "setGroups",
  signature = c(.Object = "ISATab"),
  definition = function(.Object) {
    treatments <- .Object["treatments"]
    study.files <- .Object["study.files"]
    samples.per.study <- .Object["samples.per.study"]
    groups <- list()
    if (length(treatments) != 0) {
      for (j in seq_len(length(study.files))) {
        subgroups <- list()
        if (class(treatments[[j]]) == "factor") {
          ## TO BE CHECKED
          ## Will this part ever be executed? By default elements of the treatments
          ## list are data.frames
          for (i in seq_len(length(levels(treatments[[j]])))) {
            treatment <- treatments[[j]][[i]]
            listtr <- rep(treatment, each = length(samples.per.study[[j]]))
            subgroups[[i]] <- samples.per.study[[j]][apply(X = study.files[[j]][names(treatments)[[j]]] == as.data.frame(listtr),
                                                           MARGIN = 1,
                                                           FUN = all)]
            groups[[j]] <- subgroups
          }
        } else {
          n <- nrow(treatments[[j]])
          for (i in seq_len(n)) {
            treatment <- data.frame(treatments[[j]][i, ])
            df <- data.frame(lapply(X = treatment,
                                    FUN = function(x) {
                                      rep(x, each = length(samples.per.study[[j]]))
                                    }))
            subgroups[[i]] <- samples.per.study[[j]][apply(X = study.files[[j]][names(treatments[[j]])] == df,
                                                           MARGIN = 1,
                                                           FUN = all)]
          }
          groups[[j]] <- subgroups
        }
      }
    } else {
      message("Groups slot will be an empty list")
    }
    names(groups) <- names(study.files)
    .Object["groups"] <- groups
    return(.Object)
  })

setGeneric(name = "setAssayTabs",
           def = function(.Object) {
             standardGeneric("setAssayTabs")
           })
setMethod(
  f = "setAssayTabs",
  signature = c(.Object = "ISATab"),
  definition = function(.Object) {
    atabs <- list()
    afiles <- .Object["assay.files"]
    assay.names <- .Object["assay.names"]
    assay.filenames <- .Object["assay.filenames"]
    assay.tech.types <- .Object["assay.technology.types"]
    assay.meas.types <- .Object["assay.measurement.types"]
    data.filenames <- .Object["data.filenames"]
    study.filenames <- .Object["study.filenames"]
    study.identifiers <- .Object["study.identifiers"]
    for (i in seq_len(length(assay.filenames))) {
      k <- getStudyFilenameIndex(isa = .Object,
                                 assay.filename = assay.filenames[i])
      study.filename <- study.filenames[k]
      study.identifier <- study.identifiers[k]
      if (class(data.filenames[[i]]) != "data.frame") {
        data.filenames[[i]] <- as.data.frame(data.filenames[[i]])
        index <- grep(pattern = "Data File",
                      x = colnames(afiles[[i]]))
        colnames(data.filenames[[i]]) <- colnames(afiles[[i]])[index]
      }
      if (assay.tech.types[i] == technology.types$microarray) {
        atabs[[i]] <- new(Class = "MicroarrayAssayTab",
                          path = .Object["path"],
                          study.filename = study.filename,
                          study.identifier = study.identifier,
                          assay.filename = assay.filenames[i],
                          assay.file = afiles[[i]],
                          assay.technology.type = assay.tech.types[i],
                          assay.measurement.type = assay.meas.types[i],
                          assay.names = assay.names[[i]],
                          data.filenames = data.filenames[[i]])
      } else if (assay.tech.types[i] == technology.types$ms) {
        atabs[[i]] <- new(Class = "MSAssayTab",
                          path = .Object["path"],
                          study.filename = study.filename,
                          study.identifier = study.identifier,
                          assay.filename = assay.filenames[i],
                          assay.file = afiles[[i]],
                          assay.technology.type = assay.tech.types[i],
                          assay.measurement.type = assay.meas.types[i],
                          assay.names = assay.names[[i]],
                          data.filenames = data.filenames[[i]])
      } else if (assay.tech.types[i] == technology.types$nmr) {
        atabs[[i]] <- new(Class = "NMRAssayTab",
                          path = .Object["path"],
                          study.filename = study.filename,
                          study.identifier = study.identifier,
                          assay.filename = assay.filenames[i],
                          assay.file = afiles[[i]],
                          assay.technology.type = assay.tech.types[i],
                          assay.measurement.type = assay.meas.types[i],
                          assay.names = assay.names[[i]],
                          data.filenames = data.filenames[[i]])
      } else {
        atabs[[i]] <- new(Class = "AssayTab",
                          path = .Object["path"],
                          study.filename = study.filename,
                          study.identifier = study.identifier,
                          assay.filename = assay.filenames[i],
                          assay.file = afiles[[i]],
                          assay.technology.type = assay.tech.types[i],
                          assay.measurement.type = assay.meas.types[i],
                          assay.names = assay.names[[i]],
                          data.filenames = data.filenames[[i]])
      }
    }
    .Object["assay.tabs"] <- atabs
    return(.Object)
  })

################################################################################
### getRawDataFilenames
################################################################################
### generic method called 'getAssayRawDataFilenames' that dispatches on the type
### of object it's applied to
#' @title Retrieve the raw data filenames in an ISATab dataset object.
#'
#' @description
#' Retrieves the raw data filenames in an ISATab dataset given an object from
#' the \code{\link{ISATab-class}} and a logical value indicating if the 
#' filenames are retrieved with their full path or not.
#'
#' @param .Object an object of class \linkS4class{ISATab}
#' @param full.path a length-one logical vector indicating whether the filenames
#'                  should be retrieved with their full path [default: TRUE],
#'                  or not (FALSE).
#'
#' @return 
#'
#' @export
setGeneric(name = "getRawDataFilenames",
           def = function(.Object, full.path = TRUE) {
             standardGeneric("getRawDataFilenames")
           })
setMethod(
  f = "getRawDataFilenames",
  signature = c(.Object = "ISATab", full.path = "logical"),
  definition = function(.Object, full.path = TRUE) {
    assay.tabs <- .Object["assay.tabs"]
    raw.data.filenames <- sapply(X = assay.tabs,
                                 FUN = function(x) {
                                   getAssayRawDataFilenames(x, full.path)
                                 })
    return(raw.data.filenames)
  })

################################################################################
### getAssayRawDataFilenames
################################################################################
### generic method called 'getAssayRawDataFilenames' that dispatches on the type
### of object it's applied to
#' getAssayRawDataFilenames Generic
#'
#' Generic function to retrieve the raw data files for a particular assay type.
#'
#' @param .Object An object of a defined S4 class (\linkS4class{AssayTab},
#'                \linkS4class{MSAssayTab}, \linkS4class{MicroarrayAssayTab},
#'                \linkS4class{SeqAssayTab}, \linkS4class{NMRAssayTab}).
#' @param full.path a length-one logical vector indicating whether the filenames
#'                  should be retrieved with their full path [default: TRUE],
#'                  or not (FALSE).
#'
#' @export
setGeneric(name = "getAssayRawDataFilenames",
           def = function(.Object, full.path) {
             standardGeneric("getAssayRawDataFilenames")
           })
#' @describeIn getAssayRawDataFilenames Retrieves the raw data filenames for a generic assay.
setMethod(
  f = "getAssayRawDataFilenames",
  signature = c(.Object = "AssayTab",
                full.path = "logical"),
  definition = function(.Object, full.path = TRUE) {
    raw.files <- as.list(.Object["data.filenames"][isatab.syntax$raw.data.file])
    if (full.path) {
      msfiles <- sapply(X = raw.files,
                        FUN = function(x) {
                          sapply(X = x,
                                 FUN = function(y) {
                                   paste(.Object["path"],
                                         y,
                                         sep = .Platform$file.sep)
                                 })
                        })
    }
    return(msfiles)
  })
#' @describeIn getAssayRawDataFilenames Retrieves the raw data filenames for an assay whose technology type is mass spectrometry. These data filenames correspond to those specified in the column 'Raw Spectral Data File'.
setMethod(
  f = "getAssayRawDataFilenames",
  signature = c(.Object = "MSAssayTab",
                full.path = "logical"),
  definition = function(.Object, full.path = TRUE) {
    msfiles <- as.list(.Object["data.filenames"][isatab.syntax$raw.spectral.data.file])
    if (full.path) {
      msfiles <- sapply(X = msfiles,
                        FUN = function(x) {
                          sapply(X = x,
                                 FUN = function(y) {
                                   paste(.Object["path"], y, sep = .Platform$file.sep)
                                 })
                        })
    }
    return(msfiles)
  })
#' @describeIn getAssayRawDataFilenames Retrieves the raw data filenames for an assay whose technology type is DNA microarray.
setMethod(
  f = "getAssayRawDataFilenames",
  signature = c(.Object = "MicroarrayAssayTab",
                full.path = "logical"),
  definition = function(.Object, full.path = TRUE) {
    # if (!isMicroarrayAssay(isa, assay.filename)) {
    #   stop("The ", assay.filename, " is not a microarray assay")
    # }
    microarray.files <- as.list(.Object["data.filenames"][isatab.syntax$array.data.file])
    if (full.path) {
      microarray.files <- sapply(X = microarray.files,
                                 FUN = function(x) {
                                   sapply(X = x,
                                          FUN = function(y) {
                                            paste(.Object["path"], y, sep = .Platform$file.sep)
                                          })
                                 })
    }
    return(microarray.files) 
  })
#' @describeIn getAssayRawDataFilenames Retrieves the raw data filenames for an assay whose technology type is nucleotide sequencing.
setMethod(
  f = "getAssayRawDataFilenames",
  signature = c(.Object = "SeqAssayTab",
                full.path = "logical"),
  definition = function(.Object, full.path = TRUE) {
    msfiles <- as.list(.Object["data.filenames"][isatab.syntax$raw.data.file])
    if (full.path) {
      msfiles <- sapply(X = msfiles,
                        FUN = function(x) {
                          sapply(X = x,
                                 FUN = function(y) {
                                   paste(.Object["path"],
                                         y,
                                         sep = .Platform$file.sep)
                                 })
                        })
    }
    return(msfiles)
  })
#' @describeIn getAssayRawDataFilenames Retrieves the raw data filenames for an assay whose technology type is NMR spectroscopy.
setMethod(
  f = "getAssayRawDataFilenames",
  signature = c(.Object = "NMRAssayTab",
                full.path = "logical"),
  definition = function(.Object, full.path = TRUE) {
    raw.files <- as.list(.Object["data.filenames"][isatab.syntax$free.induction.decay.data.file])
    if (full.path) {
      msfiles <- sapply(X = raw.files,
                        FUN = function(x) {
                          sapply(X = x,
                                 FUN = function(y) {
                                   paste(.Object["path"],
                                         y,
                                         sep = .Platform$file.sep)
                                 })
                        })
    }  
    return(msfiles)
  })

################################################################################
### getAssayFilenames
################################################################################
setGeneric(name = "getAssayNames",
           def = function(.Object, full.path) {
             standardGeneric("getAssayNames")
           })
setMethod(
  f = "getAssayNames",
  signature = c(.Object = "MicroarrayAssayTab",
                full.path = "logical"),
  definition = function(.Object, full.path = TRUE) {
    # assay.filenames <- isa["assay.filenames"]
    # assay.files <- isa["assay.files"]
    # microarray.assay.filenames <- assay.filenames[sapply(X = assay.files,
    #                                                      FUN = function(x) {
    #                                                        isatab.syntax$hybridization.assay.name %in% names(x)
    #                                                      })]
    # return(microarray.assay.filenames)
    assay.filename <- .Object["assay.filename"]
    assay.file <- .Object["assay.file"]
    assay.names <- assay.file[isatab.syntax$hybridization.assay.name]
  })

# getMicroarrayAssayFilenames <- function(isa) {
#   assay.filenames <- isa["assay.filenames"]
#   assay.files <- isa["assay.files"]
#   microarray.assay.filenames <- assay.filenames[sapply(X = assay.files,
#                                                        FUN = function(x) {
#                                                          isatab.syntax$hybridization.assay.name %in% names(x)
#                                                        })]
#   return(microarray.assay.filenames)
# }

################################################################################
### getDerivedDataFilenames
################################################################################
setGeneric(name = "getDerivedDataFilenames",
           def = function(.Object, full.path) {
             standardGeneric("getDerivedDataFilenames")
           })
