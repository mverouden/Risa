Sys.setlocale('LC_ALL', 'C') 

### start isatab.syntax list ----
isatab.syntax <- list(
  investigation.prefix = "i_",
  study.prefix = "s_",
  assay.prefix = "a_",
  investigation.identifier = "Investigation Identifier",
  study.identifier = "Study Identifier",
  study.title = "Study Title",
  study.description = "Study Description",
  study.person.last.name = "Study Person Last Name",
  study.person.first.name = "Study Person First Name",
  study.person.mid.initial = "Study Person Mid Initial",
  study.person.affiliation = "Study Person Affiliation",
  study.file.name = "Study File Name",
  study.assay.file.name = "Study Assay File Name",
  study.assay.technology.type = "Study Assay Technology Type",
  study.assay.measurement.type = "Study Assay Measurement Type",
  sample.name = "Sample Name",
  assay.name = "Assay Name",
  data.file = "Data File",
  raw.data.file = "Raw Data File",
  free.induction.decay.data.file = "Free Induction Decay Data File",
  array.data.file = "Array Data File",
  derived.array.data.file = "Derived Array Data File",
  raw.spectral.data.file = "Raw Spectral Data File",
  hybridization.assay.name = "Hybridization Assay Name",
  factor.name = "Factor Name",
  factor.value = "Factor Value",
  assay.name = "Assay Name")
### end isatab.syntax list ----

### start technology.types list ----
technology.types <- list(
  microarray = "DNA microarray",
  ms = "mass spectrometry",
  fc = "flow cytometry",
  seq = "nucleotide sequencing",
  nmr = "NMR spectroscopy")
### end technology.types list ----

trim <- function(x) {
  gsub(pattern = "^\\s+|\\s+$", replacement = "", x)
}

#' @title
#' Reads an ISAtab dataset into an R object.
#'
#' @description
#' Reads an ISAtab dataset given as a zip file or as a set of files in a specific
#' folder, and builds an object from the ISAtab class.
#'
#' @param path A character vector with the name of the directory in which the
#'             ISAtab files are located (if the parameter zipfile is not provided
#'             or if it is equal to NULL), or the name of the directory where the
#'             zip file will be extracted (if the parameter zipfile is not NULL).
#'             The default value is the current working directory.
#' @param zipfile A character vector with the name of the zip archive containing
#'                ISAtab files. The default value is NULL.
#' @param verbose A logical vector indicating to show messages for the different
#'                steps, if TRUE, or not to show them, if FALSE (the default
#'                value).
#'
#' @return An object from the \code{\link{ISATab-class}}.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com}),
#' Audrey Kauffmann
#'
#' @examples 
#' ## Example for mass spectrometry dataset
#' temp <- tempdir()
#' datafiles <- c(file.path(system.file("cdf/KO", package = "faahKO"),
#'                          grep(pattern = "CDF",
#'                               x = dir(system.file("cdf/KO", package = "faahKO")),
#'                               ignore.case = TRUE,
#'                               value = TRUE)),
#'                file.path(system.file("cdf/WT", package = "faahKO"),
#'                          grep(pattern = "CDF",
#'                               x = dir(system.file("cdf/WT", package = "faahKO")),
#'                               value = TRUE)))
#' file.copy(datafiles, temp, recursive = TRUE)
#' isafiles <- file.path(system.file(package = "faahKO"),
#'                       grep(pattern = "txt",
#'                            x = dir(system.file(package = "faahKO")),
#'                            value = TRUE))
#' file.copy(isafiles, temp, recursive = TRUE)
#' isaObject1 <- readISAtab(path = temp)
#'
#' ## Example of readISAtab for a mass spectrometry experiment
#' isazip <- system.file("extdata", "faahKO-metadata.zip", package = "Risa")
#' isaObject2 <- readISAtab(path = file.path(system.file("extdata",
#'                                                       package = "Risa"),
#'                                                       "ISAexample"),
#'                          zipfile = isazip,
#'                          verbose = TRUE)
#'
#' @keywords ISAtab datasets
#' @export
readISAtab <- function(path = getwd(),
                       zipfile = NULL,
                       verbose = FALSE) {
  if (!is.null(zipfile)) {
    readISAtabZip(zip = zipfile, path, verbose)
  } else {
    readISAtabFiles(path, verbose)
  }
}

### This function only works if the zip file does not contain a directory (but the ISA-TAB files themselves)
readISAtabZip <- function(zip, path = getwd(), verbose=FALSE) {
  if (verbose) {
    base::message("Unzipping file in directory ", path)
  }
  d <- utils::unzip(zipfile = zip, exdir = extract <- path)  
  if (verbose) {
    message("Unzipped files: ", d)
  }
  isaobj <- readISAtabFiles(path) 
  return(isaobj)
}

readISAtabFiles <- function(path = getwd(), verbose=FALSE) {
  if (verbose) {
    message("Converting ISA-Tab dataset at ",path," into R objects...")
  }
  isaobject <- new(Class = "ISATab", path = path)
  if (verbose) {
    message("... done.")
  }
  return(isaobject) 
}

#' @title
#' Update metadata into the assay file.
#' 
#' @description
#' \code{updataAssayMetadata}: Updates metadata into a particular assay file. See
#' an example of use for a metabolite dataset at \url{https://github.com/sneumann/mtbls2}.
#'
#' @param isa An object of the \code{\link{ISATab-class}}.
#' @param assay.filename  A character vector with the filename of an assay.
#' @param col.name A character vector with the name of the column of the assay
#'                 file to be modified.
#' @param values A character vector with the values to be added to the column of
#'               the assay file: it could be a single value, and in this case the
#'               value is repeated across the column, or it could be a list of
#'               values (whose length must match the number of rows of the assay
#'               file).
#'
#' @return An updated object from the \code{\link{ISAtab-class}}.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @examples
#' ### This example shows how to add values to the column "Derived Spectral Data File",
#' ### assuming that the results are stored in the file "faahkoDSDF.txt"
#' faahkoISA <- readISAtab(find.package("faahKO"))
#' assay.filename <- faahkoISA["assay.filenames"][[1]]
#' updateAssayMetadata(isa = faahkoISA,
#'                     assay.filename,
#'                     col.name = "Derived Spectral Data File",
#'                     values = "faahkoDSDF.txt")
#'
#' @seealso \code{\link{readISAtab}}, \url{https://github.com/sneumann/mtbls2}
#' @export
updateAssayMetadata <- function(isa, assay.filename, col.name, values) {
  assay.file <- isa["assay.files"][[assay.filename]]
  if (length(values) == 1) {
    values <- c(rep(values, nrow(assay.file)))
  } else if (length(values) != nrow(assay.file)) {
    stop("Wrong number of values to be added to the assay file")
  }
  ## update column of the assay.file
  assay.file[colnames(assay.file) == col.name] <- values
  ## update the isa object with modified assay.file
  isa <- setAssayFile(isa,assay.filename, assay.file)
  return(isa)
}

#' @title Write an ISA-Tab dataset into files.
#'
#' @description \code{write.ISAtab} writes a ISA-Tab dataset.
#'
#' @param isa An object of the \code{\link{ISATab-class}}.
#' @param path A character vector with the name of the directory to which the
#'             ISAtab files are written. The default value is the current working
#'             directory.
#'
#' @return A set of ISATab files.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#' 
#' @examples
#' ### Example of writing the study file of faahKO ISA-Tab dataset into the temp director
#' data.dir <- system.file("extdata", package = "Risa")
#' isazip <- system.file("extdata", "faahKO-metadata.zip", package = "Risa")
#' faahkoISA <- readISAtab(path = file.path(data.dir, "faahKOISA"),
#'                         zipfile = isazip,
#'                         verbose = TRUE)
#' temp <- tempdir()
#' write.ISAtab(isa = faahkoISA,
#'              paht = temp)
#'
#' @seealso 
#' \code{\link{write.investigation.file}}, \code{\link{write.study.file}},
#' \code{\link{write.assay.file}}, \code{\link{readISAtab}}
#'
#' @export
write.ISAtab <- function(isa, path = getwd()) {
  write.investigation.file(isa, path)
  for (i in seq_len(length(isa["study.filenames"]))) {
    write.study.file(isa, isa["study.filenames"][[i]], path)
  }
  for (i in seq_len(length(isa["assay.filenames"]))) {
    write.assay.file(isa, isa["assay.filenames"][[i]], path)
  }  
}

#' @title Write the Investigation File from an ISA-Tab dataset.
#'
#' @description \code{write.investigation.file} reads an \code{\link{ISATab-class}}
#'              object and writes the investigation part to a file.
#' 
#' @param isa An object of the \code{\link{ISATab-class}}.
#' @param path A character vector with the name of the directory to which the
#'             investigation file is written. The default value is the current
#'             working directory.
#'
#' @return The investigation file of an ISATab set of files.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#' 
#' @examples
#' ### Example of writing the investigation file of faahKO ISA-Tab dataset into
#' ### a temporay directory
#' faahkoISA <- readISAtab(path = find.package("faahKO"))
#' temp <- tempdir()
#' write.investigation.file(isa = faahkoISA, path = temp)
#'
#' @seealso
#' \code{\link{write.ISAtab}}, \code{\link{write.study.file}},
#' \code{\link{write.assay.file}}, \code{\link{readISAtab}}
#'
#' @export
write.investigation.file <- function(isa, path = getwd()) {
  write.table(x = isa["investigation.file"],
              file = file.path(path, isa["investigation.filename"]),
              row.names = FALSE,
              col.names = FALSE, 
              quote = TRUE,
              sep = "\t",
              na = "\"\"")
}

#' @title Write a Study File of an ISA-Tab dataset.
#'
#' @description The \code{write.study.file} function writes a specific Study file
#'              from an \code{\link{ISATab-class}} object into a file on disk.
#'
#' @param isa An object of the \code{\link{ISATab-class}}.
#' @param study.filename A character vector with the name of the study file to
#'                       be written to disk.
#' @param path A character vector with the output path in which the study file
#'             is going to be written, by default the current working directory.
#'
#' @return A study file of an ISATab set of files.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @examples
#' ### Example of writing the study file of faahKO ISA-Tab dataset into a
#' ### temporary directory
#' faahkoISA <- readISAtab(path = find.package("faahKO"))
#' temp <- tempdir()
#' write.study.file(isa = faahkoISA,
#'                  study.filename =  faahkoISA["study.filenames"][[1]],
#'                  path = temp)
#'
#' @seealso
#' \code{\link{write.ISAtab}}, \code{\link{write.investigation.fle}},
#' \code{\link{write.assay.file}} \code{\link{readISAtab}}
#'
#' @export
write.study.file <- function(isa, study.filename, path = getwd()) {
  i <- which(isa["study.filenames"] == study.filename)
  study.file <- isa["study.files"][[i]]
  write.table(x = study.file, 
              file = file.path(path, isa["study.filenames"][[i]]), 
              row.names = FALSE,
              col.names = TRUE,
              quote = TRUE,
              sep = "\t",
              na = "\"\"")
}

#' @title Write an Assay File from the ISA-Tab dataset.
#'
#' @description
#' The \code{write.assay.file} function writes a specific assay file from an
#' \code{\link{ISATab-class}} object into a file.
#'
#' @param isa An object of the \code{\link{ISATab-class}}
#' @param assay.filename A character vector with the name of the assay file to
#'                       be written to disk.
#' @param path A character vector with the output path in which the assay file
#'             is going to be written, by default the current working directory.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @examples
#' ### Example of writing the assay file of the faahKO ISA-Tab dataset into a
#' ### temporary directory
#' faahkoISA <- readISAtab(path = find.package("faahKO"))
#' temp <- tempdir()
#' write.assay.file(isa = faahkoISA,
#'                  assay.filename = faahkoISA["assay.filenames"][[1]],
#'                  path = temp)
#'
#' @seealso
#' \code{\link{write.ISAtab}}, \code{\link{write.investigation.file}},
#' \code{\link{write.study.file}}, \code{\link{readISAtab}}
#'
#' @export
write.assay.file <- function(isa, assay.filename, path = getwd()) {
  i <- which(names(isa["assay.files"]) == assay.filename)
  assay.file <- isa["assay.files"][[assay.filename ]]
  write.table(x = assay.file, 
              file = file.path(path,isa["assay.filenames"][[i]]), 
              row.names = FALSE,
              col.names = TRUE, 
              quote = TRUE,
              sep = "\t",
              na = "\"\"")
}

getStudyFilename <- function(isa, assay.filename) {
  j <- which(lapply(X = isa["assay.filenames.per.study"],
                    FUN = function(x){assay.filename %in% x}) == TRUE)
 return(isa@study.filenames[[j]])
}

getStudyFilenameIndex <- function(isa, assay.filename) {
  j <- which(lapply(X = isa["assay.filenames.per.study"],
                    FUN = function(x){assay.filename %in% x}) == TRUE)
  return(j)
}

### AnnnotatedDataFrame - previous phenoData object
#' @title
#' Get an Annotated Data Frame from an Assay file.
#'
#' @description
#' Retrieves an Annotated Data Frame from an Assay file.
#'
#' @param isa An object of the \code{\link{ISATab-class}.}
#' @param assay.filename  A character vector with the filename of an assay.
#'
#' @return An annotated data frame.
#'
#' @author
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @examples
#' faahkoISAzip <- system.file("extdata", "faahKO-metadata.zip", package = "Risa")
#' faahkoISA <- readISAtab(path = file.path(system.file("extdata", package = "Risa"),
#'                                          "faahkoISA"),
#'                         zipfile = faahkoISAzip,
#'                          verbose =TRUE)
#' faahkoADF <- getAnnotatedDataFrameAssay(faahkoISA, faahkoISA@assay.filenames[[1]])
#' str(faahkoADF)
#'
#' armstrongISAzip <- system.file("extdata", "ARMSTRONG-S-3-metadata.zip", package = "Risa")
#' armstrongISA <- readISAtab(path = file.path(system.file("extdata",
#'                                                         package = "Risa"),
#'                                             "armstrongISA"),
#'                            zipfile = armstrongISAzip,
#'                            verbose = TRUE)
#' armstrongADF <- getAnnotatedDataFrameAssay(isa = armstrongISA,
#'                                            assay.filename = armstrongISA@assay.filenames[[1]])
#' str(armstrongADF)
#'
#' @seealso \code{\link{ISATab-class}}, \code{\link{AnnotatedDataFrame}}
#'
#' @export
getAnnotatedDataFrameAssay <- function(isa, assay.filename) {
  i <- which(names(isa["assay.files"]) == assay.filename)
  dataf <- as.data.frame(isa@factors[[i]])
  colnames(dataf) <-  names(isa@factors[[i]])
  row.names(dataf) <- isa@samples 
  dataf.desc <-  as.data.frame(names(isa@factors[[i]]))
  pdata <- new(Class = "AnnotatedDataFrame",
               data = dataf,
               varMetadata = dataf.desc)
  return(pdata)
}

### Check whether all the files exist
checkFilesExist <- function(files) {
  all(file.exists(files))
}
