#' @title
#' S4 Class ISATab
#'
#' @description
#' An S4 class to store information from an ISATab dataset, including an
#' investigation file, one or more study files, and one or more assay files for
#' each study file.
#'
#' @slot path A length-one character vector containing the path to the ISA-Tab
#'            dataset.
#' @slot investigation.filename A length-one character vector containing the
#'                              investigation filename (by definition starting
#'                              with "i_").
#' @slot investigation.file A data frame with the contents of the investigation
#'                          file.
#' @slot investigation.identifier A length-one character vector containing an
#'                                identifier or an accession number for the
#'                                investigation provided by a repository (SHOULD
#'                                be locally unique).
#' @slot study.identifiers A character vector whose elements are the study
#'                         identifiers.
#' @slot study.titles A character vector whose elements are the study titles.
#' @slot study.descriptions A character vector whose elements are the study
#'                          descriptions. 
#' @slot study.contacts A character matrix where each row contains the study
#'                      contacts for a specific study. The rowname specifies to
#'                      which study the contacts belong.
#' @slot study.contacts.affiliations A character matrix where each row contains
#'                                   the study contacts affiliations for a
#'                                   specific study. The rowname specifies to
#'                                   which study the contacts affiliations belong.
#' @slot study.filenames A named character vector whose elements are the names of
#'                       the study files. The vector element names are specified
#'                       as the study identifiers.
#' @slot study.files A list object of data frames with the contents of the study
#'                   files.
#' @slot assay.filenames A character vector whose elements list the names of the 
#'                       assay files.
#' @slot assay.filenames.per.study A list object of assay filenames for each
#'                                 study in the investigation.
#' @slot assay.files A list object of data frames with the contents of the assay
#'                   files.
#' @slot assay.files.per.study A list object  of data frames containing assay
#'                             files contents for each study in the investigation.
#' @slot assay.names A list object of data frames, containing, for each assay file,
#'                   the column of the assay file corresponding to the "Assay
#'                   Name".
#' @slot assay.technology.types A character vector whose elements are the
#'                              technology types for each assay.
#' @slot assay.technology.types.per.study A list object of assay technology types
#'                                        for each study in the investigation.
#' @slot assay.measurement.types A character vector whose elements list the
#'                               measurement types for each assay.
#' @slot assay.measurement.types.per.study A list object of assay measurement
#'                                         types for each study in the investigation.
#' @slot data.filenames A list object of data frames, with each data frame
#'                      containing the names of the data files for each sample
#'                      in an assay.
#' @slot samples A character vector whose elements list the names of all samples.
#' @slot samples.per.study A list object of character vectors, where the vectors
#'                         contain the samples names for each study in the
#'                         investigation.
#' @slot samples.per.assay.filename A list object of character vectors, where the
#'                                  vectors contain the samples names for each
#'                                  assay in the investigation.
#' @slot assay.filenames.per.sample A list object with the length of the number
#'                                  of samples used in an investigation, where
#'                                  each element is a character vector of assay
#'                                  filenames in which that particular sample
#'                                  has been used.
#' @slot sample.to.rawdatafile  A list object of data frames with samples
#'                              associated with raw data files.
#' @slot sample.to.assayname A list object which maintains the association
#'                           between samples and assay names.
#' @slot rawdatafile.to.sample A list object which mantains the association
#'                             between raw data file and samples.
#' @slot assayname.to.sample A list object mantaining the associations between
#'                           assay names and samples.
#' @slot factors A list object containing R factor objects for each of the studies
#'               (with factor names and factor levels).
#' @slot treatments A list object of  data frames with the combination of factor
#'                  values, per study file, or a factor object, if there is a
#'                  single study.
#' @slot groups A list object with one element per study file, and each element
#'              is a list object of the samples for the corresponding treatment.
#' @slot assay.tabs A list object, where each element is an object of the class
#'                  AssayTab or its specialisations (MSAssayTab or
#'                  MicroarrayAssayTab), representing the information of each of
#'                  the assay files in the investigation.
#'
#' @export
ISATab <- setClass(Class = "ISATab",
                   slots = c(
                     path = "character",
                     investigation.filename = "character",
                     investigation.file = "data.frame",
                     investigation.identifier = "character",
                     study.identifiers = "character",
                     study.titles = "character",
                     study.descriptions = "character",
                     study.contacts = "matrix",
                     study.contacts.affiliations = "matrix",
                     study.filenames = "character",
                     study.files = "list",
                     assay.filenames = "character",
                     assay.filenames.per.study = "list",
                     assay.files = "list",
                     assay.files.per.study = "list",
                     assay.names = "list",
                     assay.technology.types = "character",
                     assay.technology.types.per.study = "list",
                     assay.measurement.types = "character",
                     assay.measurement.types.per.study = "list",
                     data.filenames = "list",
                     samples = "character",
                     samples.per.study = "list",
                     samples.per.assay.filename = "list",
                     assay.filenames.per.sample = "list",
                     sample.to.rawdatafile = "list",
                     sample.to.assayname = "list",
                     rawdatafile.to.sample = "list",
                     assayname.to.sample = "list",
                     factors = "list", 
                     treatments = "list", 
                     groups = "list",
                     assay.tabs = "list")
                  )

#' @title
#' S4 Class AssayTab
#'
#' @description
#' An S4 class to store information of an AssayTab.
#'
#' @slot path A length-one character vector containing the path to the ISA-Tab
#'            dataset.
#' @slot study.filename A character vector with the filename of the study to
#'                      which this assay belongs to.
#' @slot study.identifier A character vector with the identifier of the study to
#'                        which this assay belongs to.
#' @slot assay.filename A character vector with the filename of the assay.
#' @slot assay.file A data frame object with the contents of the assay file.
#' @slot assay.technology.type A character vector with the technology type of the
#'                             assay.
#' @slot assay.measurement.type A character vector with the measurement type of
#'                              the assay.
#' @slot assay.names A data frame object with the assay names for the assay file.
#' @slot data.filenames A data frame object with the data filenames for the assay.
#'
#' @author 
#' Alejandra Gonzalez-Beltran (maintainer, ISA Team e-mail:\email{isatools@googlegroups.com})
#'
#' @examples
#' showClass("AssayTab")
#' showClass("MSAssayTab")
#' showClass("MicroarrayAssayTab")
#' showClass("SeqAssayTab")
#' showClass("NMRAssayTab")
#'
#' @keywords classes
#'
#' @seealso \code{\link{ISATab-class}}, \code{\link{ISATab-methods}}
#' @aliases MSAssayTab, MicroarrayAssayTab, SeqAssayTab, NMRAssayTab
AssayTab <- setClass(Class = "AssayTab",
                     slots = c(
                       path = "character",
                       study.filename = "character",
                       study.identifier = "character",
                       assay.filename = "character",
                       assay.file = "data.frame",
                       assay.technology.type = "character",
                       assay.measurement.type = "character",
                       assay.names = "data.frame",
                       data.filenames = "data.frame")
                     )

validMSAssayTabObject <- function(object) {
  if (object@assay.technology.type == "mass spectrometry") {
    TRUE
  } else {
    paste("Technology type is not 'mass spectrometry' for ",
          object,
          sep = "")
  }
}

MSAssayTab <- setClass(Class = "MSAssayTab",
                       representation(),
                       prototype(assay.technology.type = "mass spectrometry"),
                       contains = "AssayTab",
                       validity = validMSAssayTabObject)

validMicroarrayAssayTabObject <- function(object) {
  if (object@assay.technology.type == technology.types$microarray) {
    return(TRUE)
  } else {
    return(paste("Technology type is not 'DNA microarray' for ",
                 object,
                 sep = ""))
  }
}

MicroarrayAssayTab <- setClass(Class = "MicroarrayAssayTab",
                               representation(),
                               prototype(assay.technology.type = "DNA microarray"),
                               contains = "AssayTab",
                               validity = validMicroarrayAssayTabObject)

SeqAssayTab <- setClass(Class = "SeqAssayTab",
                        representation(),
                        prototype(assay.technology.type = "nucleotide sequencing"),
                        contains = "AssayTab")

NMRAssayTab <- setClass(Class = "NMRAssayTab",
                        representation(),
                        prototype(assay.technology.type = "NMR spectroscopy"),
                        contains = "AssayTab")
