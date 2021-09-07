#' Step3 Read the metadata containg the grouping information
#'
#' @param group.path The path of the grouping metadata, which should be saved in xlsx format. The data contains two collumn, one for samples and another for grouping
#'
#' @return a data frame containg the grouping information, and also sorted by the order of rownames of genus
#' @export
#'
#' @examples
#' sort_3group(group.path = "group.xlsx")
sort_3group <- function(group.path = "group.xlsx"){
  # step 1 read in the grouping meta information
  group <- openxlsx::read.xlsx(xlsxFile = group.path, sheet = 1, colNames = T, rowNames = F)
  rownames(group) <- group$sample
  # step 2 check if the group information containing all the samples in the ASVs table (the genus file)
  if (all(rownames(genus) %in%  rownames(group))) {
    group <- group[rownames(group) %in% rownames(genus), ]
  } else {
    stop("Sth is wrong! Pls check your group.xlsx file! Missing some sample information")
  }
  # step 3 sort the group data to the order of the samples names of genus
  if (nrow(group) == nrow(genus)) {
    group <- group[rownames(genus), ]
  }
  # step return the success information
  if (all(rownames(group) == rownames(genus))) {
    cat("Bingo!, The group information has been sorted by the order of the OTU table.\n")
  }
  return(group)
}
