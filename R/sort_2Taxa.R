#' Step2 Read the classification data of the ASVs
#'
#' @param taxa.path The path of taxa data, usually ASV_tax2.txt, this data also useful to lefse analysis
#' @param taxa a value to indicate whether the data already exist
#'
#' @return a data frame containing the classification information and the lineage
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' sort_2Taxa(taxa.path = "result/ASV_tax2.txt")
sort_2Taxa <- function(taxa.path = "ASV_tax2.txt", taxa = NULL){
  # step 1 split the tax vector from Usearch ansalysi to a list
  if (is.null(taxa)) {
    taxa <- read.delim(taxa.path, header = FALSE)
  } else {
    taxa <- taxa
  }
  tax <- strsplit(taxa[, 2], split = ";")
  # step 2 calculate the length of each vector of list tax
  length <- sapply(tax, length)
  # step 3 extract the last element of list tax
  result <- mapply(FUN = "[", tax, length)
  taxa$taxa <- result
  rownames(taxa) <- taxa[, 1]
  taxa <- taxa[, -1]
  colnames(taxa) <- c("lineage", "taxa")
  taxa <- taxa[colnames(genus), ]
  return(taxa)
}
