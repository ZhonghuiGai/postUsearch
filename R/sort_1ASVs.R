#' Step1 Read the ASVs data and sort by copies number
#'
#' @param asv.path The path of the ASVs_norm.txt data
#' @param percent Transform to abundance percent
#' @param trans a boolean value, Inverse the matrix
#'
#' @return a data frame containg the ASVs
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' sort_1ASVs(asv.path = "result/ASVs_norm.txt")
sort_1ASVs <- function(asv.path = "ASVs_norm.txt", percent = TRUE, trans = TRUE){
  # step 1 read the OTU table for the result of Usearch
  genus <- read.delim(file = asv.path, header = T, row.names = 1, sep = "\t")
  # step 2 sort the ASVs by abundance
  genus <- genus[order(rowSums(genus), decreasing = TRUE), ]
  # step 3 transform to percent %
  if (percent) {
    genus <- apply(genus, 2, function(x) x/sum(x)*100)
  }
  # Calculates the inverse of the matrix
  if (trans) {
    genus <- t(genus)
  }
  return(as.data.frame(genus))
}

