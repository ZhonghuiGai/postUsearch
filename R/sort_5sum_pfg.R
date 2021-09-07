#' Step5 Read in the summary data from tax dictionary at the phylum, family and genus levels
#'
#' @param sum.path the path of the sum data
#'
#' @return a data frame containning grouping information
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' sum.phylum <- sort_5sum_pfg(sum.path = "tax/sum_phylum.txt")
#' sum.family <- sort_5sum_pfg(sum.path = "tax/sum_family.txt")
#' sum.genus <- sort_5sum_pfg(sum.path = "tax/sum_genus.txt")
sort_5sum_pfg <- function(sum.path = "tax/sum_phylum.txt"){
  # step 1 read in the alpha result and judge the order of rownames
  sum <- read.delim(file = sum.path, header = TRUE, row.names = 1, sep = "\t")
  # step 2 Inverse
  sum <- t(sum)  |> as.data.frame()
  sum <- sum[rownames(sum) != "All", ]
  stopifnot(all(rownames(sum) %in% rownames(genus)))
  stopifnot(all(rownames(sum) == rownames(genus)))
  # step 2 sort the order by genus rownames
  sum <- sum[rownames(genus), ]
  sum <- sum[, order(colSums(sum), decreasing = TRUE)]
  # step 3 add grouping infor
  if (all(rownames(group) == rownames(sum))) {
    sum <- data.frame(group = group$group, sum)
  }
  colnames(sum)[grepl("X.Unassigned.", colnames(sum))] <- "Unassigned"
  col_idx <- grep("Unassigned", names(sum))
  sum <- sum[, c((1:ncol(sum))[-col_idx], col_idx)]
  return(sum)
}


