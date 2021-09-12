#' Step7 Prepare data for LEfSe analysis, the input data are from the result of postUsearch step1-6
#'
#' @param group the grouping metadata
#' @param genus the ASVs abundance table, adapted from ASVs_norm.txt using sort_1ASVs function
#' @param taxa  the taxa data adapted from result/ASV_tax2.txt using sort_2Taxa function
#'
#' @return a data frame for LEfSe ananlysis
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' data_7for_lefse(group = group, genus = genus, taxa = taxa)
data_7for_lefse <- function(group, genus, taxa){
  # step 1 creat a dictionary to store the data for LEfSe analysis
  stopifnot(all(rownames(group) == rownames(genus)))
  dir.create("./data_for_lefse")
  write.table(group, file = "./data_for_lefse/group.txt", sep = "\t", col.names = F, row.names = F)
  # step 2 prepare ASV and taxa files
  genus.t <- t(genus)  |> as.data.frame()
  stopifnot(all(rownames(taxa) == rownames(genus.t)))
  genus.t <- data.frame(OTUID = rownames(genus.t), genus.t)
  colnames(genus.t)[1] <- "#OTU ID"
  rownames(genus.t) <- NULL
  write.table(genus.t, file = "./data_for_lefse/ASVs_norm.txt", sep = "\t", col.names = T, row.names = F)
  tax <- data.frame(rownames(taxa), taxa[, 1])
  write.table(tax, file = "./data_for_lefse/ASV_tax2.txt", sep = "\t", col.names = F, row.names = F)
}
