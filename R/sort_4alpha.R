#' Step4 Read in the alpha diversity infromation, which genarated by Usearch
#'
#' @param alpha.path The path of the alpha data, usually alpha.txt
#' @param extract a boolean value to indicate whether extract the useful information, the default value is TRUE
#'
#' @return a data frame containg the alpha diversity information
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' alpha <- sort_4alpha(alpha.path = "alpha/alpha.txt")
#'
sort_4alpha <- function(alpha.path = "alpha/alpha.txt", extract = TRUE){
  # step 1 read in the alpha result and judge the order of rownames
  alpha <- read.delim(file = alpha.path, header = TRUE, row.names = 1, sep = "\t")
  stopifnot(all(rownames(alpha) %in% rownames(genus)))
  stopifnot(all(rownames(alpha) == rownames(genus)))
  # step 2 sort the alpha order by genus rownames
  alpha <- alpha[rownames(genus), ]
  # step 3 extract
  if (extract) {
    stopifnot(all(rownames(alpha) == rownames(group)))
    alpha <- data.frame(group = group$group, alpha)
    alpha$group <- as.factor(alpha$group)
    alpha <- alpha[, c("group", "chao1", "shannon_e", "berger_parker", "simpson")]
  }
  return(alpha)
}
