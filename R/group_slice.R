#' Step6 Select specific rows from the grouping information
#'
#' @param select the groups to be selected, use ^ or $ please.
#' @param vector the group vector to be select from
#' @param data the data frame to be subset
#'
#' @return a logical vector with the same long as the input vector or a data frame
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' ind <- group_slice(select = c("CTL$", "UC$", "LA$"), vector = group$group, data = sum.phylum)
group_slice <- function(select = c("CTL$", "UC$", "LA$"), vector = group$group, data = NULL){
  # step 1 get a logical matrix
  if (!is.null(data) & is.null(vector)) {
    vector <- data$group
  }
  sel <- sapply(X = select, FUN = grepl, vector)
  # step 2 calculate the sum of the rows, and turned it to a logical vector
  ind <- apply(sel, 1, sum)  |> as.logical()
  # step 3 subset or return the ind vector
  if (!is.null(data)) {
    data <- data[ind, ]
    return(data)
  } else {
    return(ind)
  }
}


