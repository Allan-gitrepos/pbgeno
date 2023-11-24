jaccard_dist_matrix <- function(data) {
  # Extract the genotype names and the marker data
  genotypes <- colnames(data)[-1]
  markers <- data[, -1]

  # Initialize the distance matrix
  n <- ncol(markers)
  dist_matrix <- matrix(0, nrow = n, ncol = n)

  # Calculate the Jaccard distance matrix between genotypes
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      x <- markers[, i]
      y <- markers[, j]
      dist_matrix[i, j] <- 1 - sum(x & y) / sum(x | y)
    }
  }
  dist_matrix[lower.tri(dist_matrix)] <- t(dist_matrix)[lower.tri(dist_matrix)]

  # Set the row and column names of the distance matrix to the genotype names
  rownames(dist_matrix) <- genotypes
  colnames(dist_matrix) <- genotypes

  return(dist_matrix)
}




