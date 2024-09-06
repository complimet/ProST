# Constellation plots -- pre-process data for GPC (new)
# By Emily Hashimoto-Roth

library(data.table)

# Define colour palette
fill_gpc <- c("#3f8c57", "#4294f8", "#f19937", "#fffb53", "#1432f5",
              "#893df6", "#71f79c", "#22538e", "#74f9fd")

# Pre-process data
prepareGPC <- function(nodes, edges) {
  
  # Define value for theta in radians
  theta <- 45 * (pi / 180)
  
  # Bin edge distances
  binned <- c()
  for (i in 1:nrow(edges)) {
    bin <- trunc(edges[i, 5] * 10) / 10
    binned <- append(binned, bin)
  }
  edges <- cbind(edges, binned)
  print(head(edges))
  
  # Get inverse of distance correlation values, append to edges table
  inverse <- c()
  for (i in 1:nrow(edges)) {
    edges[i, 6][edges[i, 6]!= 0] <- 0.6
    inv <- round(1 - edges[i, 6], 10)
    inverse <- append(inverse, inv)
  }
  edges <- cbind(edges, inverse)
  
  # Initiate vectors for (x, y) coordinates
  x <- c()
  y <- c()
  
  # SM(d18:1) -- id 1
  x1 <- 1.0
  y1 <- 1.0
  x <- append(x, x1)
  y <- append(y, y1)
  
  # PC -- id 2
  x2 <- round(x1 + edges[1, 7], 5)
  y2 <- 1.0
  x <- append(x, x2)
  y <- append(y, y2)
  
  # 1-acylLPC -- id 3
  x3 <- round(x2 + edges[2, 7], 5)
  y3 <- 1.0
  x <- append(x, x3)
  y <- append(y, y3)
  
  # 2-acylLPC -- id 4
  x4 <- round(x2 + edges[3, 7] * cos(-theta), 5)
  y4 <- round(y2 + edges[3, 7] * sin(-theta), 5)
  x <- append(x, x4)
  y <- append(y, y4)
  
  # PC(O) -- id 5
  x5 <- round(x2 + edges[4, 7] * cos(-theta * 3), 5)
  y5 <- round(y2 + edges[4, 7] * sin(-theta * 3), 5)
  x <- append(x, x5)
  y <- append(y, y5)
  
  # PC(P) -- id 6
  x6 <- round(x2 + edges[5, 7] * cos(theta), 5)
  y6 <- round(y2 + edges[5, 7] * sin(theta), 5)
  x <- append(x, x6)
  y <- append(y, y6)
  
  # LPC(O) -- id 7
  x7 <- round(x5 + edges[6, 7] * cos(-theta * 3), 5)
  y7 <- round(y5 + edges[6, 7] * sin(-theta * 3), 5)
  x <- append(x, x7)
  y <- append(y, y7)
  
  # PC-PAF(O) -- id 8
  x8 <- round(x7 + edges[8, 7] * cos(-theta * 3), 5)
  y8 <- round(y7 + edges[8, 7] * sin(-theta * 3), 5)
  x <- append(x, x8)
  y <- append(y, y8)
  
  # LPC(P) -- id 9
  x9 <- round(x6 + edges[7, 7] * cos(theta), 5)
  y9 <- round(y6 + edges[7, 7] * sin(theta), 5)
  x <- append(x, x9)
  y <- append(y, y9)
  
  # Concatenate coordinate values to nodes table
  nodes <- nodes[1:9, ]
  nodes <- cbind(nodes, x)
  nodes <- cbind(nodes, y)
  
  # Concatenate fill colours
  fill_gpc <- fill_gpc[1:9]
  nodes <- cbind(nodes, fill_gpc)
  
  # Determine node border colour
  border <- c()
  for (j in 1:nrow(nodes)) {
    if (nodes[j, 4] == 0) {
      border <- append(border, fill_gpc[j])
    } else {
      border <- append(border, "#000000")  # black border
    }
  }
  
  # Concatenate border colours
  nodes <- cbind(nodes, border)
  
  # Convert negative abundance values to 1
  # to do: make dynamic, only execute if negative values present in abundance column?
  no_negatives <- c()
  for (i in 1:nrow(nodes)) {
    if (nodes[i, 3] < 0) {
      no_negatives <- append(no_negatives, 1)
    } else {
      no_negatives <- append(no_negatives, nodes[i, 3])
    }
  }
  nodes <- cbind(nodes, no_negatives)
  
  # Save consolidated data that will be used for plotting to working directory
  write.csv(nodes, "nodes_gpc_forPlotting.csv", row.names = FALSE)
  write.csv(edges, "edges_gpc_forPlotting.csv", row.names = FALSE)
  
  return(list(nodes, edges))
  
}
