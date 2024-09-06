# Constellation plots -- pre-process data for SPH Glc Gal
# By Emily Hashimoto-Roth

library(data.table)

# Define colour palette
fill_sphgg <- c("#ef90d4", "#ef8afa", "#cc87f7", "#ed857d", "#3f8c57", "#8fe3a9",
                "#ea4691", "#737373", "#882111", "#882111", "#a1c75b")

# Pre-process data
prepareSPH_GlcGal <- function(nodes, edges) {
  
  # Define value for theta in radians
  theta <- 60 * (pi / 180)
  
  # Bin edge distances
  binned <- c()
  for (i in 1:nrow(edges)) {
    bin <- trunc(edges[i, 5] * 10) / 10
    binned <- append(binned, bin)
  }
  edges <- cbind(edges, binned)
  
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
  
  # GB3(d18:1) -- id 1
  x1 <- 1.0
  y1 <- 1.0
  x <- append(x, x1)
  y <- append(y, y1)
  
  # LacCer(d18:1) -- id 2
  x2 <- round(x1 + edges[1, 7], 5)
  y2 <- 1.0
  x <- append(x, x2)
  y <- append(y, y2)
  
  # GlcCer(d18:1) -- id 3
  x3 <- round(x2 + edges[2, 7], 5)
  y3 <- 1.0
  x <- append(x, x3)
  y <- append(y, y3)
  
  # Cer(d18:1) -- id 4
  x4 <- round(x3 + edges[3, 7], 5)
  y4 <- 1.0
  x <- append(x, x4)
  y <- append(y, y4)
  
  # SM(d18:1) -- id 5
  x5 <- round(x4 + edges[4, 7], 5)
  y5 <- 1.0
  x <- append(x, x5)
  y <- append(y, y5)
  
  # PECer(d18:1) -- id 6
  x6 <- round(x4 + edges[5, 7] * cos(theta), 5)
  y6 <- round(y4 + edges[5, 7] * sin(theta), 5)
  x <- append(x, x6)
  y <- append(y, y6)
  
  # try variable theta
  # assuming that 
  # htrap <- sqrt(abs(edges[6, 7]^2 - ((edges[3,7] - edges[7, 7])/(1 + edges[6, 7] / edges[8, 7])) ^ 2))
  # theta <- asin(htrap / edges[6,7])
  # theta[is.nan(theta)] <- pi/4
  
  # GalCer(d18:1) -- id 7
  x7 <- round(x4 + edges[6, 7] * cos(-theta), 5)
  y7 <- round(y4 + edges[6, 7] * sin(-theta), 5)
  x <- append(x, x7)
  y <- append(y, y7)
  
  # GalSph(d18:1) -- id 8
  x8 <- round(x7 + edges[7, 7] * cos(-theta * 2), 5)
  y8 <- round(y7 + edges[7, 7] * sin(-theta * 2), 5)
  x <- append(x, x8)
  y <- append(y, y8)
  
  # Sph(d18:1) -- id 9
  x9 <- round(x8 + edges[8, 7] * cos(theta * 2), 5)
  y9 <- round(y8 + edges[8, 7] * sin(theta * 2), 5)
  x <- append(x, x9)
  y <- append(y, y9)
  
  # Sph(d18:1) -- id 10
  x10 <- round(x4 + edges[10, 7] * cos(theta * 2), 5)
  y10 <- round(y4 + edges[10, 7] * sin(theta * 2), 5)
  x <- append(x, x10)
  y <- append(y, y10)
  
  # GlcSph(d18:1) -- id 11
  x11 <- round(x3 + edges[11, 7] * cos(theta * 2), 5)
  y11 <- round(y3 + edges[11, 7] * sin(theta * 2), 5)
  x <- append(x, x11)
  y <- append(y, y11)
  
  # Concatenate coordinate values to nodes table
  nodes <- nodes[1:11, ]
  nodes <- cbind(nodes, x)
  nodes <- cbind(nodes, y)
  
  # Concatenate fill colours
  fill_sphgg <- fill_sphgg[1:11]
  nodes <- cbind(nodes, fill_sphgg)
  
  # Determine node border colour
  border <- c()
  for (j in 1:nrow(nodes)) {
    if (nodes[j, 4] == 0) {
      border <- append(border, fill_sphgg[j])
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
  write.csv(nodes, "nodes_sph_glc_gal_forPlotting.csv", row.names = FALSE)
  write.csv(edges, "edges_sph_glc_gal_forPlotting.csv", row.names = FALSE)
  
  return(list(nodes, edges))
  
}






