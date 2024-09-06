# Constellation plots -- pre-process data for SPH Hex (new)
# By Emily Hashimoto-Roth

library(data.table)

# Define colour palette
fill_sphhex <- c("#ef90d4", "#ef8afa", "#cc87f7", "#ed857d", "#3f8c57", "#8fe3a9",
                 "#ea4691", "#737373", "#882111", "#a1c75b", "#5bb3c7", "#ff91a4")

# Pre-process data
prepareSPH_HEX <- function(nodes, edges) {
  
  # Define value for theta in radians
  # theta = 45
  theta <- 90 * (pi / 180)

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
  
  # HexCer(d18:1) -- id 3
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
  x6 <- round(x4, 5)
  y6 <- round(y4 - edges[5, 7], 5)
  x <- append(x, x6)
  y <- append(y, y6)
  
  # HexSph(d18:1) -- id 7
  if (edges[7, 5] == 0) {
    x7 <- round(x3 + edges[6, 7] * cos(theta), 5)
    y7 <- round(y3 + edges[6, 7] * sin(theta), 5)
    x <- append(x, x7)
    y <- append(y, y7)
  } else if (edges[3, 5] != 0 & edges[7, 5] != 0) {
    x7 <- round(x3 + edges[6, 7] * cos(theta * 0.7), 5)
    y7 <- round(y3 + edges[6, 7] * sin(theta * 0.7), 5)
    x <- append(x, x7)
    y <- append(y, y7)
  } else {
    x7 <- round(x3 + edges[6, 7] * cos(theta / 2), 5)
    y7 <- round(y3 + edges[6, 7] * sin(theta / 2), 5)
    x <- append(x, x7)
    y <- append(y, y7)
  }
  
  # Sph(d18:1) -- id 8
  if (edges[3, 5] != 0) {
    x8 <- round(x4 + edges[8, 7] * cos(theta * 0.6), 5)
    y8 <- round(y4 + edges[8, 7] * sin(theta * 0.6), 5)
    x <- append(x, x8)
    y <- append(y, y8)
  } else {
    x8 <- round(x4 + edges[8, 7] * cos(theta), 5)
    y8 <- round(y4 + edges[8, 7] * sin(theta), 5)
    x <- append(x, x8)
    y <- append(y, y8) 
  }
  
  # S1P(d18:1) -- id 9
  x9 <- round(x8 + edges[9, 7] * cos(0), 5)
  y9 <- round(y8 + edges[9, 7] * sin(0), 5)
  x <- append(x, x9)
  y <- append(y, y9)
  
  # HexCer(d18:1)OH -- id 10
  if (edges[3, 5] != 0 & edges[7, 5] != 0) {
    x10 <- round(x7 + edges[10, 7] * cos(theta * 0.8), 5)
    y10 <- round(y7 + edges[10, 7] * sin(theta * 0.8), 5)
    x <- append(x, x10)
    y <- append(y, y10)
  } else if (edges[3, 5] != 0 & edges[12, 5] != 0) {
    x10 <- round(x7 + edges[10, 7] * cos(theta * 0.8), 5)
    y10 <- round(y7 + edges[10, 7] * sin(theta * 0.8), 5)
    x <- append(x, x10)
    y <- append(y, y10)
  } else {
    x10 <- round(x7 + edges[10, 7] * cos(theta), 5)
    y10 <- round(y7 + edges[10, 7] * sin(theta), 5)
    x <- append(x, x10)
    y <- append(y, y10)
  }
  
  # Cer(d18:1)OH -- id 11
  if (edges[7, 5] == 0 & edges[12, 5] == 0) {
    x11 <- round(x8 + edges[11, 7] * cos(theta), 5)
    y11 <- round(y8 + edges[11, 7] * sin(theta), 5)
    x <- append(x, x11)
    y <- append(y, y11)
  } else if (edges[3, 5] != 0 & edges[12, 5] != 0) {
    x11 <- round(x8 + edges[11, 7] * cos(theta * 1.2), 5)
    y11 <- round(y8 + edges[11, 7] * sin(theta * 1.2), 5)
    x <- append(x, x11)
    y <- append(y, y11)
  } else {
    x11 <- round(x8 + edges[11, 7] * cos(theta / 2), 5)
    y11 <- round(y8 + edges[11, 7] * sin(theta / 2), 5)
    x <- append(x, x11)
    y <- append(y, y11)
  }
  
  # SM(d18:1)OH -- id 12
  x12 <- round(x11 + edges[13, 7] * cos(0), 5)
  y12 <- round(y11 + edges[13, 7] * sin(0), 5)
  x <- append(x, x12)
  y <- append(y, y12)
  
  # Concatenate coordinate values to nodes table
  nodes <- nodes[1:12, ]
  print(nodes)
  print(x)
  print(length(x))
  nodes <- cbind(nodes, x)
  nodes <- cbind(nodes, y)
  
  # Concatenate fill colours
  fill_sphhex <- fill_sphhex[1:12]
  nodes <- cbind(nodes, fill_sphhex)
  
  # Determine node border colour
  border <- c()
  for (j in 1:nrow(nodes)) {
    if (nodes[j, 4] == 0) {
      border <- append(border, fill_sphhex[j])
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
  write.csv(nodes, "nodes_sph_hex_forPlotting.csv", row.names = FALSE)
  write.csv(edges, "edges_sph_hex_forPlotting.csv", row.names = FALSE)
  
  return(list(nodes, edges))
  
}






