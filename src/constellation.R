# Constellation plots
# By Emily Hashimoto-Roth
# MAC USERS: please make sure you are using R version 4.0+
# WINDOWS USERS: please make sure you are using R version 4.2+

# SET WORKING DIRECTORY --> EITHER UPDATE THE PATH ACCORDINGLY OR COMMENT OUT AND SET VIA R STUDIO
setwd("~/NRL/constellation")

library("draw")
library("glue")
library("data.table")
source("gpc.R")
source("sph_hex.R")
source("sph_glcgal.R")

# FUNCTION: compute node radius from area, where area = abundance
getRadius <- function(area) {
  radius <- round(sqrt(area / pi), 5)
  return(radius)
}

# Load input files
nodes_file <- read.csv("sph_hex_node_female_TM.csv", header = TRUE, sep = ",")
edges_file <- read.csv("sph_hex_edge_female_TM.csv", header = TRUE, sep = ",")

# Specify pathway
# pathway <- "GPC"
pathway <- "SPH_HEX"
# pathway <- "SPH_GlcGal"

# Specify output parameters
# Save options include outputting to .PNG, .SVG, and .PDF (indicate "yes" or "no" below)
saveName <- "SPH_Hex_TM"
png <- "yes"
svg <- "no"
pdf <- "no"

# Preprocess input data based on required metabolic pathway
processed <- c()
if (pathway == "GPC") {
  processed <- prepareGPC(nodes_file, edges_file)
}
if (pathway == "SPH_HEX") {
  processed <- prepareSPH_HEX(nodes_file, edges_file)
}
if (pathway == "SPH_GlcGal") {
  processed <- prepareSPH_GlcGal(nodes_file, edges_file)
}
nodes <- data.frame(processed[1])
edges <- data.frame(processed[2])

# Transpose factor to vertically align network in center
y_max = max(nodes$y)
y_min = min(nodes$y)
y_dif = y_max - y_min
cy <- (4.75 + y_dif / 2)

# Transpose horizontally
x_max <- max(nodes$x)
x_min <- min(nodes$x)
x_dif <- x_max - x_min
cx <- (4 + x_dif / 2) - x_max

# Expansion factor (default to 1 in GUI)
ef <- 1

# Initiate drawing
drawSettings(pageWidth = 8.5, pageHeight = 11, units = "inches")
drawPage()

# Draw edges
for (i in 1:nrow(edges)) {
  drawLine(x = c(nodes[nodes$id == edges[i, 1], 5] * ef + cx, nodes[nodes$id == edges[i, 2], 5] * ef + cx),
           y = c(nodes[nodes$id == edges[i, 1], 6] * ef + cy, nodes[nodes$id == edges[i, 2], 6] * ef + cy),
           lineWidth = 1,
           lineColor = "grey")
}

# Draw nodes
for (j in 1:nrow(nodes)) {
  if (nodes[j, 3] == 0) {
    r <- getRadius(0.1)
  } else {
    r <- getRadius(nodes[j, 3])
  }
  drawCircle(x = nodes[j, 5] * ef + cx,
             y = nodes[j, 6] * ef + cy,
             radius = r / 10,  # scaling factor
             fillColor = nodes[j, 7],
             lineColor = nodes[j, 8],
             lineWidth = 2)
}

# Draw lipid class legend
cL <- 4

drawText(x = 1.93,
         y = cL + 0.3,
         text = "Lipid class",
         just = "left",
         size = 14,
         face = "bold")

counter = cL
for (k in 1:nrow(nodes)) {
  drawCircle(x = 2,
             y = counter,
             radius = 0.08,
             fillColor = nodes[k, 7],
             lineColor = nodes[k, 7])
  drawText(x = 2.25,
           y = counter,
           text = nodes[k, 2],
           just = "left",
           size = 12)
  counter = counter - 0.25
}

# Draw abundance legend
drawText(x = 5.75,
         y = cL + 0.3,
         text = "Abundance",
         just = "center",
         size = 14,
         face = "bold")

# Min
abundance_min <- round(min(nodes[, 3]), 5)
if (abundance_min == 0) {
  abundance_min_radius = 0.1
} else {
  abundance_min_radius = abundance_min
}
drawCircle(x = 5.4,
           y = cL,
           r = getRadius(abundance_min_radius) / 10,
           fillColor = "black",
           lineColor = "black")
drawText(x = 5.7,
         y = cL,
         text = abundance_min,
         just = "left",
         size = 12)

# Median
abundance_median <- round(median(nodes[, 3]), 5)
drawCircle(x = 5.4,
           y = cL - 0.35,
           r = getRadius(abundance_median) / 10,
           fillColor = "black",
           lineColor = "black")
drawText(x = 5.7,
         y = cL - 0.35,
         text = abundance_median,
         just = "left",
         size = 12)

# Max
abundance_max <- round(max(nodes[, 3]), 5)
drawCircle(x = 5.4,
           y = cL - 0.8,
           r = getRadius(abundance_max) / 10,
           fillColor = "black",
           lineColor = "black")
drawText(x = 5.7,
         y = cL - 0.8,
         text = abundance_max,
         just = "left",
         size = 12)

# Export (does not support export to .json)
savePNG <- glue("{saveName}.png")
saveSVG <- glue("{saveName}.svg")
savePDF <- glue("{saveName}.pdf")
if (png == "yes") {
  drawExport(savePNG, format = "png", ppi = 300)
}
if (svg == "yes") {
  drawExport(saveSVG, format = "svg", ppi = 300)
}
if (pdf == "yes") {
  drawExport(savePDF, format = "pdf", ppi = 300)
}
