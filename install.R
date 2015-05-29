library(data.table)
library(lattice)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(igraph)


# matrix
install.packages("Matrix")
library(Matrix)

# descriptive stats
install.packages("boots")
install.packages("pastecs")
library(boots)
library(pastecs)

# package from stats book
install.packages("TRSbook")
library(TRSbook)

# for parallel coordinatse
install.packages("MASS")
library("MASS")

# stars
require(grDevices)

# graph
install.packages("igraph")
install.packages("tcltk")
install.packages("rgl")
install.packages("ape")
library(tcltk)
library(tgl)
library(ape)

# anomaly detection
install.packages("devtools", dependencies=TRUE)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)