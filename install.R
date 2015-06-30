

install.packages("data.table")
install.packages("tidyr")

install.packages("Matrix")
install.packages("boots")
install.packages("pastecs")
install.packages("TRSbook")
install.packages("MASS")
install.packages("gplots")

install.packages("igraph")
install.packages("tcltk")
install.packages("rgl")
install.packages("ape");)

install.packages("knitr")
install.packages("png")
install.packages("grid")
install.packages("gridExtra")

install.packages("wordcloud")
install.packages("ROCR")

library(data.table)
library(tidyr)
library(lattice)
library(ggplot2)
library(gplots)
library(plyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(igraph)
library(png)
library(grid)
library(Matrix)
library(ROCR)
library(grid)
library(gridExtra)

# descriptive stats
library(boots)
library(pastecs)

# package from stats book
library(TRSbook)

# for parallel coordinatse
library("MASS")

# stars
require(grDevices)

# graph
library(tcltk)
library(tgl)
library(ape)

# reporting
library(knitr)

# wordcloud
library(wordcloud)

# anomaly detection
install.packages("devtools", dependencies=TRUE)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)