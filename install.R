

install.packages("Rcpp")
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
install.packages("ape")

install.packages("knitr")
install.packages("png")
#install.packages("grid")
install.packages("gridExtra")

install.packages("wordcloud")
install.packages("ROCR")

install.packages("caret")
install.packages("rjson")
install.packages("hash")
install.packages("xts")
install.packages("jsonlite")

install.packages("microbenchmark")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("rjson")

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
library(Matrix)
library(ROCR)
library(grid)
library(gridExtra)
library(rjson)
library(hash)
library(xts)
#library(jsonlite)
library(Rcpp)
library(microbenchmark)

# descriptive stats
library(boots) ### not in R3.2.2
library(pastecs)

# package from stats book
library(TRSbook)

# for parallel coordinatse
library("MASS")

# stars
require(grDevices)

# graph
library(tcltk)
#library(tgl)  ### installed tgl2
library(ape)

# reporting
library(knitr)

# wordcloud
library(wordcloud)

# ML
library(caret)

# anomaly detection
install.packages("devtools", dependencies=TRUE)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)