#
# build DT merging requests/responses
#
# change to full dataset
require(data.table)

# TODO add unitID

#datafile <- "10k.dat"
datafile <- "~/rDia/data/scadaCops/modbus/modbus.data"

moddataDT <- as.data.table(read.csv(datafile, header=TRUE,
                                    colClass=c(ip.proto="factor", ip.version="factor",
                                               mbtcp.modbus.func_code="factor", tcp.srcport="factor",
                                               tcp.dstport="factor", mbtcp.modbus.reference_num="factor",
                                               mbtcp.prot_id="factor")))


# mergedDT <- as.data.table(read.csv("mergedAll.dat", header=TRUE,
#                                    colClass=c(mbtcp.prot_id="factor", 
#                                               mbtcp.modbus.func_code="factor", tcp.srcport="factor",
#                                               tcp.dstport="factor", mbtcp.modbus.reference_num="factor",
#                                               resp.func.code="factor", resp.srcport="factor",
#                                               resp.dstport="factor", resp.prot_id="factor")))

# cleanup
moddataDT <- moddataDT[!is.na(frame.number)]
moddataDT <- moddataDT[!is.na(mbtcp.trans_id)]

# 
ggplot(moddataDT, aes(x=frame.number)) + geom_line(aes(y=frame.time_relative, color="frame.time_relative"))+
  geom_point(aes(y=mbtcp.len, color="mbtcp.len"))+
  scale_colour_manual(name='', values=c('frame.time_relative'='olivedrab', 'mbtcp.len'='firebrick'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0)
                                                   , shape=c(NA, 16))))


# xyp1 <- ggplot(moddataDT, aes(x=frame.number, y=frame.time_relative)) +
#   geom_point(shape=1, col="firebrick", alpha=0.5) + ggtitle("Frame # / Time")
# 
# xyp2 <- ggplot(moddataDT, aes(x=frame.number, y=mbtcp.len)) +
#   geom_point(shape=1, col="olivedrab", alpha=0.5) + ggtitle("Frame # / Length")
# 
# grid.arrange(xyp1, xyp2,ncol=2)

# test
#moddataDT <- moddataDT[1:2]
# moddataDT <- moddataDT[1:2000]

# loop through each row
# iterPkt <- iter(moddataDT, by="row")
#   set mergedRow
#   if request
# while (hasNext(iterPkt)) {
#   print(nextElem(iterPkt))
#      set request fields in mergedRow: frame.time_relative, frame.time_delta_displayed, frame.len,
#           ip.src, ip.dst, ip.hdr_len, tcp.srcport, tcp.dstport, mbtcp.prot_id,
#           mbtcp.trans_id, mbtcp.len, mbtcp.modbus.func_code, mbtcp.modbus.reference_num,
#           mbtcp.modbus.word_cnt, mbtcp.modbus.data
#   get next row, should be response
#      set response fields in mergedRow
#      add mergedRow to mergedDT
# }

# preallocate
numrows <- (nrow(moddataDT)/2)

frame.number<-numeric(numrows)
frame.time_relative<-numeric(numrows)
frame.time_delta_displayed<-numeric(numrows)
frame.len<-numeric(numrows)
ip.src <- character(numrows)
ip.dst <- character(numrows)
tcp.srcport <- character(numrows)
tcp.dstport <- character(numrows)
mbtcp.prot_id <- character(numrows)
mbtcp.trans_id <- numeric(numrows)
mbtcp.len <- numeric(numrows)
mbtcp.modbus.func_code <- character(numrows)
mbtcp.modbus.word_cnt <- numeric(numrows)
mbtcp.modbus.reference_num <- character(numrows)
resp.fr.number <- numeric(numrows) 
resp.time.rel <- numeric(numrows)
resp.time.delta <- numeric(numrows)
resp.len <- numeric(numrows)
resp.src <- character(numrows)
resp.dest <- character(numrows)
resp.srcport <- character(numrows)
resp.dstport<- character(numrows)
resp.prot_id <- character(numrows)
resp.trans_id <- numeric(numrows)
resp.mbcp.len <- numeric(numrows)
resp.func.code <- character(numrows)
resp.data <- character(numrows)

mergedDT<- data.table(frame.number, frame.time_relative, frame.time_delta_displayed, frame.len,
                      ip.src, ip.dst, tcp.srcport, tcp.dstport, mbtcp.prot_id,
                      mbtcp.trans_id, mbtcp.len, mbtcp.modbus.func_code, mbtcp.modbus.word_cnt,
                      mbtcp.modbus.reference_num, resp.fr.number, resp.time.rel, resp.time.delta,
                      resp.len, resp.src, resp.dest, resp.srcport, resp.dstport, resp.prot_id,
                      resp.trans_id, resp.mbcp.len, resp.func.code, resp.data
                      )

idx <- 1

system.time(
for (i in 1:nrow(moddataDT)) {
  pkt <- moddataDT[i,]
#   print(paste("pkt :", i))
#   print(pkt)
  
  #   if request (assumption is that we're starting with a request)
  if (pkt$tcp.dstport=="502") {
    mergedRow <- pkt[,.(frame.number, frame.time_relative, frame.time_delta_displayed, frame.len,
                        ip.src, ip.dst, tcp.srcport, tcp.dstport, mbtcp.prot_id,
                        mbtcp.trans_id, mbtcp.len, mbtcp.modbus.func_code, mbtcp.modbus.word_cnt,
                        mbtcp.modbus.reference_num)]
    setkey(mergedRow, mbtcp.trans_id)
#     print(paste("mergedRow: ", i))
#     print(mergedRow)
  } # end if request
  
  # get next row, should be response
  if (pkt$tcp.srcport=="502") {
    # set response fields in mergedRow
    addCols <- pkt[,.(resp.fr.number=frame.number, resp.time.rel=frame.time_relative, 
                      resp.time.delta=frame.time_delta_displayed,
                      resp.len=frame.len, resp.src=ip.src, resp.dest=ip.dst, resp.srcport=tcp.srcport,
                      resp.dstport=tcp.dstport, resp.prot_id=mbtcp.prot_id, resp.trans_id=mbtcp.trans_id,
                      resp.mbcp.len=mbtcp.len, resp.func.code=mbtcp.modbus.func_code,
                      resp.data=mbtcp.modbus.data)]
      setkey(addCols, resp.trans_id)
#     print(paste("addCols: ", i))
#     print(addCols)
    
    # create new row
    mergedRow <- mergedRow[addCols]
#     print("mergedrow+addCols: ")
#     print(mergedRow)

    # add merged row to dataset
    mergedDT[idx, `:=`("frame.number"=mergedRow$frame.number, "frame.time_relative"=mergedRow$frame.time_relative,
                      "frame.time_delta_displayed"=mergedRow$frame.time_delta_displayed, "frame.len"=mergedRow$frame.len,
                      "ip.src"=mergedRow$ip.src, "ip.dst"=mergedRow$ip.dst,
                      "tcp.srcport"=mergedRow$tcp.srcport,
                      "tcp.dstport"=mergedRow$tcp.dstport, "mbtcp.prot_id"=mergedRow$mbtcp.prot_id,
                      "mbtcp.trans_id"=mergedRow$mbtcp.trans_id, "mbtcp.len"=mergedRow$mbtcp.len,
                      "mbtcp.modbus.func_code"=mergedRow$mbtcp.modbus.func_code,
                      "mbtcp.modbus.word_cnt"=mergedRow$mbtcp.modbus.word_cnt,
                      "mbtcp.modbus.reference_num"=mergedRow$mbtcp.modbus.reference_num,
                      "resp.fr.number"=mergedRow$resp.fr.number, "resp.time.rel"=mergedRow$resp.time.rel,
                      "resp.time.delta"=mergedRow$resp.time.delta, "resp.len"=mergedRow$resp.len,
                      "resp.src"=mergedRow$resp.src, "resp.dest"=mergedRow$resp.dest,
                      "resp.srcport"=mergedRow$resp.srcport, "resp.dstport"=mergedRow$resp.dstport,
                      "resp.prot_id"=mergedRow$resp.prot_id, "resp.trans_id"=mergedRow$mbtcp.trans_id,
                      "resp.mbcp.len"=mergedRow$resp.mbcp.len, "resp.func.code"=mergedRow$resp.func.code,
                      "resp.data"=mergedRow$resp.data)
             ]
    
    idx <- idx+1
  }

} # end for i in moddataDT
) # end system.time


rm(pkt, mergedRow, addCols, i, idx)

# decompose data field
# mergedDT$PX <- as.character(lapply(strsplit(as.character(mergedDT$resp.data), split=":"), "[", 1))
# mergedDT$PY <- as.character(lapply(strsplit(as.character(mergedDT$resp.data), split=":"), "[", 2))

# mergedDT <- separate(mergedDT, resp.data, c("d1","d2"), ":", remove=F, extra="merge")

# convert to decimal
# mergedDT$PX1 <- as.integer(paste("0x", as.character(lapply(strsplit(as.character(mergedDT$resp.data), split=":"), "[", 1)), sep=""))
# mergedDT$PY1 <- as.integer(paste("0x", as.character(lapply(strsplit(as.character(mergedDT$resp.data), split=":"), "[", 2)), sep=""))
# mergedDT[, ':=' (d1 = as.integer(paste("0x", d1, sep="")),
#                  d2 = as.integer(paste("0x", d2, sep=""))
# )]


mergedDT[, d:=as.integer(paste("0x", gsub(":", "", resp.data), sep=""))]

mergedDT$ip.src <- factor(mergedDT$ip.src)
mergedDT$ip.dst <- factor(mergedDT$ip.dst)
mergedDT$tcp.srcport <- factor(mergedDT$tcp.srcport)
mergedDT$tcp.dstport <- factor(mergedDT$tcp.dstport)
mergedDT$mbtcp.prot_id<- factor(mergedDT$mbtcp.prot_id)
mergedDT$mbtcp.modbus.func_code <- factor(mergedDT$mbtcp.modbus.func_code)
mergedDT$resp.src <- factor(mergedDT$resp.src)
mergedDT$resp.dest <- factor(mergedDT$resp.dest)
mergedDT$resp.srcport <- factor(mergedDT$resp.srcport)
mergedDT$resp.dstport <- factor(mergedDT$resp.dstport)
mergedDT$resp.prot_id <- factor(mergedDT$resp.prot_id)
mergedDT$resp.data <- factor(mergedDT$resp.data)
mergedDT$resp.func.code <- factor(mergedDT$resp.func.code)
mergedDT$mbtcp.modbus.reference_num <- factor(mergedDT$mbtcp.modbus.reference_num)


# initial stats
# setkey(mergedDT, resp.func.code, mbtcp.modbus.reference_num)
summary(mergedDT)
# > summary(mergedDT)
#         X           frame.number     frame.time_relative frame.time_delta_displayed   frame.len                 ip.src      
# Min.   :     1   Min.   :      0   Min.   :    0       Min.   :0.000              Min.   :  0.00                 :   158  
# 1st Qu.:103191   1st Qu.: 214962   1st Qu.: 3699       1st Qu.:0.000              1st Qu.: 66.00   192.168.12.250:    12  
# Median :206382   Median : 431188   Median : 7518       Median :0.000              Median : 66.00   192.168.12.51 :395531  
# Mean   :206382   Mean   : 637759   Mean   : 7631       Mean   :0.015              Mean   : 65.98   192.168.12.80 :     4  
# 3rd Qu.:309572   3rd Qu.: 648965   3rd Qu.:10654       3rd Qu.:0.000              3rd Qu.: 66.00   192.168.12.90 :  7781  
# Max.   :412762   Max.   :2297650   Max.   :16482       Max.   :1.233              Max.   :315.00   192.168.50.50 :   195  
# NA's   :9081     NA's   :9081      NA's   :9081        NA's   :9081               NA's   :  9081  
# ip.dst        tcp.srcport     tcp.dstport   mbtcp.prot_id mbtcp.trans_id      mbtcp.len       mbtcp.modbus.func_code
#               :   158   2735   :227139       :   158       :   158   Min.   :    0.0   Min.   :  0.000       :   158           
# 192.168.12.253:403523   2499   :110386   502 :403523   0   :403523   1st Qu.:   65.0   1st Qu.:  6.000   1   :  7770           
# NA's          :  9081   3441   : 58006   NA's:  9081   NA's:  9081   Median :  131.0   Median :  6.000   4   :395531           
# 1037   :  6447                               Mean   :  989.4   Mean   :  6.005   43  :     4           
# 1032   :  1102                               3rd Qu.:  197.0   3rd Qu.:  6.000   90  :   218           
#             (Other):   601                               Max.   :65313.0   Max.   :255.000   NA's:  9081           
# NA's   :  9081                                                 NA's   :9081                            
# mbtcp.modbus.word_cnt mbtcp.modbus.reference_num resp.fr.number    resp.time.rel   resp.time.delta    resp.len     
# Min.   :0                 :   380                Min.   :      0   Min.   :    0   Min.   :0.000   Min.   :  0.00  
# 1st Qu.:1             0   :170489                1st Qu.: 215350   1st Qu.: 3704   1st Qu.:0.010   1st Qu.: 65.00  
# Median :1             1   :186219                Median : 431230   Median : 7518   Median :0.010   Median : 65.00  
# Mean   :1             2   : 23375                Mean   : 634753   Mean   : 7625   Mean   :0.011   Mean   : 65.32  
# 3rd Qu.:1             3   : 23218                3rd Qu.: 647762   3rd Qu.:10636   3rd Qu.:0.010   3rd Qu.: 65.00  
# Max.   :1             NA's:  9081                Max.   :2297651   Max.   :16482   Max.   :0.119   Max.   :315.00  
#  NA's   :17073                                    NA's   :3450      NA's   :3450    NA's   :3450    NA's   :3450    
#       resp.src               resp.dest      resp.srcport   resp.dstport    resp.prot_id  resp.trans_id     resp.mbcp.len    
#               :   158                 :   158   502 :409154   2735   :229397   0   :409154   Min.   :    0.0   Min.   :  0.000  
# 192.168.12.253:409154   192.168.12.250:   284   NA's:  3608   2499   :111014   NA's:  3608   1st Qu.:   65.0   1st Qu.:  5.000  
# NA's          :  3450   192.168.12.51 :398417                 3441   : 58006                 Median :  131.0   Median :  5.000  
#                          192.168.12.80 :     4                 1037   :  8553                 Mean   :  989.4   Mean   :  5.347  
#                          192.168.12.90 : 10449                 1032   :  1550                 3rd Qu.:  197.0   3rd Qu.:  5.000  
#                          NA's          :  3450                 (Other):   634                 Max.   :65313.0   Max.   :255.000  
# NA's   :  3608                                   NA's   :3450     
# resp.func.code   resp.data            d            
# :   158    00:75  :163854   Min.   :8.000e+01  
# 1   : 10432    00:50  :154306   1st Qu.:8.000e+01  
# 4   :398417    00:54  : 33278   Median :1.170e+02  
# 43  :     5           : 10841   Mean   :8.171e+04  
# 90  :   300    1a:10  :  5431   3rd Qu.:1.170e+02  
# NA's:  3450    (Other): 41602   Max.   :2.147e+09  
#                 NA's   :  3450   NA's   :14291


modDataStats <- mergedSewDT[,.(count=.N, d.min=min(d), d.mean=mean(d, na.rm=T), d.max=max(d),
                            d.sd=sd(d, na.rm=T), min.resp.time.rel=min(resp.time.rel),min.resp.time.rel= max(resp.time.rel)),
                         by=.(resp.func.code, mbtcp.modbus.reference_num)][order(resp.func.code, mbtcp.modbus.reference_num)]


modDataStats

# > modDataStats
#    resp.func.code mbtcp.modbus.reference_num  count d.min       d.mean d.max         d.sd min.resp.time.rel min.resp.time.rel
# 1:             NA                         NA   3450    NA          NaN    NA           NA                NA                NA
# 2:             NA                               158    NA          NaN    NA           NA            0.0000             0.000
# 3:              1                         NA   2663    NA          NaN    NA           NA         1877.1583         13839.574
# 4:              1                          0   7769    NA          NaN    NA           NA         1834.1548         13840.532
# 5:              4                         NA   2885    80 6.398964e+02  6680 1.511860e+03         1877.0379         11873.525
# 6:              4                          0 162720   117 1.170000e+02   117 0.000000e+00          572.0978         16479.497
# 7:              4                          1 186219    80 8.070932e+01    84 1.527791e+00          572.1377         16480.157
# 8:              4                          2  23375  1325 3.638839e+03  5136 9.848511e+02          572.0877         16482.127
# 9:              4                          3  23218  1726 4.726736e+03  9350 1.969740e+03          572.1577         16479.167
# 10:             43                         NA      1    NA          NaN    NA           NA         1877.0490          1877.049
# 11:             43                                 4    NA          NaN    NA           NA        13788.7407         14303.061
# 12:             90                         NA     82    NA 1.545950e+08    NA 5.736099e+08         1877.0571          7567.928
# 13:             90                               218    NA 7.541246e+08    NA 1.035487e+09         1877.1080          7586.738

# scatterplot
xyp <- ggplot(mergedDT, aes(resp.time.rel, d, color=factor(mbtcp.modbus.reference_num))) +
  geom_point() + facet_grid(~resp.func.code) + ggtitle("Modbus Data Value (d) Over Time by Function Code")
xyp

# by mbtcp.modbus.reference_num
xyp <- ggplot(mergedDT[resp.func.code=="4"], aes(resp.time.rel, d, color=factor(mbtcp.modbus.reference_num))) +
  geom_point() + ggtitle("Modbus Data Value (d) Over Time For Function Code 4")
xyp


# trivariate plot
# cp <- cloud(resp.time.rel ~ d1 * d2 | mbtcp.modbus.reference_num,
#             data = mergedDT[!(is.na(resp.data))], zlim = rev(range(mergedDT$resp.time.rel)),
#             screen = list(z = 105, x = -70), panel.aspect = 0.75, xlab = "d2",
#             ylab = "d1", zlab = "resp.time.rel", main="3D of Time x d1 x d2")


# with color - this doesn't work with the zlim
# cloud(resp.time.rel ~ d + mbtcp.modbus.reference_num | resp.func.code, data = mergedDT,
#       col.point = mergedDT$mbtcp.modbus.reference_num, pch= 19,
#       zlim = rev(range(mergedDT$resp.time.rel)), xlab = "d",
#       ylab = "refNum", zlab = "resp.time.rel", main="3D of Reference Number by d Over Time Grouped by Function Code",
#       key = list(points = list(pch = 19, col = seq_along(levels(mergedDT$mbtcp.modbus.reference_num))), 
#                  text = list(levels(mergedDT$mbtcp.modbus.reference_num)), space = 'top',
#                  columns = nlevels(mergedDT$mbtcp.modbus.reference_num)))


cloud(resp.time.rel ~ d + mbtcp.modbus.reference_num | resp.func.code, data = mergedDT[resp.func.code=="4"],
      col.point = mergedDT$mbtcp.modbus.reference_num, pch= 19,
      xlab = "d", ylab = "refNum", zlab = "resp.time.rel",
      main="3D of Reference Number by d Over Time Grouped by Function Code",
      key = list(points = list(pch = 19, col = seq_along(levels(mergedDT$mbtcp.modbus.reference_num))), 
                 text = list(levels(mergedDT$mbtcp.modbus.reference_num)), space = 'top',
                 columns = nlevels(mergedDT$mbtcp.modbus.reference_num)))


rm("frame.number",  "frame.time_relative",  "frame.time_delta_displayed"
,"frame.len",  "ip.src", "ip.dst", "tcp.srcport"  
,"tcp.dstport", "mbtcp.prot_id",  "mbtcp.trans_id",  "mbtcp.len"                 
,"mbtcp.modbus.func_code", "mbtcp.modbus.word_cnt", "mbtcp.modbus.reference_num", "resp.fr.number"            
,"resp.time.rel"  ,"resp.time.delta",  "resp.len", "resp.src"                  
,"resp.dest", "resp.srcport", "resp.dstport",  "resp.prot_id"              
,"resp.trans_id", "resp.mbcp.len",  "resp.func.code", "resp.data" )


sewEndPtsDT <- as.data.table(read.csv("~/rDia/data/scadaCops/normal/sewTCPEndpoints.csv"))
sewTCPConvDT <- as.data.table(read.csv("~/rDia/data/scadaCops/normal/sewTCPConv.csv"))

# correlation
casted <- dcast(modDataStats[,.(resp.func.code, mbtcp.modbus.reference_num, d.mean)], resp.func.code ~ mbtcp.modbus.reference_num)
thecor <- cor(casted[2:5], method="spearman", use="complete.obs")
casted


head(nmmaps[,sort(c("death", "temp", "dewpoint", "pm10", "o3"))])


# df1<- melt(data=merged10k, id.vars="mbtcp.modbus.reference_num")
# 
# mergedDT[!(is.na(mbtcp.modbus.reference_num))]
# mergedDT<-mergedDT[!(is.na(frame.number))]

# pl <- cloud(depth ~ lat * long | Magnitude, data = quakes, zlim = rev(range(quakes$depth) ←
# ),
# +
#   screen = list(z = 105, x = -70), panel.aspect = 0.75, xlab = "Longitude",
# +
#   ylab = "Latitude", zlab = "Depth")


# modDataStats <- NULL
# 
# modDataStats <- mergedDT[,.(count=.N, PX1.min=min(PX1), PX1.mean=mean(PX1), PX1.max=max(PX1)),
#                          by=.(resp.func.code, mbtcp.modbus.reference_num)]
# 
# modDataStats <- cbind(modDataStats, mergedDT[,.(PY1.min=min(PY1), PX1.mean=mean(PY1), PY1.max=max(PY1)),
#          by=.(resp.func.code, mbtcp.modbus.reference_num)])

# 
# mergedDT[,.(PX, PX1, PX1.min=min(PX1), PX1.mean=mean(PX1), PX1.max=max(PX1),
#             PY, PY1, PY1.min=min(PY1), PY1.mean=mean(PY1), PY1.max=max(PY1)),
#          by=.(resp.func.code, mbtcp.modbus.reference_num)]


# rm(mergedDT)


# summaryBy(Sepal.Length + Sepal.Width + Petal.Length + Petal.Width ~ Species, data=iris, FUN=mean)
# dt$PX = as.character(lapply(strsplit(as.character(dt$PREFIX), split="_"), "[", 1))

# 
# as.integer(paste("0x", mergedDT$PX, sep=""))
# as.integer(paste("0x", mergedDT$PY, sep=""))

#     vals <- list(resp.time.rel=frame.time_relative, resp.time.delta=frame.time_delta_displayed,
#                   resp.len=frame.len, resp.src=ip.src, resp.dest=ip.dst, resp.srcport=tcp.srcport,
#                   resp.dstport=tcp.dstport, resp.prot_id=mbtcp.prot_id, mbtcp.trans_id,
#                   resp.mbcp.len=mbtcp.len, resp.func.code=mbtcp.modbus.func_code,
#                   resp.data=mbtcp.modbus.data)


#     colNames = names(mergedRow)
#     vals <- unlist(mergedRow)

#vals <- unlist(c(mergedRow), use.names=FALSE)

# add new row to data
#     mergedDT[idx, `:=` (frame.time_relative := mergedRow$frame.time_relative)]
#     idx <- idx+1

#     l <- list(mergedDT, mergedRow)
#     rbindlist(l, use.names=TRUE, fill=TRUE)

#     mergedRow <- merge(mergedRow, pkt[,.(resp.time.rel=frame.time_relative, resp.time.delta=frame.time_delta_displayed,
#                                          resp.len=frame.len, resp.src=ip.src, resp.dest=ip.dst, resp.srcport=tcp.srcport,
#                                          resp.dstport=tcp.dstport, resp.prot_id=mbtcp.prot_id, resp.trans.id=mbtcp.trans_id,
#                                          resp.mbcp.len=mbtcp.len, resp.func.code=mbtcp.modbus.func_code,
#                                          resp.data=mbtcp.modbus.data), key="mbtcp.trans_id"])
#     
# add mergedRow to mergedDT
#mergedDT <- rbind(mergedDT, mergedRow, use.names=TRUE, fill=TRUE)
