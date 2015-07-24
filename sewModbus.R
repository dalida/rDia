# sewModbusDT

datafile <- "~/scada/sew.data"

sewModbusDT <- as.data.table(
  read.csv(datafile, header=TRUE,
           stringsAsFactors=T,
           colClass=c(ip.proto="factor", ip.version="factor", ip.src="factor",
                      ip.dst="factor", eth.src="factor", eth.dst="factor",
                      mbtcp.modbus.unit_id="factor",
                      tcp.srcport="factor", tcp.dstport="factor",
                      mbtcp.modbus.func_code="factor",
                      mbtcp.modbus.reference_num="factor",
                      mbtcp.prot_id="factor")))

# cleanup

sewModbusDT <- sewModbusDT[!(is.na(frame.number))]
sewModbusDT <- sewModbusDT[!(is.na(mbtcp.modbus.unit_id))]
sewModbusDT <- sewModbusDT[!(is.na(mbtcp.trans_id))]
sewModbusDT <- sewModbusDT[!(is.na(mbtcp.modbus.reference_num))]

save(sewModbusDT, file="sew.Rda")
rm(datafile)

numrows <- (nrow(sewModbusDT)/2)
#numrows<-500

frame.number<-numeric(numrows)
frame.time_relative<-numeric(numrows)
frame.time_delta<-numeric(numrows)
frame.len<-numeric(numrows)
ip.src <- character(numrows)
eth.src <- character(numrows)
ip.dst <- character(numrows)
eth.dst <- character(numrows)
mbtcp.modbus.unit_id <- character(numrows)
tcp.srcport <- character(numrows)
tcp.dstport <- character(numrows)
mbtcp.prot_id <- character(numrows)
mbtcp.trans_id <- numeric(numrows)
mbtcp.len <- numeric(numrows)
mbtcp.modbus.func_code <- character(numrows)
mbtcp.modbus.word_cnt <- numeric(numrows)
mbtcp.modbus.reference_num <- character(numrows)
frame.second <-numeric(numrows)
resp.frame.number <- numeric(numrows) 
resp.time.rel <- numeric(numrows)
resp.time.delta <- numeric(numrows)
resp.len <- numeric(numrows)
resp.ip.src <- character(numrows)
resp.eth.src <- character(numrows)
resp.ip.dst <- character(numrows)
resp.eth.dst <- character(numrows)
resp.srcport <- character(numrows)
resp.dstport<- character(numrows)
resp.unit_id<- character(numrows)
resp.prot_id <- character(numrows)
resp.trans_id <- numeric(numrows)
resp.mbcp.len <- numeric(numrows)
resp.func.code <- character(numrows)
resp.second <-numeric(numrows)
resp.data <- character(numrows)
d <- character(numrows)

mergedSewDT<- data.table(frame.number, frame.time_relative, frame.time_delta, frame.len,
                      ip.src, eth.src, ip.dst, eth.dst, mbtcp.modbus.unit_id, tcp.srcport, tcp.dstport,
                      mbtcp.prot_id, mbtcp.trans_id, mbtcp.len, mbtcp.modbus.func_code,
                      mbtcp.modbus.word_cnt, frame.second, mbtcp.modbus.reference_num, resp.frame.number,
                      resp.time.rel, resp.time.delta, resp.len, resp.ip.src, resp.ip.dst,
                      resp.srcport, resp.unit_id, resp.dstport, resp.prot_id, resp.trans_id,
                      resp.mbcp.len, resp.func.code, resp.second, resp.data
)

idx <- 1

system.time(
  for (i in 1:nrow(sewModbusDT)) {
#  for (i in 1:(numrows*2)) {
    pkt <- sewModbusDT[i,]
    #   print(paste("pkt :", i))
    #   print(pkt)
  
    # TODO: need to check transactionID are the same, right now this is an assumption
    
    #   if request (assumption is that we're starting with a request)
    if (pkt$tcp.dstport=="502") {
      mergedSewRow <- pkt[,.(frame.number, frame.time_relative, frame.time_delta,
                             frame.len, ip.src, eth.src, ip.dst, eth.dst, mbtcp.modbus.unit_id,
                             tcp.srcport, tcp.dstport, mbtcp.prot_id, mbtcp.trans_id, mbtcp.len,
                             mbtcp.modbus.func_code, mbtcp.modbus.word_cnt, 
                             mbtcp.modbus.reference_num)]
                             
      setkey(mergedSewRow, mbtcp.trans_id)
      #     print(paste("mergedSewRow: ", i))
      #     print(mergedSewRow)
    } # end if request
    
    # get next row, should be response
    if (pkt$tcp.srcport=="502") {
      # set response fields in mergedSewRow
      addCols <- pkt[,.(resp.frame.number=frame.number, resp.time.rel=frame.time_relative, 
                        resp.time.delta=frame.time_delta,
                        resp.len=frame.len, resp.ip.src=ip.src, resp.eth.src=eth.src,
                        resp.ip.dst=ip.dst, resp.eth.dst=eth.dst,
                        resp.unit_id=mbtcp.modbus.unit_id, resp.srcport=tcp.srcport, 
                        resp.dstport=tcp.dstport, resp.prot_id=mbtcp.prot_id, 
                        resp.trans_id=mbtcp.trans_id, resp.mbcp.len=mbtcp.len,
                        resp.func.code=mbtcp.modbus.func_code, resp.data=mbtcp.modbus.data)]
      
      setkey(addCols, resp.trans_id)
      #     print(paste("addCols: ", i))
      #     print(addCols)
      
      # create new row
      mergedSewRow <- mergedSewRow[addCols]
      #     print("mergedSewrow+addCols: ")
      #     print(mergedSewRow)
      
      mergedSewDT[idx, `:=`("frame.number"=mergedSewRow$frame.number,
                            "frame.time_relative"=mergedSewRow$frame.time_relative,
                            "frame.time_delta"=mergedSewRow$frame.time_delta,
                            "frame.len"=mergedSewRow$frame.len, "ip.src"=mergedSewRow$ip.src,
                            "eth.src"=mergedSewRow$eth.src,  "ip.dst"=mergedSewRow$ip.dst,
                            "eth.dst"=mergedSewRow$eth.dst,
                            "mbtcp.modbus.unit_id"=mergedSewRow$mbtcp.modbus.unit_id,
                            "tcp.srcport"=mergedSewRow$tcp.srcport, "tcp.dstport"=mergedSewRow$tcp.dstport,
                            "mbtcp.prot_id"=mergedSewRow$mbtcp.prot_id,
                            "mbtcp.trans_id"=mergedSewRow$mbtcp.trans_id, "mbtcp.len"=mergedSewRow$mbtcp.len,
                            "mbtcp.modbus.func_code"=mergedSewRow$mbtcp.modbus.func_code,
                            "mbtcp.modbus.word_cnt"=mergedSewRow$mbtcp.modbus.word_cnt,
                            "mbtcp.modbus.reference_num"=mergedSewRow$mbtcp.modbus.reference_num,
                            "resp.frame.number"=mergedSewRow$resp.frame.number, "resp.time.rel"=mergedSewRow$resp.time.rel,
                            "resp.time.delta"=mergedSewRow$resp.time.delta, "resp.len"=mergedSewRow$resp.len,
                            "resp.ip.src"=mergedSewRow$resp.ip.src, "resp.eth.src"=mergedSewRow$resp.eth.src,
                            "resp.ip.dst"=mergedSewRow$resp.ip.dst, "resp.eth.dst"=mergedSewRow$resp.eth.dst,
                            "resp.unit_id"=mergedSewRow$resp.unit_id,
                            "resp.srcport"=mergedSewRow$resp.srcport, "resp.dstport"=mergedSewRow$resp.dstport,
                            "resp.prot_id"=mergedSewRow$resp.prot_id, "resp.trans_id"=mergedSewRow$mbtcp.trans_id,
                            "resp.mbcp.len"=mergedSewRow$resp.mbcp.len, "resp.func.code"=mergedSewRow$resp.func.code,
                            "resp.data"=mergedSewRow$resp.data)
               ]
      
      idx <- idx+1
      rm(mergedSewRow)
    } # end srcport=502
    rm(pkt,addCols)
  } # end for i in sewModbusDT
) # end system.time


rm(mergedSewRow, i, idx)

# convert hex to decimal
mergedSewDT[, d:=as.integer(paste("0x", gsub(":", "", resp.data), sep=""))]
# seconds
mergedSewDT$frame.second <- floor(mergedSewDT$frame.time_relative)
mergedSewDT$resp.second <- floor(mergedSewDT$resp.time.rel)

# factorize
mergedSewDT$ip.src <- factor(mergedSewDT$ip.src)
mergedSewDT$ip.dst <- factor(mergedSewDT$ip.dst)
mergedSewDT$mbtcp.modbus.unit_id <- factor(mergedSewDT$mbtcp.modbus.unit_id)
mergedSewDT$tcp.srcport <- factor(mergedSewDT$tcp.srcport)
mergedSewDT$tcp.dstport <- factor(mergedSewDT$tcp.dstport)
mergedSewDT$mbtcp.prot_id <- factor(mergedSewDT$mbtcp.prot_id)
mergedSewDT$mbtcp.modbus.func_code<- factor(mergedSewDT$mbtcp.modbus.func_code)
mergedSewDT$resp.ip.src <- factor(mergedSewDT$resp.ip.src)
mergedSewDT$resp.eth.src <- factor(mergedSewDT$resp.eth.src)
mergedSewDT$resp.ip.dst <- factor(mergedSewDT$resp.ip.dst)
mergedSewDT$resp.eth.dst <- factor(mergedSewDT$resp.eth.dst)
mergedSewDT$resp.unit_id <- factor(mergedSewDT$resp.unit_id)
mergedSewDT$resp.srcport <- factor(mergedSewDT$resp.srcport)
mergedSewDT$resp.dstport <- factor(mergedSewDT$resp.dstport)
mergedSewDT$resp.prot_id <- factor(mergedSewDT$resp.prot_id)
mergedSewDT$resp.data <- factor(mergedSewDT$resp.data)
mergedSewDT$resp.func.code <- factor(mergedSewDT$resp.func.code)
mergedSewDT$mbtcp.modbus.reference_num <- factor(mergedSewDT$mbtcp.modbus.reference_num)

mergedSewDT <- mergedSewDT[ip.src!=""]

save(mergedSewDT, file="sewMerged.Rda")

# decompose data field
# mergedSewDT$PX <- as.character(lapply(strsplit(as.character(mergedSewDT$resp.data), split=":"), "[", 1))
# mergedSewDT$PY <- as.character(lapply(strsplit(as.character(mergedSewDT$resp.data), split=":"), "[", 2))

# mergedSewDT <- separate(mergedSewDT, resp.data, c("d1","d2"), ":", remove=F, extra="merge")

# convert to decimal
# mergedSewDT$PX1 <- as.integer(paste("0x", as.character(lapply(strsplit(as.character(mergedSewDT$resp.data), split=":"), "[", 1)), sep=""))
# mergedSewDT$PY1 <- as.integer(paste("0x", as.character(lapply(strsplit(as.character(mergedSewDT$resp.data), split=":"), "[", 2)), sep=""))
# mergedSewDT[, ':=' (d1 = as.integer(paste("0x", d1, sep="")),
#                  d2 = as.integer(paste("0x", d2, sep=""))
# )]

# initial stats
setkey(mergedSewDT, resp.func.code, mbtcp.modbus.reference_num)

sewModbusDT[,.(count=.N), by=.(ip.src, ip.dst, mbtcp.modbus.unit_id)]
# mergedSewDT[,.(count=.N), by=.(ip.src, ip.dst, mbtcp.modbus.unit_id, resp.data)]


# modsewDataStats <- mergedSewDT[,.(count=.N, d.min=min(d), d.mean=mean(d, na.rm=T),
#                                   d.sd=sd(d, na.rm=T), d.max=max(d)),
#                             by=.(resp.func.code, mbtcp.modbus.reference_num)][order(resp.func.code, mbtcp.modbus.reference_num)]
# 
# modsewDataStats

# > modsewDataStats
#    resp.func.code mbtcp.modbus.reference_num count d.min     d.mean d.max        d.sd min.resp.time.rel min.resp.time.rel
# 1:              4                          0  7616   112  112.00000   112    0.000000          24.53739          601.3492
# 2:              4                          1  8704    64   81.30423    84    3.960659          24.58845          601.4262
# 3:              4                          2  1088  3455 4972.96140  5241  256.073630          24.52703          601.2214
# 4:              4                          3  1088     0 3467.62592  7327 2125.525453          24.61349          601.3094

# average frequency of packets per second
avgFrequency <- mergedSewDT[,.(frequency=.N),by=frame.second][,mean(frequency)]

# Source / Destination / UnitID
#sdu <- sewModbusDT[,.(count=.N), by=.(ip.src, ip.dst,mbtcp.modbus.unit_id)]
sdu <- unique(sewModbusDT, by=c("ip.src", "ip.dst", "mbtcp.modbus.unit_id"))[,.(ip.src, ip.dst,mbtcp.modbus.unit_id)]
sdu

#### GENERATE WHITE LIST ####
# Sources
# srcs <- sewModbusDT[,.(count=.N), by=.(ip.src)]
srcs  <- unique(sewModbusDT, by=c("ip.src"))[,.(ip.src)]
srcs
txt1 <- sprintf('"IP_SRC" : [%s]',
                paste0(
                  with(srcs,
                       sprintf('"%s"',
                               ip.src)),
                  collapse = ","))
txt1


# Destinations
# dst <- sewModbusDT[,.(count=.N), by=.(ip.dst)]
dst <- unique(sewModbusDT, by=c("ip.dst"))[,.(ip.dst)]
dst
txt2 <- sprintf('"IP_DST" : [%s]',
                paste0(
                  with(dst,
                       sprintf('"%s"',
                               ip.dst)),
                  collapse = ","))
txt2

# Destination / UnitID
# du <- sewModbusDT[,.(ip.dst.unit_id = paste(ip.dst, mbtcp.modbus.unit_id, sep="/")),
#                   by=.(ip.dst, mbtcp.modbus.unit_id)]
du <- unique(sewModbusDT,
             by=c("ip.dst",
                  "mbtcp.modbus.unit_id"))[
                    ,.(IP_MODBUS_UNIT_ID = paste(ip.dst,
                                                 mbtcp.modbus.unit_id, sep="/"),
                       ip.dst,
                       mbtcp.modbus.unit_id)]
du
txt3 <- sprintf('"IP_MODBUS_UNIT_ID" : [\n%s\n]',
                paste0(
                  with(du,
                       sprintf('  "%s" : {\n   "IP_ADDR" : "%s",\n   "UNIT_ID" : "%s "\n  }',
                               IP_MODBUS_UNIT_ID, ip.dst, mbtcp.modbus.unit_id)),
                  collapse = ",\n"))
txt3

# Source / MAC Address
#smac <- sewModbusDT[,.(count=.N), by=.(ip.src, eth.src)]
smac <- unique(sewModbusDT,
               by=c("ip.src", "eth.src"))[
                 ,.(IP_ADDR_MAC_ADDR = paste(ip.src, eth.src, sep="/"),
                    ip.src,
                    eth.src)]
smac
txt4 <- sprintf('"IP_ADDR_MAC_ADDR" : [\n%s\n]',
                paste0(
                  with(smac,
                       sprintf('  "%s" : {\n    "IP_ADDR" : "%s",\n   "MAC_ADDR" : "%s "\n  }',
                               IP_ADDR_MAC_ADDR, ip.src, eth.src)),
                  collapse = ",\n"))
txt4

# Source / Function Code
sfunc <- sewModbusDT[,.(IP_ADDR_MOD_FUNC = paste(ip.src, mbtcp.modbus.func_code, sep="/"))
                     , by=.(ip.src, mbtcp.modbus.func_code)][
                       ,.(IP_ADDR_MOD_FUNC, ip.src,
                         mbtcp.modbus.func_code)]
sfunc
txt5 <- sprintf('"IP_ADDR_MODBUS_FUNC" : [\n%s\n]',
                paste0(
                  with(sfunc,
                       sprintf('  "%s" : {\n    "IP_ADDR" : "%s",\n   "MODBUS_FUNCTION" : "%s "\n  }',
                               IP_ADDR_MOD_FUNC, ip.src, mbtcp.modbus.func_code)),
                  collapse = ",\n"))
txt5

# Source / Function Code
sfuncRef <- mergedSewDT[,.(IP_ADDR_MOD_FUNC_REF = paste(ip.src, mbtcp.modbus.func_code,
                                                mbtcp.modbus.reference_num, sep="/"))
                     , by=.(ip.src, mbtcp.modbus.func_code, mbtcp.modbus.reference_num)][
                       ,.(IP_ADDR_MOD_FUNC_REF,
                          ip.src,
                          mbtcp.modbus.func_code,
                          mbtcp.modbus.reference_num)][order(mbtcp.modbus.reference_num)]
sfuncRef
txt6 <- sprintf('"IP_ADDR_MODBUS_FUNC_REF" : [\n%s\n]',
                paste0(
                  with(sfuncRef,
                       sprintf('  "%s" : {\n    "IP_SRC" : "%s",\n     "MODBUS_FUNCTION" : "%s ",\n    "MODBUS_REFERENCE" : "%s "\n  }',
                               IP_ADDR_MOD_FUNC_REF, ip.src, mbtcp.modbus.func_code, mbtcp.modbus.reference_num)),
                  collapse = ",\n"))
txt6

whitelist <- sprintf('{\n%s\n}',
                     paste(txt1, txt2, txt3, txt4, txt5, txt6, sep=",\n")
)
write(whitelist, file="whitelist.db")
rm(txt1, txt2, txt3, txt4, txt5, txt6, sdu, srcs, dst, du, smac, sfunc, sfuncRef, whitelist)

# Packet Analysis

#### STATS ####
#Average frequency of packets per second
avgPkt <- mergedSewDT[,.(frequency=.N),by=frame.second][,mean(frequency)]
avgPkt

# Frequency per second, per source/dest ip and function code
srcFuncFreq <- mergedSewDT[,.(frequency=.N),
            by =.(ip.src, ip.dst, mbtcp.modbus.func_code,
                  frame.second)][
                    order(ip.src, ip.dst, mbtcp.modbus.func_code,
                          frame.second)][,.(avgFrequencySec=mean(frequency)),
                                         by=.(ip.src, ip.dst, mbtcp.modbus.func_code)]
srcFuncFreq
txt1 <- sprintf('"SOURCE_DEST_FUNCTION_FREQUENCY" : [\n%s\n]',
                paste0(
                  with(srcFuncFreq,
                       sprintf('  "%s" : {\n    "IP_SRC" : "%s",\n    "IP_DST" : "%s",\n    "MODBUS_FUNCTION" : "%s ",\n    "FREQUENCY" : "%f "\n  }',
                               paste(ip.src, ip.dst, mbtcp.modbus.func_code, sep="/"),
                               ip.src, ip.dst, mbtcp.modbus.func_code,
                               avgFrequencySec)),
                  collapse = ",\n"))
txt1

# Frequency per second, per source/dest ip, function code, and reference num
srcFuncRefFreq <- mergedSewDT[,.(frequency=.N),
            by =.(ip.src, ip.dst, mbtcp.modbus.func_code, mbtcp.modbus.reference_num,
                  frame.second)][
                    order(ip.src, ip.dst, mbtcp.modbus.func_code, mbtcp.modbus.reference_num,
                          frame.second)][,.(avgFrequncySec=mean(frequency)),
                                         by=.(ip.src, ip.dst, mbtcp.modbus.func_code,
                                              mbtcp.modbus.reference_num)]
srcFuncRefFreq
txt2 <- sprintf('"SOURCE_DEST_FUNCTION_REFERENCE_FREQUENCY" : [\n%s\n]',
                paste0(
                  with(srcFuncRefFreq,
                       sprintf('  "%s" : {\n    "IP_SRC" : "%s",\n    "IP_DST" : "%s",\n    "MODBUS_FUNCTION" : "%s ",\n    "MODBUS_REFERENCE" : "%s ",\n    "FREQUENCY" : "%f "\n  }',
                               paste(ip.src, ip.dst, mbtcp.modbus.func_code, mbtcp.modbus.reference_num, sep="/"),
                               ip.src, ip.dst, mbtcp.modbus.func_code,
                               mbtcp.modbus.reference_num,
                               avgFrequncySec)),
                  collapse = ",\n"))
txt2

modbusStats <- mergedSewDT[,.(frequency=.N, d.min=min(d), d.mean=mean(d, na.rm=T),
                              d.sd=sd(d, na.rm=T), d.max=max(d)
), by =.(mbtcp.modbus.func_code, mbtcp.modbus.reference_num)][
  order(mbtcp.modbus.func_code, mbtcp.modbus.reference_num)]
modbusStats
txt3 <- sprintf('"MODBUS_FUNCTION_REFERENCE_DATA_STATS" : [\n%s\n]',
                paste0(
                  with(modbusStats,
                       sprintf('  "%s" : {\n    "MODBUS_FUNCTION" : "%s",\n    "MODBUS_REFERENCE" : "%s",\n    "D_MIN" : "%.2f ",\n    "D_MEAN" : "%.2f ",\n    "D_STD_DEV" : "%.2f ",\n    "D_MAX" : "%.2f "\n  }',
                               paste(mbtcp.modbus.func_code, mbtcp.modbus.reference_num, sep="/"),
                               mbtcp.modbus.func_code,
                               mbtcp.modbus.reference_num,
                               d.min, d.mean, d.sd, d.max)),
                  collapse = ",\n"))
txt3
stats <- sprintf('{\n%s\n}',
                     paste(txt1, txt2, txt3, sep=",\n")
)
write(stats, file="stats.db")
rm(txt1, txt2, txt3)


# scatterplot
# ggplot(mergedSewDT, aes(resp.time.rel, d, color=factor(mbtcp.modbus.reference_num))) +
#   geom_point() + facet_grid(~resp.func.code) + ggtitle("Modbus Data Value (d) Over Time by Function Code")


# meltedSew <- melt(mergedSewDT, id=c("resp.time.rel","mbtcp.trans_id","resp.func.code", 
#                                     "mbtcp.modbus.reference_num", "d"
#                                     ), measure=c("mbtcp.modbus.reference_num"))

# trivariate plot
# cloud(resp.time.rel ~ factor(mbtcp.modbus.reference_num) * d | resp.func.code,
#             data = mergedSewDT[!(is.na(resp.data))], zlim = rev(range(mergedSewDT$resp.time.rel)),
#             screen = list(z = 135, x = -70), panel.aspect = 0.75, xlab = "d",
#             ylab = "refNum", zlab = "resp.time.rel",
#             main="3D of Reference Number by d Over Time Grouped by Function Code")

# with color
# cloud(resp.time.rel ~ d + mbtcp.modbus.reference_num | resp.func.code, data = mergedSewDT,
#       col.point = mergedSewDT$mbtcp.modbus.reference_num, pch= 19,
#       zlim = rev(range(mergedSewDT$resp.time.rel)), xlab = "d",
#       ylab = "refNum", zlab = "resp.time.rel", main="3D of Reference Number by d Over Time Grouped by Function Code",
#       key = list(points = list(pch = 19, col = seq_along(levels(mergedSewDT$mbtcp.modbus.reference_num))), 
#                  text = list(levels(mergedSewDT$mbtcp.modbus.reference_num)), space = 'top',
#                  columns = nlevels(mergedSewDT$mbtcp.modbus.reference_num)))
# 
# 
# vars <- ls()
# vars <- vars[-49]
# vars
# do.call(rm, as.list(vars))
#rm(vars)





## time x d1 x d2 per ref#
# cp <- cloud(resp.time.rel ~ d1 * d2 | mbtcp.modbus.reference_num,
#             data = mergedSewDT[!(is.na(mbtcp.modbus.reference_num))], zlim = rev(range(mergedSewDT$resp.time.rel)),
#             screen = list(z = 135, x = -60), panel.aspect = 0.75, xlab = "d2",
#             ylab = "d1", zlab = "resp.time.rel", main="3D of Time x d1 x d2")
# 
# cp


# ggplot(meltedSew, aes(x=resp.time.rel, y=value, colour=variable)) +
#   geom_point()



# original data in a 'wide' format
# x  <- seq(-2, 2, 0.05)
# y1 <- pnorm(x)
# 
# df2 <- melt(data = df, id.vars = "x")
# y2 <- pnorm(x, 1, 1)
# df <- data.frame(x, y1, y2)