install.packages("foreach", dependencies=TRUE)
install.packages("doParallel", dependencies=TRUE)

library(data.table)
library(iterators)
library(parallel)
library(foreach)
library(doParallel)

cl <- makeCluster(4)
registerDoParallel(cl)

# d10kDT <- moddataDT[1:10000,]
d10kDT <- moddataDT[1:100,]

# preallocate
numrows <- (nrow(d10kDT)/2)

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

m10kDT<- data.table(frame.number, frame.time_relative, frame.time_delta_displayed, frame.len,
                      ip.src, ip.dst, tcp.srcport, tcp.dstport, mbtcp.prot_id, mbtcp.trans_id,
                      mbtcp.len, mbtcp.modbus.func_code, mbtcp.modbus.word_cnt,
                      mbtcp.modbus.reference_num, resp.fr.number, resp.time.rel, resp.time.delta,
                      resp.len, resp.src, resp.dest, resp.srcport, resp.dstport, resp.prot_id,
                      resp.trans_id, resp.mbcp.len, resp.func.code, resp.data
)

idx <- 1

system.time(
  for (i in 1:nrow(d10kDT)) {
    pkt <- m10kDT[i,]
    #   print(paste("pkt :", i))
    #   print(pkt)
    
    #   if request (assumption is that we're starting with a request)
    if (pkt$tcp.dstport=="502") {
      m10kRow <- pkt[,.(frame.number, frame.time_relative, frame.time_delta_displayed, frame.len,
                          ip.src, ip.dst, tcp.srcport, tcp.dstport, mbtcp.prot_id,
                          mbtcp.trans_id, mbtcp.len, mbtcp.modbus.func_code, mbtcp.modbus.word_cnt,
                          mbtcp.modbus.reference_num)]
      setkey(m10kRow, mbtcp.trans_id)
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
      mergedRow <- m10kRow[addCols]
      #     print("mergedrow+addCols: ")
      #     print(mergedRow)
      
      m10kDT[idx, `:=`("frame.number"=mergedRow$frame.number, "frame.time_relative"=mergedRow$frame.time_relative,
                         "frame.time_delta_displayed"=mergedRow$frame.time_delta_displayed, "frame.len"=mergedRow$frame.len,
                         "ip.src"=mergedRow$ip.src, "ip.dst"=mergedRow$ip.dst, "tcp.srcport"=mergedRow$tcp.srcport,
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
    
  } # end for i in m10kDT
) # end system.time


rm(pkt, m10kRow, addCols, i, idx)


m10kDT[, d:=as.integer(paste("0x", gsub(":", "", resp.data), sep=""))]

m10kDT$resp.func.code <- factor(mergedDT$resp.func.code)
m10kDT$mbtcp.modbus.reference_num <- factor(mergedDT$mbtcp.modbus.reference_num)


# initial stats
setkey(m10kDT, resp.func.code, mbtcp.modbus.reference_num)

m10kStats <- m10kDT[,.(count=.N, d.min=min(d), d.mean=mean(d, na.rm=T), d.max=max(d),
                            d.sd=sd(d, na.rm=T), min.resp.time.rel=min(resp.time.rel),min.resp.time.rel= max(resp.time.rel)),
                         by=.(resp.func.code, mbtcp.modbus.reference_num)]


m10kStats




# parallel

idx <- 1

strt <- Sys.time()

pkt<-NULL
#   for (i in 1:nrow(d10kDT)) {
foreach (i = 1:nrow(d10kDT)) %dopar% {
  pkt <- d10kDT[i,]
  if (pkt$tcp.dstport=="502") {
    print(paste("pkt$tcp.dstport ::", pkt) )
    d10kDT[i,.(frame.number, frame.time_relative, frame.time_delta_displayed, frame.len,
                                         ip.src, ip.dst, tcp.srcport, tcp.dstport, mbtcp.prot_id,
                                         mbtcp.trans_id, mbtcp.len, mbtcp.modbus.func_code, mbtcp.modbus.word_cnt,
                                         mbtcp.modbus.reference_num)]
    print("dest port 502")
  }
}
    
  #   if request (assumption is that we're starting with a request)
#   if (pkt$tcp.dstport=="502") {
#       m10kRow <- pkt[,.(frame.number, frame.time_relative, frame.time_delta_displayed, frame.len,
#                           ip.src, ip.dst, tcp.srcport, tcp.dstport, mbtcp.prot_id,
#                           mbtcp.trans_id, mbtcp.len, mbtcp.modbus.func_code, mbtcp.modbus.word_cnt,
#                           mbtcp.modbus.reference_num)]
#       setkey(m10kRow, mbtcp.trans_id)
#       #     print(paste("mergedRow: ", i))
#       #     print(mergedRow)
#   } # end if request
    
    # get next row, should be response
#     if (pkt$tcp.srcport=="502") {
#       # set response fields in mergedRow
#       addCols <- pkt[,.(resp.fr.number=frame.number, resp.time.rel=frame.time_relative, 
#                         resp.time.delta=frame.time_delta_displayed,
#                         resp.len=frame.len, resp.src=ip.src, resp.dest=ip.dst, resp.srcport=tcp.srcport,
#                         resp.dstport=tcp.dstport, resp.prot_id=mbtcp.prot_id, resp.trans_id=mbtcp.trans_id,
#                         resp.mbcp.len=mbtcp.len, resp.func.code=mbtcp.modbus.func_code,
#                         resp.data=mbtcp.modbus.data)]
#       setkey(addCols, resp.trans_id)
#       #     print(paste("addCols: ", i))
#       #     print(addCols)
# 
#       m10kRow <- m10kRow[addCols]
#       #     print("mergedrow+addCols: ")
#       #     print(mergedRow)
#       
#       # create new row
#       m10kDT[idx, `:=`("frame.number"=m10kRow$frame.number, "frame.time_relative"=m10kRow$frame.time_relative,
#                          "frame.time_delta_displayed"=m10kRow$frame.time_delta_displayed, "frame.len"=m10kRow$frame.len,
#                          "ip.src"=m10kRow$ip.src, "ip.dst"=m10kRow$ip.dst, "tcp.srcport"=m10kRow$tcp.srcport,
#                          "tcp.dstport"=m10kRow$tcp.dstport, "mbtcp.prot_id"=m10kRow$mbtcp.prot_id,
#                          "mbtcp.trans_id"=m10kRow$mbtcp.trans_id, "mbtcp.len"=m10kRow$mbtcp.len,
#                          "mbtcp.modbus.func_code"=m10kRow$mbtcp.modbus.func_code,
#                          "mbtcp.modbus.word_cnt"=m10kRow$mbtcp.modbus.word_cnt,
#                          "mbtcp.modbus.reference_num"=m10kRow$mbtcp.modbus.reference_num,
#                          "resp.fr.number"=m10kRow$resp.fr.number, "resp.time.rel"=m10kRow$resp.time.rel,
#                          "resp.time.delta"=m10kRow$resp.time.delta, "resp.len"=m10kRow$resp.len,
#                          "resp.src"=m10kRow$resp.src, "resp.dest"=m10kRow$resp.dest,
#                          "resp.srcport"=m10kRow$resp.srcport, "resp.dstport"=m10kRow$resp.dstport,
#                          "resp.prot_id"=m10kRow$resp.prot_id, "resp.trans_id"=m10kRow$mbtcp.trans_id,
#                          "resp.mbcp.len"=m10kRow$resp.mbcp.len, "resp.func.code"=m10kRow$resp.func.code,
#                          "resp.data"=m10kRow$resp.data)
#                ]
#       
#       idx <- idx+1
#     }
    
} # end for i in m10kDT

totT <- Sys.time() - strt
totT

idx <- 1

system.time(
  print("start")
  foreach (i = 1:nrow(d10kDT)) %dopar% {
    print(i)
    pkt <- d10kDT[i,]

  } # end for i in m10kDT
) # end system.time
