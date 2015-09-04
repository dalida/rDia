
require(data.table)

# 1- load data/stats from normal pcap
#   - ~/scada/sew.sh => sew.data

# - load data/stats from attack pcap
#   - ~/scada/attack.sh => attack.data
#   - attackModbusDT
#   - mergedAttackDT

# - make comparison


# attackModbusDT

datafile <- "~/scada/attack.data"

attackModbusDT <- as.data.table(
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

attackModbusDT <- attackModbusDT[!(is.na(frame.number))]
attackModbusDT <- attackModbusDT[!(is.na(mbtcp.modbus.unit_id))]
attackModbusDT <- attackModbusDT[!(is.na(mbtcp.trans_id))]
attackModbusDT <- attackModbusDT[!(is.na(mbtcp.modbus.reference_num))]

save(attackModbusDT, file="attack.Rda")
rm(datafile)

numrows <- (nrow(attackModbusDT)/2)
#numrows<-500

# frame.number<-numeric(numrows)
# frame.time_relative<-numeric(numrows)
# frame.time_delta<-numeric(numrows)
# frame.len<-numeric(numrows)
# ip.src <- character(numrows)
# eth.src <- character(numrows)
# ip.dst <- character(numrows)
# eth.dst <- character(numrows)
# mbtcp.modbus.unit_id <- character(numrows)
# tcp.srcport <- character(numrows)
# tcp.dstport <- character(numrows)
# mbtcp.prot_id <- character(numrows)
# mbtcp.trans_id <- numeric(numrows)
# mbtcp.len <- numeric(numrows)
# mbtcp.modbus.func_code <- character(numrows)
# mbtcp.modbus.word_cnt <- numeric(numrows)
# mbtcp.modbus.reference_num <- character(numrows)
# frame.second <-numeric(numrows)
# resp.frame.number <- numeric(numrows) 
# resp.time.rel <- numeric(numrows)
# resp.time.delta <- numeric(numrows)
# resp.len <- numeric(numrows)
# resp.ip.src <- character(numrows)
# resp.eth.src <- character(numrows)
# resp.ip.dst <- character(numrows)
# resp.eth.dst <- character(numrows)
# resp.srcport <- character(numrows)
# resp.dstport<- character(numrows)
# resp.unit_id<- character(numrows)
# resp.prot_id <- character(numrows)
# resp.trans_id <- numeric(numrows)
# resp.mbcp.len <- numeric(numrows)
# resp.func.code <- character(numrows)
# resp.second <-numeric(numrows)
# resp.data <- character(numrows)
# d <- character(numrows)
# 
# mergedAttackDT<- data.table(frame.number, frame.time_relative, frame.time_delta, frame.len,
#                          ip.src, eth.src, ip.dst, eth.dst, mbtcp.modbus.unit_id, tcp.srcport, tcp.dstport,
#                          mbtcp.prot_id, mbtcp.trans_id, mbtcp.len, mbtcp.modbus.func_code,
#                          mbtcp.modbus.word_cnt, frame.second, mbtcp.modbus.reference_num, resp.frame.number,
#                          resp.time.rel, resp.time.delta, resp.len, resp.ip.src, resp.ip.dst,
#                          resp.srcport, resp.unit_id, resp.dstport, resp.prot_id, resp.trans_id,
#                          resp.mbcp.len, resp.func.code, resp.second, resp.data
# )
# 
# idx <- 1
# 
# system.time(
#   for (i in 1:nrow(attackModbusDT)) {
#     #  for (i in 1:(numrows*2)) {
#     pkt <- attackModbusDT[i,]
#     #   print(paste("pkt :", i))
#     #   print(pkt)
#     
#     # TODO: need to check transactionID are the same, right now this is an assumption
#     
#     #   if request (assumption is that we're starting with a request)
#     if (pkt$tcp.dstport=="502") {
#       mergedAttackRow <- pkt[,.(frame.number, frame.time_relative, frame.time_delta,
#                              frame.len, ip.src, eth.src, ip.dst, eth.dst, mbtcp.modbus.unit_id,
#                              tcp.srcport, tcp.dstport, mbtcp.prot_id, mbtcp.trans_id, mbtcp.len,
#                              mbtcp.modbus.func_code, mbtcp.modbus.word_cnt, 
#                              mbtcp.modbus.reference_num)]
#       
#       setkey(mergedAttackRow, mbtcp.trans_id)
#       #     print(paste("mergedattackRow: ", i))
#       #     print(mergedattackRow)
#     } # end if request
#     
#     # get next row, should be response
#     if (pkt$tcp.srcport=="502") {
#       # set response fields in mergedattackRow
#       addCols <- pkt[,.(resp.frame.number=frame.number, resp.time.rel=frame.time_relative, 
#                         resp.time.delta=frame.time_delta,
#                         resp.len=frame.len, resp.ip.src=ip.src, resp.eth.src=eth.src,
#                         resp.ip.dst=ip.dst, resp.eth.dst=eth.dst,
#                         resp.unit_id=mbtcp.modbus.unit_id, resp.srcport=tcp.srcport, 
#                         resp.dstport=tcp.dstport, resp.prot_id=mbtcp.prot_id, 
#                         resp.trans_id=mbtcp.trans_id, resp.mbcp.len=mbtcp.len,
#                         resp.func.code=mbtcp.modbus.func_code, resp.data=mbtcp.modbus.data)]
#       
#       setkey(addCols, resp.trans_id)
#       #     print(paste("addCols: ", i))
#       #     print(addCols)
#       
#       # create new row
#       mergedAttackRow <- mergedAttackRow[addCols]
#       #     print("mergedattackrow+addCols: ")
#       #     print(mergedattackRow)
#       
#       mergedAttackDT[idx, `:=`("frame.number"=mergedAttackRow$frame.number,
#                             "frame.time_relative"=mergedAttackRow$frame.time_relative,
#                             "frame.time_delta"=mergedAttackRow$frame.time_delta,
#                             "frame.len"=mergedAttackRow$frame.len, "ip.src"=mergedAttackRow$ip.src,
#                             "eth.src"=mergedAttackRow$eth.src,  "ip.dst"=mergedAttackRow$ip.dst,
#                             "eth.dst"=mergedAttackRow$eth.dst,
#                             "mbtcp.modbus.unit_id"=mergedAttackRow$mbtcp.modbus.unit_id,
#                             "tcp.srcport"=mergedAttackRow$tcp.srcport, "tcp.dstport"=mergedAttackRow$tcp.dstport,
#                             "mbtcp.prot_id"=mergedAttackRow$mbtcp.prot_id,
#                             "mbtcp.trans_id"=mergedAttackRow$mbtcp.trans_id, "mbtcp.len"=mergedAttackRow$mbtcp.len,
#                             "mbtcp.modbus.func_code"=mergedAttackRow$mbtcp.modbus.func_code,
#                             "mbtcp.modbus.word_cnt"=mergedAttackRow$mbtcp.modbus.word_cnt,
#                             "mbtcp.modbus.reference_num"=mergedAttackRow$mbtcp.modbus.reference_num,
#                             "resp.frame.number"=mergedAttackRow$resp.frame.number, "resp.time.rel"=mergedAttackRow$resp.time.rel,
#                             "resp.time.delta"=mergedAttackRow$resp.time.delta, "resp.len"=mergedAttackRow$resp.len,
#                             "resp.ip.src"=mergedAttackRow$resp.ip.src, "resp.eth.src"=mergedAttackRow$resp.eth.src,
#                             "resp.ip.dst"=mergedAttackRow$resp.ip.dst, "resp.eth.dst"=mergedAttackRow$resp.eth.dst,
#                             "resp.unit_id"=mergedAttackRow$resp.unit_id,
#                             "resp.srcport"=mergedAttackRow$resp.srcport, "resp.dstport"=mergedAttackRow$resp.dstport,
#                             "resp.prot_id"=mergedAttackRow$resp.prot_id, "resp.trans_id"=mergedAttackRow$mbtcp.trans_id,
#                             "resp.mbcp.len"=mergedAttackRow$resp.mbcp.len, "resp.func.code"=mergedAttackRow$resp.func.code,
#                             "resp.data"=mergedAttackRow$resp.data)
#                   ]
#       
#       idx <- idx+1
#       rm(mergedAttackRow)
#     } # end srcport=502
#     rm(pkt,addCols)
#   } # end for i in attackModbusDT
# ) # end system.time
# 
# 
# rm(mergedAttackRow, i, idx)
# 
# # convert hex to decimal
# mergedAttackDT[, d:=as.integer(paste("0x", gsub(":", "", resp.data), sep=""))]
# # seconds
# mergedAttackDT$frame.second <- floor(mergedAttackDT$frame.time_relative)
# mergedAttackDT$resp.second <- floor(mergedAttackDT$resp.time.rel)
# 
# # factorize
# mergedAttackDT$ip.src <- factor(mergedAttackDT$ip.src)
# mergedAttackDT$ip.dst <- factor(mergedAttackDT$ip.dst)
# mergedAttackDT$mbtcp.modbus.unit_id <- factor(mergedAttackDT$mbtcp.modbus.unit_id)
# mergedAttackDT$tcp.srcport <- factor(mergedAttackDT$tcp.srcport)
# mergedAttackDT$tcp.dstport <- factor(mergedAttackDT$tcp.dstport)
# mergedAttackDT$mbtcp.prot_id <- factor(mergedAttackDT$mbtcp.prot_id)
# mergedAttackDT$mbtcp.modbus.func_code<- factor(mergedAttackDT$mbtcp.modbus.func_code)
# mergedAttackDT$resp.ip.src <- factor(mergedAttackDT$resp.ip.src)
# mergedAttackDT$resp.eth.src <- factor(mergedAttackDT$resp.eth.src)
# mergedAttackDT$resp.ip.dst <- factor(mergedAttackDT$resp.ip.dst)
# mergedAttackDT$resp.eth.dst <- factor(mergedAttackDT$resp.eth.dst)
# mergedAttackDT$resp.unit_id <- factor(mergedAttackDT$resp.unit_id)
# mergedAttackDT$resp.srcport <- factor(mergedAttackDT$resp.srcport)
# mergedAttackDT$resp.dstport <- factor(mergedAttackDT$resp.dstport)
# mergedAttackDT$resp.prot_id <- factor(mergedAttackDT$resp.prot_id)
# mergedAttackDT$resp.data <- factor(mergedAttackDT$resp.data)
# mergedAttackDT$resp.func.code <- factor(mergedAttackDT$resp.func.code)
# mergedAttackDT$mbtcp.modbus.reference_num <- factor(mergedAttackDT$mbtcp.modbus.reference_num)
# 
# mergedAttackDT <- mergedAttackDT[ip.src!=""]
# 
# save(mergedAttackDT, file="attackMerged.Rda")
