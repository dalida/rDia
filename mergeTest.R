

mergedDT <- NULL
system.time(
 
  for (i in 1:10) {
    pkt <- moddataDT[i,]
    mergedRow <- NULL
    
    if (pkt$tcp.dstport=="502") {
      #      set request fields in mergedRow: frame.time_relative, frame.time_delta_displayed, frame.len,
      #           ip.src, ip.dst, ip.hdr_len, tcp.srcport, tcp.dstport, mbtcp.prot_id,
      #           mbtcp.trans_id, mbtcp.len, mbtcp.modbus.func_code, mbtcp.modbus.reference_num,
      #           mbtcp.modbus.word_cnt, mbtcp.modbus.data
      mergedRow <- pkt[,.(frame.time_relative, frame.time_delta_displayed, frame.len,
                          ip.src, ip.dst, tcp.srcport, tcp.dstport, mbtcp.prot_id,
                          mbtcp.trans_id, mbtcp.len, mbtcp.modbus.func_code, mbtcp.modbus.word_cnt,
                          mbtcp.modbus.reference_num), key="mbtcp.trans_id"]
      
    } # end if request
    
    # get next row, should be response
#     if (pkt$tcp.srcport=="502") {
#       # set response fields in mergedRow
#       mergedRow[, pkt[,.(resp.time.rel=frame.time_relative, resp.time.delta=frame.time_delta_displayed,
#                          resp.len=frame.len, resp.src=ip.src, resp.dest=ip.dst, resp.srcport=tcp.srcport,
#                          resp.dstport=tcp.dstport, resp.prot_id=mbtcp.prot_id, resp.trans.id=mbtcp.trans_id,
#                          resp.mbcp.len=mbtcp.len, resp.func.code=mbtcp.modbus.func_code,
#                          resp.data=mbtcp.modbus.data), key="mbtcp.trans_id" ]]
# #       mergedRow <- cbind(mergedRow, pkt[,.(resp.time.rel=frame.time_relative, resp.time.delta=frame.time_delta_displayed,
# #                                            resp.len=frame.len, resp.src=ip.src, resp.dest=ip.dst, resp.srcport=tcp.srcport,
# #                                            resp.dstport=tcp.dstport, resp.prot_id=mbtcp.prot_id, resp.trans.id=mbtcp.trans_id,
# #                                            resp.mbcp.len=mbtcp.len, resp.func.code=mbtcp.modbus.func_code,
# #                                            resp.data=mbtcp.modbus.data), key="mbtcp.trans_id"])
# 
#       # add mergedRow to mergedDT
#       l = list(mergedDT, mergedRow)
#       mergedDT <- rbindlist(l, use.names=TRUE, fill=TRUE)
#     }
    
  } # end for (i in n)
)

l<-list("hi", "hello")
c<-c("col1","col2")
x <- data.table(a=1:3,b=1:6) 
f <- function(x) {list("hi", "hello")} 
x[,c("col1","col2"):=f(), by=a][]

x[,c("mean","sum"):=list(mean(b),sum(b)),by=a][]


frame.number<-numeric(500)
frame.time_relative<-numeric(500)
frame.time._delta_displayed<-numeric(500)
frame.len<-numeric(500)
mbtcp.len<-numeric(500)
afactor<-factor(NA)
merged<- data.table(frame.number, frame.time_relative,frame.time._delta_displayed,frame.len,mbtcp.len,afactor)


# split resp.data -> a,b
# system.time(
#   merged10k<-separate(merged10k, resp.data, c("a","b"), ":", remove=F, extra="merge")
# )
# 
# system.time(
#   subD<-separate(subD, resp.data, c("a","b"), ":", remove=F, extra="merge")
# )
# 
# system.time(
#   uni<-unite(merged10k, resp.data, c("a","b"), ":")
#   )
# 
# system.time(
#   merged10k$Pa <- as.character(lapply(strsplit(as.character(mergedDT$resp.data), split=":"), "[", 1))
#   merged10k$Pb <- as.character(lapply(strsplit(as.character(mergedDT$resp.data), split=":"), "[", 2))
# )

# get min and max start time

# merged10k$Pa <- as.integer(paste("0x", merged10k$PX, sep=""))

merged10k[, d:=as.integer(paste("0x", gsub(":", "", resp.data), sep=""))]

# merged10k[,c("pa","pb") := NULL]
# merged10k[, ':=' (pa = as.integer(paste("0x", PX, sep="")),
#                   pb = as.integer(paste("0x", PY, sep=""))
#                   )]

## time x ref2 x ref3
cp <- cloud(resp.time.rel ~ merged10k[mbtcp.modbus.reference_num=="2",pb] * 
              merged10k[mbtcp.modbus.reference_num=="3", pb],
            data = merged10k[!(is.na(resp.data))], zlim = rev(range(merged10k$resp.time.rel)),
            screen = list(z = 105, x = -70), panel.aspect = 0.75, xlab = "pb2",
            ylab = "pb3", zlab = "resp.time.rel", main="3D of Time x refNum2 x refNum3")

## time x d1 x d2 per ref#
cp <- cloud(resp.time.rel ~ pa * pb | mbtcp.modbus.reference_num,
            data = merged10k[!(is.na(resp.data))], zlim = rev(range(merged10k$resp.time.rel)),
            panel.aspect = 0.75, xlab = "pb",
            ylab = "pa", zlab = "resp.time.rel", main="3D of Time x d1 x d2")

cp



melted10k <- melt(merged10k, id=c("resp.time.rel","mbtcp.trans_id"), measure=c("mbtcp.modbus.reference_num", "pa", "pb"))

ggplot(data = melted10k, aes(x = resp.time.rel, y=value)) + geom_point()

# some basic dummy data
DF <- data.frame(x = runif(10),
                 y = runif(10), 
                 z = runif(10), 
                 group = sample(letters[1:3],10, replace = TRUE))

cloud(z~x+y, data = DF, pch= 19, col.point = DF$group, 
      key = list(points = list(pch = 19, col = seq_along(levels(DF$group))), 
                 text = list(levels(DF$group)), space = 'top', columns = nlevels(DF$group)))

cloud(z~x+y, data = DF, pch= 19, col.point = DF$group)
