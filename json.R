install.packages("rjson")
install.packages("jsonlite")
require(data.table)
require(rjson)
require(plyr)
require(jsonlite)


load("/home/lisa/rDia/sew.Rda")

unique(sewModbusDT, by=ip.src)


src <- sewModbusDT[,.(count=.N), by=.(ip.src)]
src$count<-NULL
srcJS <- toJSON(src[,.(ip.src=ip.src)])
srcJS


dst <- sewModbusDT[,.(count=.N), by=.(ip.dst)]
dst$count<-NULL
dstJS <- toJSON(dst[,.(ip.dst=ip.dst)])
dstJS

dstU <- sewModbusDT[,.(ip.dst.unit_id = paste(ip.dst, mbtcp.modbus.unit_id, sep="/")),
                    by=.(ip.dst, mbtcp.modbus.unit_id)]
dstU
dstUJS <- toJSON(dstU[,.(ip.dst.unit_id=ip.dst.unit_id)])
dstUJS

srcFunc <- sewModbusDT[,.(src.func = paste(ip.src, mbtcp.modbus.func_code, sep="/"))
                       , by=.(ip.src, mbtcp.modbus.func_code)]
srcFunc

sewModbusDT[,.(paste(ip.src, mbtcp.modbus.func_code, sep="/")=ip.src), by=.(ip.src, mbtcp.modbus.func_code)]

sewModbusDT[,.(paste(ip.src, mbtcp.modbus.func_code, sep="/")=ip.src), by=.(ip.src, mbtcp.modbus.func_code)]

sewModbusDT[,.(hi = paste(ip.src, mbtcp.modbus.func_code, sep="/")), by=.(ip.src, mbtcp.modbus.func_code)]

# this is how JSON should be, now how to create it???
dict<-list(ip.dst.unit_id = list("192.168.12.252/1"=list(ip="192.168.12.252", unitID="1"),
                                 "192.168.12.53/1"=list(ip="192.168.12.53", unitID="1"))
)
toJSON(dict)

confJS<- toJSON(list(src, dst))
confJS

a<-unique(mergedSewDT, by=c("ip.src"))[,.(ip.src)]
b<-unique(mergedSewDT, by=c("ip.dst"))[,.(ip.dst)]

unique(mergedSewDT, by=c("ip.dst", "mbtcp.modbus.unit_id"))[,.(ip.dst,mbtcp.modbus.unit_id)]

toJSON(list(IP_MOD_UNIT_ID=unique(mergedSewDT, by=c("ip.dst", "mbtcp.modbus.unit_id"))[,.(ip.dst,mbtcp.modbus.unit_id)]))

unique(mergedSewDT, by=c("ip.dst", "mbtcp.modbus.unit_id"))[,.(ip.dst,mbtcp.modbus.unit_id)]
ab<-unique(mergedSewDT, by=c("ip.dst", "mbtcp.modbus.unit_id"))[,.(ip.dst,mbtcp.modbus.unit_id,
                                                               ip_modbus_unit_id=paste(ip.dst,mbtcp.modbus.unit_id, sep="/"))]

getIPUnit <- function(unitID, ip, unit) {
  return(list("modUnit"=list(unitID= list("ip_addr"=ip, "modbus_unit"=unit) )))
}
toJSON(getIPUnit(eval(e)="192.168.12.252/1",ip="192.168.12.252",unit="1"))

getIPUnit <- function(unitID, ip, unit) {
  return(list("modUnit"=hash[[unitID]]<-list("ip_addr"=ip, "modbus_unit"=unit) ))
}

e<-parse(text="unitID")
e<- parse(text="getIPUnit(ip_modbus_unit_id, ip.dst, mbtcp.modbus.unit_id)")
ab[,.(eval(e))]




#e<-parse(text = paste("list(\"modUnit\"=list(",ip_modbus_unit_ID,"=list(\"ip_addr\"="ip.dst", \"modbus_unit\"=", unit,")))")
e<-parse(text = paste("list(\"modUnit\"=list(ip_modbus_unit_ID=list(\"ip_addr\"=ip.dst,\"modbus_unit\"=mbtcp.modbus.unit_id)))"))
e<-parse(text = paste("list(ip_modbus_unit_ID=list(\"ip_addr\"=ip.dst,\"modbus_unit\"=mbtcp.modbus.unit_id))"))

e=parse(text="list(\"ipMOD\"=ip_modbus_unit_ID)")

rDT<-ab[,eval(e)]
h<-hash()
h[["ip_modbus"]]<-list(ip="192.285.3",unit="1")


e<- parse(text = "paste(ip.dst,mbtcp.modbus.unit_id,sep=\"/\")")
rDT<- ab[, .(eval(e), ip.dst, mbtcp.modbus.unit_id) ]




v1<-"ip.dst"
v2<-"mbtcp.modbus.unit_id"
e<- parse(text = paste("paste(", v1, ",", v2, ",sep=\"/\")"))


(colnames(src), src$ip.src, colnames(dst), dst$ip.dst)

# USING hash to create JSON
## collect parameters
h<-hash()
h[[colnames(src)]]<-src$ip.src
h[[colnames(dst)]]<-dst$ip.dst

## convert to JSON
confJS <- toJSON(h)




Data <- data.frame(Time=Sys.time()+1:20,x=rnorm(20))
nSeconds <- 5

agg <- aggregate(Data[,1], by=list(as.numeric(Data$Time) %/% nSeconds), mean)
agg[,1] <- .POSIXct(agg[,1]*nSeconds)


xData<- xts(Data[,1], Data[,1])
period.apply(xData, endpoints(xData, "seconds", 5), colMeans)


ab<-unique(mergedSewDT, by=c("ip.dst", "mbtcp.modbus.unit_id"))[,.(ip.dst,mbtcp.modbus.unit_id,
                                                                   ip_modbus_unit_id=paste(ip.dst,mbtcp.modbus.unit_id, sep="/"))]


e=parse(text="list(\"ipMOD\"=list( quote(eval(parse(text=\"ip_modbus_unit_id\"))) =list(\"ip\"=ip.dst,\"unit\"=mbtcp.modbus.unit_id))")
e=parse(text="list(\"ipMOD\"=list( ip_modbus_unit_id = list(\"ip\"=ip.dst,\"unit\"=mbtcp.modbus.unit_id)))")

e=parse(text="list(ip_modbus_unit_id =(list(\"ip\"=ip.dst,\"unit\"=mbtcp.modbus.unit_id) ))")

f=parse(text="eval(ip_modbus_unit_id)")
e=parse(text="list(quote(f)=c(\"ip\"=ip.dst,\"unit\"=mbtcp.modbus.unit_id))")
rDT<-ab[,eval(e)]
rDT
toJSON(rDT)

ab[,eval(parse(text="ip_modbus_unit_id"))]
eval(parse(text="ip_modbus_unit_id"))


e=parse(text="ipUnit")
ipUnit<-"192.168.12.253/1"
eval(e)
f=parse(text="ip_modbus_unit_id=(list(\"ip\"=ip.dst,\"unit\"=mbtcp.modbus.unit_id) )")
rDT<-ab[,eval(f)]
rDT

x<-data.table(ipUnit=c("192.168.2.254/1","192.168.2.5/1"), ip=c("192.168.2.254","192.168.2.5"), unit=c("1","1"))
x
e=parse(text="list(IP_UNIT_ID=( list(ipUnit = list(\"IP_ADDR\"=ip,\"UNIT_ID\"=unit) )))")
toJSON(x[,eval(e)])


rm(dst,dstU,rDT,src,srcFunc,x,xDtata,confH,al,confJS,dict,dstJS,dstUJS,e,f,map,nSeconds,srcJS,v1,v2,ipUnit,ip_addr)





txt_ <- sprintf('"IP_UNIT_ID" : [\n%s\n]',
                paste0(
                  with(x, sprintf('    "%s" : {\n        "IP_ADDR" : "%s",\n        "UNIT_ID" : "%s "\n    }',
                                       ipUnit, ip, unit, unit)), collapse = ",\n"))
identical(txt, txt_)



sprintf('"IP_MODBUS_UNIT_ID" : [\n%s\n]')