#### ####
#### Install packages ####

install.packages("data.table")
install.packages("lattice.extra")
install.packages("plyr")
install.packages("dplyr")

install.packages("TRSbook")
install.packages("igraph")
install.packages("tcltk")
install.packages("rgl")
install.packages("ape")

library(data.table)
library(lattice)
library(latticeExtra)
library(plyr)
library(dplyr)

library(reshape2)
library(RColorBrewer)

library(TRSbook)
library(igraph)
library(tcltk)
library(tgl)
library(ape)

#### Read in data - entire pcap file ####
scadaDT <- as.data.table(read.csv("~/Bureau/data/scadaCops/SCADA_20150429_csv", stringsAsFactors=TRUE))


#### Describe data ####
str(scadaDT)
> str(scadaDT)
# Classes ‘data.table’ and 'data.frame':  3566892 obs. of  7 variables:
#   $ No.        : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Time       : num  0 1 1.76 1.76 2 ...
# $ Source     : Factor w/ 41 levels "0e:10:ff:b7:dc:26",..: 33 33 12 12 33 12 12 6 12 9 ...
# $ Destination: Factor w/ 37 levels "192.168.100.2",..: 32 32 18 18 32 18 18 7 19 14 ...
# $ Protocol   : Factor w/ 28 levels "ARP","BROWSER",..: 16 16 25 25 16 25 25 28 5 7 ...
# $ Length     : int  56 56 62 62 56 62 62 82 76 104 ...
# $ Info       : Factor w/ 2398392 levels "0 > asa-appl-proto [<None>] Seq=174781686 Win=512 Len=0",..: 1217902 1217903 1387261 1387261 1217904 1387261 1387261 1450729 1453023 1428177 ...
# - attr(*, ".internal.selfref")=<externalptr> 
summary(scadaDT)
# > summary(scadaDT)
# No.               Time                     Source                Destination            Protocol           Length       
# Min.   :      1   Min.   :    0   192.168.12.51    :2004927   192.168.12.253:1882254   TCP       :1692588   Min.   :  42.00  
# 1st Qu.: 891724   1st Qu.: 7250   AdeptTec_5c:74:1e: 743837   Broadcast     : 746874   Modbus/TCP: 825521   1st Qu.:  54.00  
# Median :1783446   Median :14018   192.168.12.253   : 412802   192.168.12.51 : 680298   ARP       : 751226   Median :  54.00  
# Mean   :1783446   Mean   :11400   192.168.12.252   : 277302   192.168.12.252: 143086   T.125     : 277283   Mean   :  60.05  
# 3rd Qu.:2675169   3rd Qu.:14102   192.168.50.50    :  76642   83.133.119.197:  66226   HTTP      :  11275   3rd Qu.:  65.00  
# Max.   :3566892   Max.   :16616   192.168.12.90    :  22641   192.168.12.128:  15201   DNS       :   2525   Max.   :1514.00  
# (Other)          :  28741   (Other)       :  32953   (Other)   :   6474                    
# Info        
# detachUserRequest                                                 : 277276  
# Continuation or non-HTTP traffic                                  :   5635  
# [TCP Retransmission] Continuation or non-HTTP traffic             :   4970  
# query: trans:     0; unit:   1, func:   4: Read input registers:   1568  
# query: trans:   100; unit:   1, func:   4: Read input registers:   1568  
# query: trans:   101; unit:   1, func:   4: Read input registers:   1568  
# (Other):3274307


###########  EXPLORATORY  ###########

### PROTOCOLS

# ptm <- proc.time()
# protocols <- as.table(sort(table(scadaDT[,Protocol]), decreasing=TRUE)) # protocols sorted from greatest
# proc.time() - ptm
# protocolsDT <- as.data.table(protocols)

protocols <- sort(table(scadaDT$Protocol),decreasing=TRUE)
protocols
# as.matrix(protocols)
# > protocols <- sort(table(scadaDT$Protocol),decreasing=TRUE)
# > #proc.time() - ptm
# > protocols
# 
# TCP   Modbus/TCP          ARP        T.125         HTTP          DNS          SMB          UDP         IMAP 
# 1692588       825521       751226       277283        11275         2525         1007          861          849 
# TLSv1         SMTP         ICMP         NBNS       PN-DCP       DHCPv6       Syslog      BROWSER         SSDP 
# 575          533          526          491          364          273          246          181          168 
# LLMNR       LANMAN         NBSS         MDNS       DCERPC RELOAD Frame       REMACT       SRVSVC          IMF 
# 128          108           80           28           21           14            6            6            5 
# TPKT 
# 4
rm(protocols)

scadaDT[, .(Count=.N), by=.(Protocol)][order(-Count)]
scadaDT[, .(Count=.N), by=Protocol][order(-Count)]
# pie chart
# plot.new()
# lbls <- paste(names(protocols), "\n", protocols, sep="")
# par(mfrow=c(1,1))
# pie(protocols, labels=lbls, main="Distribution by Protocols")


# camembert
protocol <- scadaDT$Protocol
camembert(protocol)
rm(protocol)

camembert(scadaDT[Protocol])

camembert(scadaDT$Protocol)
camembert(scadaDT[,Protocol])

# stacked
sbar <- ggplot(scadaDT, aes(x=Source, fill=Protocol)) + geom_bar()
sbar <- sbar + ggtitle("Frequency of Packets by Source/Protocol") + ylab("Frequency")
sbar + theme(axis.text.x = element_text(angle=90))
rm(sbar)

#### Plots ####
scadaDT[,(plot(Protocol, main="Number of Packets by Protocol", xlab="Protocol", ylab="Frequency"))]

### LENGTH

summary(scadaDT[,Length])
# > summary(scadaDT[,Length])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 42.00   54.00   54.00   60.05   65.00 1514.00
#scadaDT[,(plot(Length, main="Length"))]
scadaDT[,(boxplot(Length~Protocol, main="Packet Length by Protocol", xlab="Protocol", ylab="Bytes"))]

boxplot(scadaDT[Protocol=="Modbus/TCP"]$Length, main="Boxplot of Length for Modbus/TCP Packets", ylab="bytes")
boxplot(scadaDT[Protocol=="TCP"]$Length, main="Boxplot of Length for TCP Packets", ylab="bytes")
boxplot(scadaDT[Protocol=="ARP"]$Length, main="Boxplot of Length for ARP Packets", ylab="bytes")


# Number of Packets by Source
bc <- ggplot(scadaDT, aes(x=Source)) + geom_bar(stat="bin", fill="#FF9999", colour="black")
bc <- bc + ggtitle("Bar Chart of Packets by Source") + ylab("Packet Count")
bc + theme(axis.text.x = element_text(angle=90))
rm(bc)

scadaDT[, .(Count=.N) ,by=Source]
# scadaDT[,(plot(Source, main="Frequency by Source", xlab="Source", ylab="Frequency"))]

# > scadaDT[, .(Count=.N) ,by=Source]
#                       Source   Count
# 1:          Siemens_45:5c:e4     643
# 2:             192.168.50.50   76642
# 3:             192.168.12.51 2004927
# 4:              192.168.50.1     361
# 5:         Spectrum_18:78:cd     777
# 6:            Intel_03:5f:5b    1305
# 7:            192.168.12.252  277302
# 8:            Intel_af:1c:7d     969
# 9:             192.168.50.11    1905
# 10:           Vmware_73:7a:76     403
# 11:              Nsi_1e:00:6e     638
# 12:            Intel_bb:66:28    1014
# 13:            192.168.12.250    9636
# 14:            192.168.12.128    7126
# 15:            192.168.12.253  412802
# 16:         Veritech_37:9b:5b    1783
# 17:         Telemeca_0f:35:aa      80
# 18: fe80::c8fd:ac99:e724:26e9     421
# 19:            83.133.119.197    1251
# 20:         DlgAutom_56:ec:6d      81
# 21:             192.168.12.90   22641
# 22:            Intel_f7:82:bc      42
# 23:             192.168.50.18     216
# 24:         AdeptTec_5c:74:1e  743837
# 25:  fe80::250:b6ff:fe4f:d6c0       4
# 26: fe80::488e:75ff:fea0:2422       4
# 27: fe80::7cca:f2ff:fe61:5a77       4
# 28: fe80::88d0:fcff:fec2:77b3       4
# 29: fe80::58aa:15ff:febb:a322       4
# 30:  fe80::e88b:eff:fe6b:bfc5       4
# 31: fe80::b4cf:74ff:feba:c2cd       4
# 32:             192.168.12.80      44
# 33:         0e:10:ff:b7:dc:26       2
# 34:         26:73:f5:ee:88:a8       2
# 35:         2e:8e:45:86:ec:5a       2
# 36:         GoodWayI_4f:d6:c0       2
# 37:           Vmware_c0:00:00       2
# 38:           Vmware_c0:00:06       2
# 39:         2a:84:83:48:30:2b       2
# 40:           Vmware_c0:00:02       2
# 41:           Vmware_c0:00:01       2

# Number of Packets by Destination
bc <- ggplot(scadaDT, aes(x=Destination)) + geom_bar(stat="bin", fill="#56B4E9", colour="black")
bc <- bc + ggtitle("Bar Chart of Packets by Destination") + ylab("Packet Count")
bc + theme(axis.text.x = element_text(angle=90))
rm(bc)

scadaDT[, .(Count=.N), by=Destination]
# scadaDT[,(plot(Destination, main="Frequency by Destination", xlab="Destination", ylab="Frequency"))]
# > scadaDT[, .(Count=.N) ,by=Destination]
#          Destination   Count
# 1:    PN-MC_00:00:00     364
# 2:    83.133.119.197   66226
# 3:    192.168.12.255     673
# 4:           8.8.8.8     620
# 5:     192.168.50.50     365
# 6:    Intel_03:5f:5b    1304
# 7: Spectrum_18:78:cd     763
# 8:     192.168.12.51  680298
# 9:    192.168.12.252  143086
# 10:    Intel_af:1c:7d     716
# 11:     192.168.100.2    1905
# 12:         Broadcast  746874
# 13:   Vmware_73:7a:76     366
# 14:   255.255.255.255     430
# 15:     192.168.50.11    2281
# 16:    192.168.50.255     376
# 17:    192.168.12.128   15201
# 18:      Nsi_1e:00:6e     505
# 19:    192.168.12.250    8342
# 20: Veritech_37:9b:5b     304
# 21:    192.168.12.253 1882254
# 22:         ff02::1:2     273
# 23: DlgAutom_56:ec:6d      63
# 24:     192.168.12.90   11237
# 25:           ff02::c      84
# 26:   239.255.255.250      84
# 27:    Intel_f7:82:bc      38
# 28:     192.168.12.16     197
# 29:     192.168.50.18    1168
# 30:    Intel_bb:66:28     233
# 31: Telemeca_0f:35:aa      24
# 32:          ff02::fb      28
# 33:         ff02::1:3      64
# 34:       224.0.0.252      64
# 35:  Siemens_45:5c:e4       1
# 36: AdeptTec_5c:74:1e      35
# 37:     192.168.12.80      46

# Number of Packets by Source/Protocol
scadaDT[, .(Count=.N), by=.(Source,Protocol)]
# > scadaDT[, .(Count=.N) , by=.(Source,Protocol)]
#                       Source     Protocol   Count
# 1:          Siemens_45:5c:e4       PN-DCP     364
# 2:             192.168.50.50          TCP   63000
# 3:             192.168.12.51          UDP     861
# 4:             192.168.50.50          DNS     620
# 5:              192.168.50.1         ICMP     361
# 6:         Spectrum_18:78:cd          ARP     777
# 7:            Intel_03:5f:5b          ARP    1305
# 8:            192.168.12.252          TCP  138664
# 9:             192.168.12.51          TCP 1463781
# 10:            Intel_af:1c:7d          ARP     969
# 11:             192.168.50.11          DNS    1905
# 12:          Siemens_45:5c:e4          ARP     279
# 13:           Vmware_73:7a:76          ARP     403
# 14:             192.168.50.50         IMAP     334
# 15:             192.168.50.50      BROWSER      94
# 16:             192.168.50.50         ICMP       4
# 17:              Nsi_1e:00:6e          ARP     638
# 18:            Intel_bb:66:28          ARP    1014
# 19:            192.168.12.250         ICMP     129
# 20:            192.168.12.128         ICMP       8
# 21:             192.168.50.50         NBNS     282
# 22:            192.168.12.253          TCP     178
# 23:         Veritech_37:9b:5b          ARP    1783
# 24:         Telemeca_0f:35:aa          ARP      80
# 25: fe80::c8fd:ac99:e724:26e9       DHCPv6     273
# 26:            192.168.12.128      BROWSER      23
# 27:            192.168.12.128         NBNS      23
# 28:             192.168.12.51         NBNS      59
# 29:            192.168.12.128          TCP    6534
# 30:            192.168.12.128         NBSS      49
# 31:             192.168.12.51         NBSS      31
# 32:            192.168.12.128          SMB     426
# 33:             192.168.12.51          SMB     301
# 34:            192.168.12.128       LANMAN      54
# 35:             192.168.12.51       LANMAN      54
# 36:            83.133.119.197          TCP     198
# 37:            83.133.119.197         IMAP     515
# 38:             192.168.12.51      BROWSER      41
# 39:         DlgAutom_56:ec:6d          ARP      81
# 40:             192.168.12.90       Syslog      49
# 41: fe80::c8fd:ac99:e724:26e9         SSDP      84
# 42:             192.168.12.90         SSDP      84
# 43:             192.168.12.90      BROWSER      23
# 44:            192.168.12.250          TCP    9035
# 45:             192.168.12.51         TPKT       2
# 46:            192.168.12.252         TPKT       2
# 47:             192.168.12.51   Modbus/TCP  401150
# 48:             192.168.12.51        T.125  138647
# 49:            192.168.12.253   Modbus/TCP  412604
# 50:            192.168.12.252        T.125  138636
# 51:            Intel_f7:82:bc          ARP      42
# 52:             192.168.50.18         NBNS      19
# 53:            192.168.12.250         NBNS      12
# 54:             192.168.50.18       Syslog     197
# 55:            192.168.12.253         ICMP      20
# 56:            83.133.119.197         SMTP     533
# 57:            83.133.119.197          IMF       5
# 58:             192.168.50.50         HTTP   11275
# 59:            192.168.12.250          SMB     140
# 60:             192.168.50.50          SMB     140
# 61:            192.168.12.250       DCERPC       6
# 62:            192.168.12.128       DCERPC       9
# 63:             192.168.50.50       DCERPC       6
# 64:            192.168.12.250       REMACT       3
# 65:             192.168.50.50       REMACT       3
# 66:            192.168.12.250       SRVSVC       3
# 67:             192.168.50.50       SRVSVC       3
# 68:            192.168.12.250 RELOAD Frame       8
# 69:             192.168.50.50 RELOAD Frame       6
# 70:         AdeptTec_5c:74:1e          ARP  743837
# 71:             192.168.12.90          TCP   11162
# 72:             192.168.12.90   Modbus/TCP   11163
# 73:            192.168.12.250   Modbus/TCP     300
# 74:             192.168.50.50   Modbus/TCP     300
# 75:  fe80::250:b6ff:fe4f:d6c0         MDNS       4
# 76: fe80::488e:75ff:fea0:2422         MDNS       4
# 77: fe80::7cca:f2ff:fe61:5a77         MDNS       4
# 78: fe80::88d0:fcff:fec2:77b3         MDNS       4
# 79: fe80::58aa:15ff:febb:a322         MDNS       4
# 80:  fe80::e88b:eff:fe6b:bfc5         MDNS       4
# 81: fe80::b4cf:74ff:feba:c2cd         MDNS       4
# 82: fe80::c8fd:ac99:e724:26e9        LLMNR      64
# 83:             192.168.12.90        LLMNR      64
# 84:             192.168.12.90         NBNS      96
# 85:             192.168.50.50        TLSv1     575
# 86:             192.168.12.80          TCP      36
# 87:             192.168.12.80   Modbus/TCP       4
# 88:             192.168.12.80         ICMP       4
# 89:         0e:10:ff:b7:dc:26          ARP       2
# 90:         26:73:f5:ee:88:a8          ARP       2
# 91:         2e:8e:45:86:ec:5a          ARP       2
# 92:         GoodWayI_4f:d6:c0          ARP       2
# 93:           Vmware_c0:00:00          ARP       2
# 94:           Vmware_c0:00:06          ARP       2
# 95:         2a:84:83:48:30:2b          ARP       2
# 96:           Vmware_c0:00:02          ARP       2
# 97:           Vmware_c0:00:01          ARP       2

# Number of Packets by Destination/Protocol
scadaDT[, .(Count=.N), by=.(Destination,Protocol)]
# > scadaDT[, .(Count=.N) , by=.(Destination,Protocol)]
#          Destination     Protocol   Count
# 1:    PN-MC_00:00:00       PN-DCP     364
# 2:    83.133.119.197          TCP   54951
# 3:    192.168.12.255          UDP     431
# 4:           8.8.8.8          DNS     620
# 5:     192.168.50.50         ICMP     365
# 6:    Intel_03:5f:5b          ARP    1304
# 7: Spectrum_18:78:cd          ARP     763
# 8:     192.168.12.51          TCP  140133
# 9:    192.168.12.252          TCP    4437
# 10:    Intel_af:1c:7d          ARP     716
# 11:     192.168.100.2          DNS    1905
# 12:         Broadcast          ARP  746874
# 13:   Vmware_73:7a:76          ARP     366
# 14:   255.255.255.255          UDP     430
# 15:     192.168.50.11          TCP     894
# 16:     192.168.50.11         IMAP     849
# 17:    192.168.50.255      BROWSER      94
# 18:    192.168.12.128         ICMP       8
# 19:      Nsi_1e:00:6e          ARP     505
# 20:    192.168.12.250         ICMP      16
# 21:    192.168.50.255         NBNS     282
# 22: Veritech_37:9b:5b          ARP     304
# 23:    192.168.12.253          TCP 1469317
# 24:         ff02::1:2       DHCPv6     273
# 25:    192.168.12.255      BROWSER      87
# 26:    192.168.12.255         NBNS     155
# 27:    192.168.12.128         NBNS      23
# 28:    192.168.12.128          TCP   14466
# 29:     192.168.12.51         NBSS      31
# 30:    192.168.12.128         NBSS      31
# 31:     192.168.12.51          SMB     301
# 32:    192.168.12.128          SMB     581
# 33:     192.168.12.51       LANMAN      54
# 34:    192.168.12.128       LANMAN      54
# 35: DlgAutom_56:ec:6d          ARP      63
# 36:    192.168.12.250       Syslog      49
# 37:     192.168.12.90         ICMP      49
# 38:           ff02::c         SSDP      84
# 39:   239.255.255.250         SSDP      84
# 40:    192.168.12.250          TCP    7818
# 41:    192.168.12.252         TPKT       2
# 42:     192.168.12.51         TPKT       2
# 43:    192.168.12.253   Modbus/TCP  412917
# 44:    192.168.12.252        T.125  138647
# 45:     192.168.12.51   Modbus/TCP  401141
# 46:     192.168.12.51        T.125  138636
# 47:    Intel_f7:82:bc          ARP      38
# 48:    192.168.12.253         NBNS      20
# 49:     192.168.12.16       Syslog     197
# 50:     192.168.50.18         ICMP      88
# 51:     192.168.50.11         SMTP     533
# 52:     192.168.50.11          IMF       5
# 53:    83.133.119.197         HTTP   11275
# 54:    192.168.12.250          SMB     125
# 55:    192.168.12.250         NBSS      18
# 56:    192.168.12.250         NBNS       7
# 57:    Intel_bb:66:28          ARP     233
# 58:    192.168.12.128       DCERPC      12
# 59:    192.168.12.250       DCERPC       9
# 60:    192.168.12.128       REMACT       6
# 61:    192.168.12.128       SRVSVC       6
# 62:    192.168.12.128 RELOAD Frame      14
# 63:     192.168.12.90          TCP      29
# 64:     192.168.12.90   Modbus/TCP   11159
# 65:    192.168.12.250   Modbus/TCP     300
# 66: Telemeca_0f:35:aa          ARP      24
# 67:          ff02::fb         MDNS      28
# 68:         ff02::1:3        LLMNR      64
# 69:       224.0.0.252        LLMNR      64
# 70:  Siemens_45:5c:e4          ARP       1
# 71:     192.168.50.18          TCP     505
# 72:     192.168.50.18        TLSv1     575
# 73: AdeptTec_5c:74:1e          ARP      35
# 74:     192.168.12.80          TCP      38
# 75:     192.168.12.80   Modbus/TCP       4
# 76:     192.168.12.80         NBNS       4

### MODBUS PACKETS

summary(scadaDT[Protocol=="Modbus/TCP"])
# > summary(scadaDT[Protocol=="Modbus/TCP"])
# No.               Time                    Source               Destination           Protocol          Length     
# Min.   :   2408   Min.   :  706.3   192.168.12.253:412604   192.168.12.253:412917   Modbus/TCP:825521   Min.   : 64.0  
# 1st Qu.: 579452   1st Qu.: 3838.2   192.168.12.51 :401150   192.168.12.51 :401141   ARP       :     0   1st Qu.: 65.0  
# Median : 921409   Median : 7648.8   192.168.12.90 : 11163   192.168.12.90 : 11159   BROWSER   :     0   Median : 66.0  
# Mean   :1058519   Mean   : 7753.9   192.168.12.250:   300   192.168.12.250:   300   DCERPC    :     0   Mean   : 65.7  
# 3rd Qu.:1149872   3rd Qu.:10759.7   192.168.50.50 :   300   192.168.12.80 :     4   DHCPv6    :     0   3rd Qu.: 66.0  
# Max.   :3566858   Max.   :16616.4   192.168.12.80 :     4   192.168.100.2 :     0   DNS       :     0   Max.   :315.0  
# (Other)       :     0   (Other)       :     0   (Other)   :     0                  
# Info       
# query: trans:     0; unit:   1, func:   4: Read input registers:  1568  
# query: trans:   100; unit:   1, func:   4: Read input registers:  1568  
# query: trans:   101; unit:   1, func:   4: Read input registers:  1568  
# query: trans:   102; unit:   1, func:   4: Read input registers:  1568  
# query: trans:   103; unit:   1, func:   4: Read input registers:  1568  
# query: trans:   104; unit:   1, func:   4: Read input registers:  1568  
# (Other)                                                          :816113  

# Number of Modbus/TCP packets per Source
scadaDT[Protocol=="Modbus/TCP", .(Count=.N), by=.(Source, Protocol)]
# > scadaDT[Protocol=="Modbus/TCP", .(Count=.N), by=.(Source, Protocol)]
#            Source   Protocol  Count
# 1: 192.168.12.253 Modbus/TCP 412604
# 2:  192.168.12.51 Modbus/TCP 401150
# 3:  192.168.12.90 Modbus/TCP  11163
# 4: 192.168.12.250 Modbus/TCP    300
# 5:  192.168.50.50 Modbus/TCP    300
# 6:  192.168.12.80 Modbus/TCP      4

# Number of Modbus/TCP packets per destination
scadaDT[Protocol=="Modbus/TCP", .(Count=.N), by=.(Destination, Protocol)]
# > scadaDT[Protocol=="Modbus/TCP", .(Count=.N), by=.(Destination,Protocol)]
#       Destination   Protocol  Count
# 1: 192.168.12.250 Modbus/TCP    300
# 2: 192.168.12.253 Modbus/TCP 412917
# 3:  192.168.12.51 Modbus/TCP 401141
# 4:  192.168.12.80 Modbus/TCP      4
# 5:  192.168.12.90 Modbus/TCP  11159


summary(scadaDT[Protocol=="Modbus/TCP"]$Length)
# > summary(scadaDT[Protocol=="Modbus/TCP"]$Length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 64.0    65.0    66.0    65.7    66.0   315.0

scadaDT[,(plot(Protocol,main="Number of Packets by Protocol", xlab="Protocol", ylab="Frequency"))]

# Boxplots
require(lattice)
plot.new()
boxLatModbusLengthSource <- 
  bwplot(~Length | Source, data=scadaDT[Protocol=="Modbus/TCP"], as.table=TRUE,
         scales="free", main="Boxplot of Modbus/TCP Packet Lengths by Source",
         xlab="Packet Length", layout=c(2,3))
print(boxLatModbusLengthSource, more=TRUE)
rm(boxLatModbusLengthSource)

plot.new()
boxLatModbusLengthDestination <- 
  bwplot(~Length | Destination, data=scadaDT[Protocol=="Modbus/TCP"], as.table=TRUE,
         scales="free", main="Boxplot of Modbus/TCP Packet Lengths by Destination",
         xlab="Packet Length", layout=c(2,3))
print(boxLatModbusLengthDestination, more=TRUE)
rm(boxLatModbusLengthDestination)

plot.new()
boxLatLengthProtocol <- 
  bwplot(~Length | Protocol, data=scadaDT, as.table=TRUE,
         scales="free", main="Boxplots of Packet Lengths by Protocol",
         xlab="bytes", layout=c(4,7))
print(boxLatLengthProtocol, more=TRUE)
rm(boxLatLengthProtocol)

# Pie charts
newplot()
setkey(scadaDT, Protocol, Source)
p = ggplot(
  count(scadaDT[.("Modbus/TCP")], vars="Source"),
  aes(x="", y=freq, fill=Source))
p = p + geom_bar(width=1, stat="identity")
p = p + coord_polar(theta="y")
p = p + xlab('') + ylab('')
p = p + ggtitle("Modbus/TCP Packets by Source")
p
rm(p)

newplot()
setkey(scadaDT, Protocol, Destination)
p = ggplot(
  count(scadaDT[.("Modbus/TCP")], vars="Destination"),
  aes(x="", y=freq, fill=Destination))
p = p + geom_bar(width=1, stat="identity")
p = p + coord_polar(theta="y")
p = p + xlab('') + ylab('')
p = p + ggtitle("Modbus/TCP Packets by Destination")
p
rm(p)

rm(scadaDT)



#### Read in data - TCP Endpoints pcap file ####
scadaEndPtsDT <- as.data.table(read.csv("~/Bureau/data/scadaCops/SCADA_Security_042915_TCP_Endpoints.csv"))

setkey(scadaEndPtsDT, Address, Port)
str(scadaEndPtsDT)
# > str(scadaEndPtsDT)
# Classes ‘data.table’ and 'data.frame':  70220 obs. of  10 variables:
#   $ Address   : Factor w/ 11 levels "192.168.12.128",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Port      : int  21 22 23 25 42 49 69 80 81 102 ...
# $ Packets   : int  510 27 27 27 27 27 27 27 27 2 ...
# $ Bytes     : int  30260 1602 1602 1602 1602 1602 1602 1602 1602 128 ...
# $ Tx.Packets: int  170 9 9 9 9 9 9 9 9 1 ...
# $ Tx.Bytes  : int  9180 486 486 486 486 486 486 486 486 54 ...
# $ Rx.Packets: int  340 18 18 18 18 18 18 18 18 1 ...
# $ Rx.Bytes  : int  21080 1116 1116 1116 1116 1116 1116 1116 1116 74 ...
# $ Latitude  : Factor w/ 1 level "-": 1 1 1 1 1 1 1 1 1 1 ...
# $ Longitude : Factor w/ 1 level "-": 1 1 1 1 1 1 1 1 1 1 ...
# - attr(*, ".internal.selfref")=<externalptr> 
#   - attr(*, "sorted")= chr  "Address" "Port"

summary(scadaEndPtsDT)
# > summary(scadaEndPtsDT)
# Address           Port          Packets            Bytes            Tx.Packets        Tx.Bytes       
# 192.168.12.51 :65536   Min.   :    0   Min.   :      1   Min.   :6.20e+01   Min.   :     0   Min.   :       0  
# 192.168.50.50 : 2996   1st Qu.:12970   1st Qu.:     22   1st Qu.:1.19e+03   1st Qu.:    22   1st Qu.:    1188  
# 192.168.12.250: 1361   Median :30472   Median :     22   Median :1.19e+03   Median :    22   Median :    1188  
# 192.168.12.128:  286   Mean   :30795   Mean   :     80   Mean   :5.18e+03   Mean   :    40   Mean   :    2590  
# 192.168.12.80 :   14   3rd Qu.:48004   3rd Qu.:     22   3rd Qu.:1.19e+03   3rd Qu.:    22   3rd Qu.:    1188  
# 83.133.119.197:   14   Max.   :65535   Max.   :2295012   Max.   :1.34e+08   Max.   :412780   Max.   :26981315  
# (Other)       :   13                                                                                           
# Rx.Packets         Rx.Bytes        Latitude  Longitude
# Min.   :      0   Min.   :0.00e+00   -:70220   -:70220  
# 1st Qu.:      0   1st Qu.:0.00e+00                      
# Median :      0   Median :0.00e+00                      
# Mean   :     40   Mean   :2.59e+03                      
# 3rd Qu.:      0   3rd Qu.:0.00e+00                      
# Max.   :1882232   Max.   :1.07e+08 

scadaEndPtsDT[,.(NumberOfPorts=(.N)), by=Address]
# > scadaEndPtsDT[,.(NumberOfPorts=(.N)), by=Address]
#           Address NumberOfPorts
# 1: 192.168.12.128           286
# 2: 192.168.12.250          1361
# 3: 192.168.12.252             1
# 4: 192.168.12.253             2
# 5:  192.168.12.51         65536
# 6:  192.168.12.80            14
# 7:  192.168.12.90             7
# 8:  192.168.50.11             2
# 9:  192.168.50.18             1
# 10:  192.168.50.50          2996
# 11: 83.133.119.197            14

qplot(scadaEndPtsDT[,Address], geom="histogram", binwidth = 0.5, main = "Number of Ports Per Address",
      xlab = "Address", ylab = "Number of Ports", fill = I("blue"), alpha = I(".2"), col = I("red")) +
      theme(axis.text.x = element_text(angle=90))

setkey(scadaEndPtsDT, Address, Port, Packets)
scadaEndPtsDT[.(Address, Port, Packets), .(TotalPackets=sum(Packets)), by = .(Address)]
# > scadaEndPtsDT[.(Address, Port, Packets), .(TotalPackets=sum(Packets)), by = .(Address)]
#           Address TotalPackets
# 1: 192.168.12.128        22242
# 2: 192.168.12.250        17765
# 3: 192.168.12.252       420388
# 4: 192.168.12.253      2295016
# 5:  192.168.12.51      2684264
# 6:  192.168.12.80           82
# 7:  192.168.12.90        33513
# 8:  192.168.50.11         2281
# 9:  192.168.50.18         1080
# 10:  192.168.50.50        75840
# 11: 83.133.119.197        67675

# Bar plots
# bp <- ggplot(scadaEndPtsDT[,Address, by=Packets], aes(x=Address, y=Packets, fill=Address)) + geom_bar(stat="identity") +
#   scale_fill_brewer(palette="Setl")
# bp  + ggtitle("Number of Packets Per Address")
# rm(bp)

hp <- ggplot(scadaEndPtsDT[,Address, by=Packets], aes(x=Address, y=Packets/1e3, fill=Address)) + geom_bar(stat="identity")
  + scale_fill_brewer(palette="Set1")
hp <- hp + ggtitle("Number of Packets Per Address") + ylab("Packets (x 1e3)")
hp + theme(axis.text.x = element_text(angle=90), legend.position="none")
rm(hp)

setkey(scadaEndPtsDT, Address, Port, Bytes)
scadaEndPtsDT[.(Address, Port, Bytes), .(TotalBytes=sum(Bytes)), by = .(Address)]
# > scadaEndPtsDT[.(Address, Port, Bytes), .(TotalBytes=sum(Bytes)), by = .(Address)]
#           Address TotalBytes
# 1: 192.168.12.128    7924237
# 2: 192.168.12.250    5023679
# 3: 192.168.12.252   30620524
# 4: 192.168.12.253  133587888
# 5:  192.168.12.51  162198756
# 6:  192.168.12.80       4976
# 7:  192.168.12.90    2191339
# 8:  192.168.50.11    1160812
# 9:  192.168.50.18     337679
# 10:  192.168.50.50   11494014
# 11: 83.133.119.197    9168886

# bp <- ggplot(scadaEndPtsDT[,Address, by=Bytes], aes(x=Address, y=Bytes, fill=Address)) + geom_bar(stat="identity") +
#   scale_fill_brewer()
# bp  + ggtitle("Number of Bytes Per Address")
# rm(bp)

bp <- ggplot(scadaEndPtsDT[,Address, by=Bytes], aes(x=Address, y=Bytes/1e6, fill=Address)) + geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set2")
bp <- bp  + ggtitle("Number of Bytes Per Address") + ylab("MBytes")
bp +  theme(axis.text.x = element_text(angle=90), legend.position="none")
rm(bp)

rm(scadaEndPtsDT)


#### Read in data - TCP Endpoints pcap file ####
#scadaTCPConvDT <- as.data.table(read.csv("~/Bureau/data/scadaCops/SCADA_Security_042915_TCP_Conversations.csv"))
scadaTCPConvDT <- as.data.table(read.csv("~/Bureau/data/scadaCops/SCADA_Security_042915_TCP_Conversations.csv", header=TRUE,
                                         na.strings="NA", quote="\"", dec=",", colClass=c(Port.A="factor", Port.B="factor")))


str(scadaTCPConvDT)
# > str(scadaTCPConvDT)
# Classes ‘data.table’ and 'data.frame':  76485 obs. of  14 variables:
# $ Address.A    : Factor w/ 8 levels "192.168.12.128",..: 8 8 8 8 8 8 8 4 8 8 ...
# $ Port.A       : Factor w/ 65034 levels "1000","10000",..: 20758 20758 20758 20758 20758 20758 20758 11221 20758 20758 ...
# $ Address.B    : Factor w/ 9 levels "192.168.12.128",..: 8 8 8 8 8 8 8 3 8 8 ...
# $ Port.B       : Factor w/ 2009 levels "0","1","10","100",..: 9 9 9 9 9 10 10 7 10 10 ...
# $ Packets      : int  6 6 6 6 6 6 6 45 6 6 ...
# $ Bytes        : int  372 372 372 372 372 372 372 2430 372 372 ...
# $ Packets.A.B  : int  0 0 0 0 0 0 0 23 0 0 ...
# $ Bytes.A.B    : int  0 0 0 0 0 0 0 1242 0 0 ...
# $ Packets.A.B.1: int  6 6 6 6 6 6 6 22 6 6 ...
# $ Bytes.A.B.1  : int  372 372 372 372 372 372 372 1188 372 372 ...
# $ Rel.Start    : num  1.76 2.72 3.7 4.8 5.89 ...
# $ Duration     : num  0.96 0.983 1.092 1.094 0.983 ...
# $ bps.A.B      : num  NA NA NA NA NA ...
# $ bps.A.B.1    : num  3099 3026 2724 2721 3028 ...
# - attr(*, ".internal.selfref")=<externalptr> 

summary(scadaTCPConvDT)
# > summary(scadaTCPConvDT)
# Address.A         Port.A               Address.B         Port.B         Packets             Bytes           Packets.A.B          Bytes.A.B       
# 192.168.12.51 :65495   2887   : 2170   192.168.12.253:65087   502    :65527   Min.   :     1.0   Min.   :      62   Min.   :     0.00   Min.   :       0  
# 192.168.50.50 : 5906   502    :  503   83.133.119.197: 5498   2887   : 5494   1st Qu.:    21.0   1st Qu.:    1134   1st Qu.:    21.00   1st Qu.:    1134  
# 192.168.12.128: 2159   14846  :  135   192.168.50.50 : 3159   21     :  133   Median :    22.0   Median :    1188   Median :    22.00   Median :    1188  
# 83.133.119.197: 2107   15810  :   75   192.168.12.250: 1505   443    :  108   Mean   :    36.7   Mean   :    2378   Mean   :    27.22   Mean   :    1674  
# 192.168.12.253:  502   20589  :   75   192.168.12.128:  556   139    :   74   3rd Qu.:    22.0   3rd Qu.:    1188   3rd Qu.:    22.00   3rd Qu.:    1188  
# 192.168.12.250:  295   3695   :   62   192.168.12.51 :  535   445    :   49   Max.   :476770.0   Max.   :31071169   Max.   :245207.00   Max.   :16019750  
# (Other)       :   21   (Other):73465   (Other)       :  145   (Other): 5100                                                                               
# Packets.A.B.1        Bytes.A.B.1         Rel.Start            Duration          bps.A.B          bps.A.B.1        
# Min.   :     0.00   Min.   :       0   Min.   :    1.759   Min.   :    0.0   Min.   :      1   Min.   :      0.2  
# 1st Qu.:     0.00   1st Qu.:       0   1st Qu.:13967.350   1st Qu.:  131.8   1st Qu.:     72   1st Qu.:   1397.2  
# Median :     0.00   Median :       0   Median :13969.123   Median :  131.9   Median :     72   Median :   1571.4  
# Mean   :     9.53   Mean   :     703   Mean   :12981.764   Mean   :  114.5   Mean   :   1691   Mean   :  15064.7  
# 3rd Qu.:     0.00   3rd Qu.:       0   3rd Qu.:13970.987   3rd Qu.:  131.9   3rd Qu.:     72   3rd Qu.:   2724.6  
# Max.   :235628.00   Max.   :15800911   Max.   :16616.189   Max.   :15558.1   Max.   :5604520   Max.   :2708835.0  
# NA's   :3685      NA's   :70984  

# Scatter plots
# xyp <- ggplot(scadaTCPConvDT, aes(x=Rel.Start, y=Port.A)) + geom_point(shape=1)
# xyp + facet_grid(. ~ Address.A)

# Scatter plots
xyp <- ggplot(scadaTCPConvDT, aes(x=Rel.Start, y=Port.A)) + geom_point(shape=1, col="red", alpha=0.5)
xyp <- xyp + facet_wrap( ~ Address.A, ncol=3, scales="free")
xyp <- xyp + theme(axis.ticks = element_blank(), axis.text.y = element_blank(),
                   panel.border = element_rect(fill=NA, size=0.25), legend.position="none")
xyp <- xyp + labs(title="Scatter Plot of Source and Start Time by Address", y="Source Port", x="Start Time")
xyp
rm(xyp)

xyp <- ggplot(scadaTCPConvDT, aes(x=Rel.Start, y=Port.B)) + geom_point(shape=1, colour="blue", alpha=0.5)
xyp <- xyp + facet_wrap( ~ Address.B, ncol=3, scales="free")
xyp <- xyp + theme(axis.ticks = element_blank(), axis.text.y = element_blank(),
                   panel.border = element_rect(fill=NA, size=0.25), legend.position="none")
xyp <- xyp + labs(title="Scatter Plot of Destination and Start Time by Address", y="Destination Port", x="Start Time")
xyp
rm(xyp)


# bxp <- ggplot(scadaTCPConvDT, aes(Address.A, Duration))
# bxp + geom_boxplot()
# 
# bxp <- ggplot(scadaTCPConvDT, aes(Port.A, Duration)) + geom_boxplot()
# bxp

# bxp <- ggplot(scadaTCPConvDT, aes(Port.A, Duration)) +  geom_boxplot()
# bxp <- bxp + facet_grid(. ~ Address.A)
# bxp
# 
# scadaTCPConvDT[,bwplot(Port.A~Duration | Address.A, main="Boxplot of Duration Size by Source Port/Address",
#                        scales = list(y=list(relation="free")
#                        ))]


# old scatter plots
# plot.new()
# scadaTCPConvDT[,xyplot(Port.A~Rel.Start | Address.A, main="Scatter Plot of Source and Start Time by Address",
#                        xlab="Start Time", ylab="Source Port", layout=c(3,3), col="red",
#                        scales = list(y=list(relation="free", tick.number=4, limits=c(0,60000))
#                        ))]
# 
# 
# plot.new()
# scadaTCPConvDT[,xyplot(Port.B~Rel.Start | Address.B, main="Scatter Plot of Destination and Start Time by Address",
#                        xlab="Start Time", ylab="Destination Port", col="blue", layout=c(3,3),
#                        scales = list(y=list(relation="free", tick.number=5, limits=c(0,2000))
#                        ))]

# Box plots
plot.new()
scadaTCPConvDT[,bwplot(~Bytes/1e6 | Address.A, main="Boxplot of Source and Packet Size by Address",
                       xlab="MBytes", col="red", layout=c(3,3), 
                       scales = list(y=list(relation="free")
                       ))]

plot.new()
scadaTCPConvDT[,bwplot(~Bytes/1e6 | Address.B, main="Boxplot of Destination and Packet Size by Address",
                       xlab="MBytes", layout=c(3,3),
                       scales = list(y=list(relation="free")
                       ))]


## Histogram of number of packets by Destination/Source
# this is questionnable
x <- scadaTCPConvDT[,Packets]
bw <- diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))
hp <- ggplot(scadaTCPConvDT, aes(x=Packets)) + geom_histogram(binwidth=bw, fill="grey28", colour="black")
hp <- hp + facet_grid(Address.B ~ Address.A, scales="free_y")
hp <- hp + ggtitle("Packet Count per Destination/Source") + ylab("Count")
hp + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.text.y = element_text(angle=0))
rm(x, bw, hp)



# 
# In the first argument we indicate that the dataframe is summary.mtc, next we indicate in the aes() statement
# that the x-axis is gear and the y-axis is meanwt, and finally we add the geom_bar() layer.
# We use the geom_bar(stat=“identity”) to indicate that we want the y-values to be exactly the values in the dataset.
# Remember, by default the stat is set to stat=“bin” which is a count of the x-axis variable,
# so it's important to change it when we have summarized our data.

# qplot(scadaEndPtsDT[,Address], geom="histogram", binwidth = 0.5, main = "Number of Ports Per Address",
#       xlab = "Address", ylab = "Number of Ports", fill = I("blue"), alpha = I(".2"), col = I("red"))


# correlation
obs<-scadaTCPConvDT[,.(Packets,Bytes,Packets.A.B,Bytes.A.B,Packets.A.B.1,Bytes.A.B.1,Duration,bps.A.B,bps.A.B.1)]
cor(obs, use="complete.obs",method="spearman")
cor(obs,method="spearman")

# > obs<-scadaTCPConvDT[,.(Packets,Bytes,Packets.A.B,Bytes.A.B,Packets.A.B.1,Bytes.A.B.1,Duration,bps.A.B,bps.A.B.1)]
# > cor(obs)
# Packets     Bytes Packets.A.B Bytes.A.B Packets.A.B.1 Bytes.A.B.1  Duration bps.A.B bps.A.B.1
# Packets       1.0000000 0.9954044   0.9848243 0.9894129     0.9888339   0.9807663 0.3656115      NA        NA
# Bytes         0.9954044 1.0000000   0.9721946 0.9881901     0.9912479   0.9904929 0.3793788      NA        NA
# Packets.A.B   0.9848243 0.9721946   1.0000000 0.9891334     0.9479642   0.9374376 0.3913442      NA        NA
# Bytes.A.B     0.9894129 0.9881901   0.9891334 1.0000000     0.9657124   0.9577159 0.4033520      NA        NA
# Packets.A.B.1 0.9888339 0.9912479   0.9479642 0.9657124     1.0000000   0.9942389 0.3346695      NA        NA
# Bytes.A.B.1   0.9807663 0.9904929   0.9374376 0.9577159     0.9942389   1.0000000 0.3502280      NA        NA
# Duration      0.3656115 0.3793788   0.3913442 0.4033520     0.3346695   0.3502280 1.0000000      NA        NA
# bps.A.B              NA        NA          NA        NA            NA          NA        NA       1        NA
# bps.A.B.1            NA        NA          NA        NA            NA          NA        NA      NA         1
# > cor(obs, use="complete.obs")
# Packets        Bytes  Packets.A.B    Bytes.A.B Packets.A.B.1  Bytes.A.B.1     Duration      bps.A.B
# Packets        1.000000000  0.997361624  0.984977213  0.997275992   0.988957843  0.983266702  0.539043699 -0.004199670
# Bytes          0.997361624  1.000000000  0.973343973  0.991565687   0.994101977  0.993307603  0.541378239 -0.004099281
# Packets.A.B    0.984977213  0.973343973  1.000000000  0.994793627   0.948509616  0.940398921  0.569452849 -0.004239428
# Bytes.A.B      0.997275992  0.991565687  0.994793627  1.000000000   0.975536887  0.969960481  0.558796218 -0.003907224
# Packets.A.B.1  0.988957843  0.994101977  0.948509616  0.975536887   1.000000000  0.996521528  0.500044846 -0.004065033
# Bytes.A.B.1    0.983266702  0.993307603  0.940398921  0.969960481   0.996521528  1.000000000  0.518163736 -0.004212189
# Duration       0.539043699  0.541378239  0.569452849  0.558796218   0.500044846  0.518163736  1.000000000 -0.007531572
# bps.A.B       -0.004199670 -0.004099281 -0.004239428 -0.003907224  -0.004065033 -0.004212189 -0.007531572  1.000000000
# bps.A.B.1     -0.003211298 -0.002975338 -0.003474098 -0.002988144  -0.002908902 -0.002921650 -0.007224129  0.999326308
# bps.A.B.1
# Packets       -0.003211298
# Bytes         -0.002975338
# Packets.A.B   -0.003474098
# Bytes.A.B     -0.002988144
# Packets.A.B.1 -0.002908902
# Bytes.A.B.1   -0.002921650
# Duration      -0.007224129
# bps.A.B        0.999326308
# bps.A.B.1      1.000000000
# > cor(obs, use="complete.obs",method="spearman")
# Packets       Bytes Packets.A.B  Bytes.A.B Packets.A.B.1 Bytes.A.B.1    Duration    bps.A.B  bps.A.B.1
# Packets        1.00000000  0.97894124   0.9788233  0.5040848    0.89660460   0.4077640 -0.04869179  0.2755771  0.2534123
# Bytes          0.97894124  1.00000000   0.9165046  0.4720734    0.92313903   0.4206617 -0.02865136  0.2503647  0.2290456
# Packets.A.B    0.97882329  0.91650460   1.0000000  0.5149616    0.83109826   0.3770389 -0.06698470  0.2899822  0.2673998
# Bytes.A.B      0.50408481  0.47207338   0.5149616  1.0000000    0.42692965  -0.5593192 -0.28164421  0.7759541 -0.3608199
# Packets.A.B.1  0.89660460  0.92313903   0.8310983  0.4269296    1.00000000   0.4756859  0.02599502  0.1968267  0.1914597
# Bytes.A.B.1    0.40776399  0.42066174   0.3770389 -0.5593192    0.47568586   1.0000000  0.27201179 -0.5562958  0.5958265
# Duration      -0.04869179 -0.02865136  -0.0669847 -0.2816442    0.02599502   0.2720118  1.00000000 -0.7473304 -0.4840310
# bps.A.B        0.27557708  0.25036473   0.2899822  0.7759541    0.19682668  -0.5562958 -0.74733042  1.0000000  0.1014912
# bps.A.B.1      0.25341226  0.22904557   0.2673998 -0.3608199    0.19145965   0.5958265 -0.48403095  0.1014912  1.0000000
# > cor(obs,method="spearman")
# Packets      Bytes Packets.A.B  Bytes.A.B Packets.A.B.1 Bytes.A.B.1   Duration bps.A.B bps.A.B.1
# Packets        1.0000000  0.9906469   0.9937750  0.9873695    -0.4379808  -0.4380163  0.6638251      NA        NA
# Bytes          0.9906469  1.0000000   0.9846322  0.9959394    -0.4464193  -0.4463099  0.6556155      NA        NA
# Packets.A.B    0.9937750  0.9846322   1.0000000  0.9887292    -0.4924911  -0.4924989  0.6667188      NA        NA
# Bytes.A.B      0.9873695  0.9959394   0.9887292  1.0000000    -0.4843853  -0.4843273  0.6556359      NA        NA
# Packets.A.B.1 -0.4379808 -0.4464193  -0.4924911 -0.4843853     1.0000000   0.9999314 -0.4080878      NA        NA
# Bytes.A.B.1   -0.4380163 -0.4463099  -0.4924989 -0.4843273     0.9999314   1.0000000 -0.4080025      NA        NA
# Duration       0.6638251  0.6556155   0.6667188  0.6556359    -0.4080878  -0.4080025  1.0000000      NA        NA
# bps.A.B               NA         NA          NA         NA            NA          NA         NA       1        NA
# bps.A.B.1             NA         NA          NA         NA            NA          NA         NA      NA         1


### covariance
cov(obs)
cov(obs,method="spearman")
cov(obs,method="spearman",use="complete.obs")

# > cov(obs)
#                   Packets       Bytes  Packets.A.B   Bytes.A.B Packets.A.B.1 Bytes.A.B.1   Duration bps.A.B bps.A.B.1
# Packets         5577979.0   377175108   2571248.22   179251412    3006730.79   197923696  122173.76      NA        NA
# Bytes         377175107.8 25740089982 172426912.20 12161638942  204748195.62 13578451040 8611877.30      NA        NA
# Packets.A.B     2571248.2   172426912   1222063.16    83878038    1349185.06    88548874   61210.44      NA        NA
# Bytes.A.B     179251412.2 12161638942  83878038.27  5884277553   95373373.90  6277361389 4377745.77      NA        NA
# Packets.A.B.1   3006730.8   204748196   1349185.06    95373374    1657545.73   109374822   60963.32      NA        NA
# Bytes.A.B.1   197923695.6 13578451040  88548873.93  6277361389  109374821.72  7301089651 4234131.53      NA        NA
# Duration         122173.8     8611877     61210.44     4377746      60963.32     4234132   20018.86      NA        NA
# bps.A.B                NA          NA           NA          NA            NA          NA         NA      NA        NA
# bps.A.B.1              NA          NA           NA          NA            NA          NA         NA      NA        NA
# > cov(obs,method="spearman")
#                 Packets     Bytes Packets.A.B Bytes.A.B Packets.A.B.1 Bytes.A.B.1  Duration bps.A.B bps.A.B.1
# Packets       392373451 388928765   390385580 387880493     -85907029   -85919699 290329484      NA        NA
# Bytes         388928765 392828247   387018100 391473770     -87612933   -87597260 286905068      NA        NA
# Packets.A.B   390385580 387018100   393288958 388867518     -96711508   -96719445 291935071      NA        NA
# Bytes.A.B     387880493 391473770   388867518 393311664     -95122493   -95117399 287090514      NA        NA
# Packets.A.B.1 -85907029 -87612933   -96711508 -95122493      98050026    98049792 -89220593      NA        NA
# Bytes.A.B.1   -85919699 -87597260   -96719445 -95117399      98049792    98063021 -89207858      NA        NA
# Duration      290329484 286905068   291935071 287090514     -89220593   -89207858 487500823      NA        NA
# bps.A.B              NA        NA          NA        NA            NA          NA        NA      NA        NA
# bps.A.B.1            NA        NA          NA        NA            NA          NA        NA      NA        NA
# > cov(obs,method="spearman",use="complete.obs")
#                 Packets     Bytes Packets.A.B  Bytes.A.B Packets.A.B.1 Bytes.A.B.1    Duration    bps.A.B  bps.A.B.1
# Packets       54907.736 54905.176   54898.479   54897.22     46449.061    44407.42   -6062.009   34308.76   31549.29
# Bytes         54905.176 57290.127   52506.587   52514.52     48850.185    46795.36   -3643.588   31838.91   29127.75
# Packets.A.B   54898.479 52506.587   57289.957   57285.41     43979.555    41942.60   -8518.417   36877.02   34005.21
# Bytes.A.B     54897.218 52514.516   57285.412  216003.07     43867.763  -120814.64  -69546.427  191606.90  -89097.51
# Packets.A.B.1 46449.061 48850.185   43979.555   43867.76     48878.562    48877.54    3053.468   23120.03   22489.59
# Bytes.A.B.1   44407.418 46795.361   41942.599 -120814.64     48877.541   216003.00   67167.881 -137366.51  147127.84
# Duration      -6062.009 -3643.588   -8518.417  -69546.43      3053.468    67167.88  282285.180 -210960.85 -136635.11
# bps.A.B       34308.759 31838.911   36877.019  191606.90     23120.026  -137366.51 -210960.846  282286.63   28649.61
# bps.A.B.1     31549.286 29127.750   34005.206  -89097.51     22489.594   147127.84 -136635.114   28649.61  282286.62


# Heatmap
# tSourceH <- heatmap(tSource, Rowv=NA, Colv=NA, margins=c(6,11), col=heat.colors(300),
#                        main="Heatmap of Packet Frequency by Source",
#                        xlab="Start Time", ylab="Source Address")


breaks <- c(0, 1800, 3600, 5400, 7200, 9000, 10800, 12600, 14400, 16200, 18000, 19800)
## All protocols
# source
# acast to convert Source to variables on y and Time on x
tSource <- acast(scadaDT[, .(Count=.N),
                 by=.(Time=cut(Time, breaks=breaks, labels=breaks[1:11]),
                      Source)][order(Time, Source)]
                     , Source~Time, value.var="Count")

tSource[is.na(tSource)] <- 0  ## replace NA->0
dimnames(tSource)[2][[1]][11]<-18000  ## rename NA col

col_breaks = c(seq(0,500,length=25),
               seq(500,25e2,length=25),
               seq(25e2,5e5,length=25),
               seq(5e5,1e6,length=25),
               seq(1e6,3e6,length=25),
               seq(3e6,5e6,length=25))

heatmap.2(tSource,
          main = "Heatmap of Packet Frequency by Source", # heat map title
          xlab = "Time From Start of Capture",
          ylab = "Source",
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(10,12),     # widens margins around plot
          col=heat.colors(149),
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",
          key=TRUE,
          keysize=.85,
          key.xlab="Packet Count",
          Colv="NA")            # turn off column clustering

# destination
tDest <- acast(scadaDT[, .(Count=.N),
                         by=.(Time=cut(Time, breaks=breaks, labels=breaks[1:11]),
                              Destination)][order(Time, Destination)]
                 , Destination~Time, value.var="Count")

tDest[is.na(tDest)] <- 0  ## replace NA->0
dimnames(tDest)[2][[1]][11]<-18000  ## rename NA col

col_breaks = c(seq(0,500,length=25),
               seq(500,25e2,length=25),
               seq(25e2,5e5,length=25),
               seq(5e5,1e6,length=25),
               seq(1e6,3e6,length=25),
               seq(3e6,5e6,length=25))

par(cex.main=1) # title size

heatmap.2(tDest,
          main = "Heatmap of Packet Frequency by Destination", # heat map title
          xlab = "Time From Start of Capture",
          ylab = "Destination",
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(10,12),     # widens margins around plot
          col=heat.colors(149),
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",
          key=TRUE,
          keysize=.85,
          key.xlab="Packet Count",
          Colv="NA")            # turn off column clustering






## Modbus
# source
# acast to convert Source to variables on y and Time on x
tModSource <- acast( scadaDT[Protocol=="Modbus/TCP", .(Count=.N),
                             by=.(Time=cut(Time, breaks=breaks[1:11], labels=breaks[1:10]),
                                  Source)][order(Time, Source)]
                     , Source~Time, value.var="Count")
# replace NA->0
tModSource[is.na(tModSource)] <- 0

# heatmap
hmcol <- brewer.pal(9,"GnBu")
par(cex.main=.85) # title size
# tModSourceH <- heatmap(tModSource, Rowv=NA, Colv=NA, margins=c(5,8), col=hmcol, cexRow=1, cexCol=1,
#                        main="Heatmap of Modbus/TCP Packet Frequency by Source", cellnote,
#                        xlab="Start Time", ylab="Source Address", scale=c("row"))

heatmap.2(tModSource,
          main = "Heatmap of Modbus/TCP Packet Frequency by Source", # heat map title
          xlab = "Time From Start of Capture",
          ylab = "Source",
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(8,10),     # widens margins around plot
          col=hmcol,
          dendrogram="none",     # only draw a row dendrogram
          keysize=1, # text size of key
          key.xlab="Packet Count",
          cexRow=1, # size of y labels
          Colv="NA")            # turn off column clustering

# destination
tModDest <- acast( scadaDT[Protocol=="Modbus/TCP", .(Count=.N),
                             by=.(Time=cut(Time, breaks=breaks[1:11], labels=breaks[1:10]),
                                  Destination)][order(Time, Destination)]
                     , Destination~Time, value.var="Count")
# replace NA->0
tModDest[is.na(tModDest)] <- 0

# heatmap
hmcol <- brewer.pal(9,"GnBu")
par(cex.main=.85) # title size
# tModSourceH <- heatmap(tModSource, Rowv=NA, Colv=NA, margins=c(5,8), col=hmcol, cexRow=1, cexCol=1,
#                        main="Heatmap of Modbus/TCP Packet Frequency by Source", cellnote,
#                        xlab="Start Time", ylab="Source Address", scale=c("row"))

heatmap.2(tModDest,
          main = "Heatmap of Modbus/TCP Packet Frequency by Destination", # heat map title
          xlab = "Time From Start of Capture",
          ylab = "Source",
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(8,10),     # widens margins around plot
          col=hmcol,
          dendrogram="none",     # only draw a row dendrogram
          keysize=1, # text size of key
          key.xlab="Packet Count",
          cexRow=1, # size of y labels
          Colv="NA")            # turn off column clustering

rm(breaks, tModSource, hmcol, col_breaks, tSource, tModDest, tDest)

## Add Souce and Destination by concatenating address and port
# paste(as.character(scadaTCPConvDT[,Address.A]), as.character(scadaTCPConvDT[,Port.A]), sep=":")
# paste(as.character(scadaTCPConvDT[,Address.B]), as.character(scadaTCPConvDT[,Port.B]), sep=":")
# scadaTCPConvDT[,Source:=NULL][,Destination:=NULL] # to delete variables
# head(scadaTCPConvDT[, PointA:= paste(as.character(Address.A), as.character(Port.A), sep=":")]
#      [, PointB:= paste(as.character(Address.B), as.character(Port.B), sep=":")])

# PointA, PointB, Packets, Bytes, Duration
# head(scadaTCPConvDT[,.(PointA, PointB, Packets, Bytes, Duration)])
g <- graph.data.frame(scadaTCPConvDT[,.(Address.A, Address.B, Packets, Bytes, Duration)], directed=FALSE)
gAdjMtx <- get.adjacency(g)  # adjacency mtx
gAdj <- graph.adjacency(gAdjMtx, mode="undirected", weighted=TRUE)
V(gAdj)$size <- degree(gAdj)*5  # node size according to degree of centrality
eW <- E(gAdj)$weight  # edge weights
eW <- ceiling(eW/max(eW)*55)
eW[eW>10] <- 10
plot.igraph(gAdj, main="Graph of SCADA Network", edge.width=eW)

rm(g, gAdjMtx, gAdj, eW)






## TODO: analyze time intervals

#### Modbus Packet Data ####
#   - source IP
#   - source port
#   - destination IP
#   - destination port
#   - protocol
#   - start
#   - duration
#   - packets
#   - bytes
#   - length

# TODO use moddata instead?
modbusDT <- as.data.table(read.csv("~/Bureau/data/scadaCops/modbus/modbus_transform.data", header=FALSE,
                                 col.names=c("protocolID", "transID", "length","functionCode","refType","wordCnt","data"),
                                 colClass=c(protocolID="factor", functionCode="factor", refType="factor")))
# setnames(modbus, c("protocolID", "length","functionCode","refNum","refType","wordCnt","data"))

str(modbusDT)
# > str(modbusDT)
# Classes ‘data.table’ and 'data.frame':  825535 obs. of  7 variables:
#   $ protocolID  : Factor w/ 6 levels "0","1","228",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ transID     : int  0 0 1 1 2 2 3 3 4 4 ...
# $ length      : int  6 5 6 5 6 5 6 5 6 5 ...
# $ functionCode: Factor w/ 10 levels "","0","1","228",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ refType     : Factor w/ 7 levels "","1","228","4",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ wordCnt     : int  1 NA 1 NA 1 NA 1 NA 1 NA ...
# $ data        : Factor w/ 120 levels "","00:01:00",..: 1 83 1 10 1 10 1 10 1 10 ...
# - attr(*, ".internal.selfref")=<externalptr>
  
summary(modbusDT)
# > summary(modbusDT)
# protocolID        transID            length         functionCode     refType          wordCnt             data       
# 0    :825521   Min.   :    0.0   Min.   :    0.0   4      :802288        :825518   Min.   :    1.0          :423997  
# 1    :     5   1st Qu.:   65.0   1st Qu.:    5.0   1      : 22284   1    :     3   1st Qu.:    1.0   00:75  :165176  
# 228  :     2   Median :  131.0   Median :    6.0   90     :   936   228  :     4   Median :    1.0   00:50  :155265  
# 4    :     3   Mean   :  989.4   Mean   :    5.8   43     :    10   4    :     2   Mean   :    1.2   00:54  : 33506  
# 43554:     1   3rd Qu.:  197.0   3rd Qu.:    6.0          :     5   42786:     1   3rd Qu.:    1.0   1a:10  :  5457  
# 6    :     3   Max.   :65313.0   Max.   :42786.0   0      :     3   43042:     1   Max.   :43298.0   0a:68  :  4931  
#                NA's   :3         NA's   :7         (Other):     9   6    :     6   NA's   :424376    (Other): 37203   

# TODO: need to change these two tables to use moddataDT instead
table(modbusDT[,protocolID])
> table(modbusDT[,protocolID])

#      0      1    228      4  43554      6 
# 825521      5      2      3      1      3 

table(modbusDT[,functionCode])
# > table(modbusDT[,functionCode])
# 
#        0      1    228      4  42786     43  43042      6     90 
# 5      3  22284      3 802288      1     10      2      3    936

bwplot(~length | protocolID, data=modbusDT, as.table=TRUE,
       scales="free", main="Boxplot of Modbus/TCP Packet Data Lengths by Modbus Protocol ID")


modbusDT[, list(min=as.numeric(min(length, na.rm=TRUE)),
                lower=quantile(length, .25, na.rm=TRUE),
                median=as.numeric(median(length, na.rm=TRUE)),
                mean=as.numeric(mean(length, na.rm=TRUE)),
                middle=quantile(length, .50, na.rm=TRUE),
                upper=quantile(length, .75, na.rm=TRUE),
                max=as.numeric(max(length, na.rm=TRUE))
                ), by=protocolID]
# ), by=protocolID][2:.N]  # removes NAs
# protocolID min lower median     mean middle upper max
#          0   0   5.0      6 5.697069      6     6 255
#          2   2   2.0      2 2.000000      2     2   2
#          4   4   4.0      4 4.000000      4     4   4
#          1   1   1.0      1 1.000000      1     1   1
#          6   1   2.5      4 3.000000      4     4   4

#    protocolID min lower median     mean middle upper   max
# 1:          0   0   5.0      6 5.801011      6     6 42786
# 2:          4   1   1.0      1 1.000000      1     1     1
# 3:          6   1   2.5      4 3.000000      4     4     4
# 4:          1 Inf    NA     NA      NaN     NA    NA  -Inf
# 5:        228   6   6.0      6 6.000000      6     6     6
# 6:      43554   6   6.0      6 6.000000      6     6     6


ggplot(modbusDT, aes(x=refType)) + geom_histogram() + ggtitle("Histogram of Reference Numbers") +
  xlab("Reference Type") + ylab("Frequency")
ggplot(modbusDT, aes(x=functionCode)) + geom_histogram() + ggtitle("Histogram of Modbus Function Code") +
  xlab("Function Code") + ylab("Frequency")
# ggplot(modbusDT, aes(x=refType)) + geom_histogram()  # only NAs



## modbus/tcp data
moddataDT <- as.data.table(read.csv("~/Bureau/data/scadaCops/modbus/modbus.data", header=TRUE,
                                   colClass=c(ip.proto="factor", ip.version="factor",
                                              mbtcp.modbus.func_code="factor", tcp.srcport="factor",
                                              tcp.dstport="factor", mbtcp.modbus.reference_num="factor",
                                              mbtcp.prot_id="factor")))


modsubDT <- as.data.table(read.csv("~/Bureau/data/scadaCops/modbus/modsplit/data.txt", header=TRUE,
                                    colClass=c(ip.proto="factor", ip.version="factor",
                                               mbtcp.modbus.func_code="factor", tcp.srcport="factor",
                                               tcp.dstport="factor", mbtcp.modbus.reference_num="factor",
                                               mbtcp.prot_id="factor")))

# cleanup
moddataDT <- moddataDT[!is.na(mbtcp.trans_id)]

# replace NA->0
# is.na(modsubDT[,.(mbtcp.modbus.word_cnt)])
# 
# modsubDT[is.na(modbusDT[,.(mbtcp.modbus.word_cnt)])] <- 0
# 
# summary(modsubDT)
# modbusDT[mbtcp.modbus.word_cnt]

# subset(subRequests, !(is.na(mbtcp.trans_id))

scadaDT[,(plot(Protocol, main="Number of Packets by Protocol", xlab="Protocol", ylab="Frequency"))]

par(mfrow=c(1,2))
moddataDT[,(plot(frame.number, frame.time_relative, main="Frame # / Time"))]
moddataDT[,(plot(frame.number, mbtcp.len, main="Frame # / Length"))]
dev.off()


# plot on same window
xyp1 <- ggplot(moddataDT, aes(x=frame.number, y=frame.time_relative)) +
  geom_point(shape=1, col="firebrick", alpha=0.5) + ggtitle("Frame # / Time")

xyp2 <- ggplot(moddataDT, aes(x=frame.number, y=mbtcp.len)) +
  geom_point(shape=1, col="olivedrab", alpha=0.5) + ggtitle("Frame # / Length")

grid.arrange(xyp1, xyp2,ncol=2)


# modbus requests
requests <- subset(moddataDT[tcp.dstport=="502"], !(is.na(mbtcp.trans_id)))
str(requests)
summary(requests)
# > summary(requests)
# frame.time_relative frame.time_delta_displayed   frame.len      ip.proto   ip.version            ip.src                  ip.dst        tcp.srcport    
# Min.   :  572.1     Min.   :-0.000011          Min.   : 64.00    :     0    :     0   192.168.12.51 :401150   192.168.12.253:412917   2735   :231547  
# 1st Qu.: 3703.5     1st Qu.: 0.000238          1st Qu.: 66.00   1:     0   1:     0   192.168.12.90 : 11163                 :     0   2499   :111597  
# Median : 7512.7     Median : 0.000280          Median : 66.00   4:     0   4:412917   192.168.12.250:   300   0             :     0   3441   : 58006  
# Mean   : 7618.5     Mean   : 0.014916          Mean   : 66.02   6:412917   6:     0   192.168.50.50 :   300   1             :     0   1037   :  8878  
# 3rd Qu.:10624.4     3rd Qu.: 0.000346          3rd Qu.: 66.00                         192.168.12.80 :     4   192.168.12.128:     0   1032   :  1641  
# Max.   :16482.1     Max.   : 4.807170          Max.   :315.00                                       :     0   192.168.12.250:     0   1034   :   338  
# (Other)       :     0   (Other)       :     0   (Other):   910  
# tcp.dstport     mbtcp.prot_id mbtcp.trans_id    mbtcp.len        mbtcp.modbus.func_code mbtcp.modbus.reference_num mbtcp.modbus.word_cnt
# 502    :412917    :     0      Min.   :    0   Min.   :    0.00   4      :401145         1      :188770             Min.   :    1.00     
# :     0   0:412917      1st Qu.:   65   1st Qu.:    6.00   1      : 11140         0      :176317             1st Qu.:    1.00     
# 1      :     0   1:     0      Median :  131   Median :    6.00   90     :   618         2      : 23602             Median :    1.00     
# 1032   :     0   2:     0      Mean   :  989   Mean   :    6.23   43     :     5         3      : 23596             Mean   :    1.22     
# 1033   :     0                 3rd Qu.:  197   3rd Qu.:    6.00   0      :     3                :   623             3rd Qu.:    1.00     
# 1034   :     0                 Max.   :65313   Max.   :42786.00   228    :     3         228    :     4             Max.   :43298.00     
# (Other):     0                                                    (Other):     3         (Other):     5             NA's   :11763        
#  mbtcp.modbus.data
#          :412585  
#  01:04   :    84  
#  00:04   :    52  
#  00:01:00:    34  
#  00:02   :    34  
#  01:12   :    32  
#  (Other) :    96 
# modbus response

responses <- subset(moddataDT[tcp.srcport=="502"], !(is.na(mbtcp.trans_id)))
str(responses)
summary(responses)
# > summary(responses)
# frame.time_relative frame.time_delta_displayed   frame.len      ip.proto   ip.version            ip.src                  ip.dst        tcp.srcport    
# Min.   :  572.1     Min.   :0.000017           Min.   : 64.00    :     0    :     0   192.168.12.253:412604   192.168.12.51 :401141   502    :412604  
# 1st Qu.: 3704.5     1st Qu.:0.009595           1st Qu.: 65.00   1:     0   1:     0                 :     0   192.168.12.90 : 11159          :     0  
# Median : 7516.1     Median :0.009719           Median : 65.00   4:     0   4:412604   0             :     0   192.168.12.250:   300   0      :     0  
# Mean   : 7620.8     Mean   :0.011458           Mean   : 65.37   6:412604   6:     0   1             :     0   192.168.12.80 :     4   1      :     0  
# 3rd Qu.:10626.5     3rd Qu.:0.009799           3rd Qu.: 65.00                         192.168.12.128:     0                 :     0   10     :     0  
# Max.   :16482.1     Max.   :0.150748           Max.   :315.00                         192.168.12.250:     0   0             :     0   100    :     0  
# (Other)       :     0   (Other)       :     0   (Other):     0  
# tcp.dstport     mbtcp.prot_id mbtcp.trans_id      mbtcp.len      mbtcp.modbus.func_code mbtcp.modbus.reference_num mbtcp.modbus.word_cnt
# 2735   :231547    :     0      Min.   :    0.0   Min.   :  4.00   4      :401141                :412604             Min.   : NA          
# 2499   :111588   0:412604      1st Qu.:   65.0   1st Qu.:  5.00   1      : 11140         0      :     0             1st Qu.: NA          
# 3441   : 58006   1:     0      Median :  131.0   Median :  5.00   90     :   318         1      :     0             Median : NA          
# 1037   :  8875   2:     0      Mean   :  989.8   Mean   :  5.37   43     :     5         2      :     0             Mean   :NaN          
# 1032   :  1640                 3rd Qu.:  197.0   3rd Qu.:  5.00          :     0         228    :     0             3rd Qu.: NA          
# 1034   :   338                 Max.   :65313.0   Max.   :255.00   0      :     0         3      :     0             Max.   : NA          
# (Other):   610                                                    (Other):     0         (Other):     0             NA's   :412604       
#  mbtcp.modbus.data
#  00:75  :165176   
#  00:50  :155265   
#  00:54  : 33506   
#         : 11405   
#  1a:10  :  5457   
#  0a:68  :  4931   
#  (Other): 36864 

# stats on register values
# min med mean max 
summary(requests[,.(mbtcp.modbus.data),
                 by=.(mbtcp.modbus.func_code, mbtcp.modbus.reference_num)])
# > summary(requests[,.(mbtcp.modbus.data),
#                    +                  by=.(mbtcp.modbus.func_code, mbtcp.modbus.reference_num)])
# mbtcp.modbus.func_code mbtcp.modbus.reference_num mbtcp.modbus.data
# 4      :401145         1      :188770                     :412585  
# 1      : 11140         0      :176317             01:04   :    84  
# 90     :   618         2      : 23602             00:04   :    52  
# 43     :     5         3      : 23596             00:01:00:    34  
# 0      :     3                :   623             00:02   :    34  
# 228    :     3         228    :     4             01:12   :    32  
# (Other):     3         (Other):     5             (Other) :    96 


summary(requests[,.(mbtcp.modbus.data),
                  by=.(mbtcp.modbus.func_code, mbtcp.modbus.reference_num)])
# mbtcp.modbus.func_code mbtcp.modbus.reference_num mbtcp.modbus.data
# 4      :401145         1      :188770                     :412585  
# 1      : 11140         0      :176317             01:04   :    84  
# 90     :   618         2      : 23602             00:04   :    52  
# 43     :     5         3      : 23596             00:01:00:    34  
# 0      :     3                :   623             00:02   :    34  
# 228    :     3         228    :     4             01:12   :    32  
# (Other):     3         (Other):     5             (Other) :    96

modMergeDT <- merge(requests[mbtcp.trans_id])
merge(requests[,.(mbtcp.trans_id, req.func=mbtcp.modbus.func_code, req.ref=mbtcp.modbus.reference_num,
            req.word.cnt=mbtcp.modbus.word_cnt, req.data=mbtcp.modbus.data),],
      responses[,.(mbtcp.trans_id, resp.data=mbtcp.modbus.data)],
      by="mbtcp.trans_id"
)

subRequests<-modsubDT[tcp.dstport=="502"]
subResponses<-modsubDT[tcp.srcport=="502"]
requests[,.N > 1, by=mbtcp.trans_id]

setkey(requests, mbtcp.trans_id)
setkey(responses, mbtcp.trans_id)
merge(requests[,.(mbtcp.trans_id, req.func=mbtcp.modbus.func_code)],
      responses[,.(mbtcp.trans_id, resp.data=mbtcp.modbus.data)],
      by="mbtcp.trans_id"
      )




######## EXAMPLES ########

# remove rows with variable having value of NA
# subset(subRequests, !(is.na(mbtcp.trans_id))

# vS <- ceiling((degree(g)/50))
# vS[vS > 1000] <- 20
# vS[vS > 100] <- 15
# vS[vS > 20] <- 10
# vS

# remove variables from DT
# scadaTCPConvDT[,Source:=NULL][,Destination:=NULL]

# array/faster
#ptm <- proc.time() 
#proc.time() - ptm

# df <- data.frame(
#   group = c("Male", "Female", "Child"),
#   value = c(25, 25, 50)
# )
# 
# bp<- ggplot(df, aes(x="", y=value, fill=group))+
#   geom_bar(width = 1, stat = "identity")
# bp

# hp <- ggplot(scadaTCPConvDT[Address.A=="83.133.119.197", ], aes(y=Bytes/1e3, x=Packets)
# ) + geom_histogram(stat="identity", binwidth=2)
# hp <- hp + facet_grid(. ~ Address.A)
# hp <- hp + ylab("KBytes") + ggtitle("Histogram of Bytes per Packet by Address")
# # hp <- hp + coord_cartesian(ylim=c(0, 800)) + 
# #   scale_y_continuous(breaks=seq(0, 800, 400))
# hp

# scadaDT[3:5,] # select rows 3-5
# scadaDT[3:10, .(Source,Protocol)]

# head(scadaDT[Protocol=="Modbus/TCP"])
# (subset(scadaDT, Protocol=="Modbus/TCP"))

# Pie Chart from data frame with Appended Sample Sizes
# mytable <- table(iris$Species)
# lbls <- paste(names(mytable), "\n", mytable, sep="")
# pie(mytable, labels = lbls, 
#     main="Pie Chart of Species\n (with sample sizes)")
# 
# 
# a <- rep(c(NA, 1/0:3), 10)
# table(a)
# table(a, exclude = NULL)
# b <- factor(rep(c("A","B","C"), 10))
# table(b)
# table(b, exclude = "B")
# d <- factor(rep(c("A","B","C"), 10), levels = c("A","B","C","D","E"))
# table(d, exclude = "B")
# print(table(b, d), zero.print = ".")