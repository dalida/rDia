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

# pie chart
# plot.new()
# lbls <- paste(names(protocols), "\n", protocols, sep="")
# par(mfrow=c(1,1))
# pie(protocols, labels=lbls, main="Distribution by Protocols")


# camembert
protocol <- scadaDT$Protocol
camembert(protocol)
rm(protocol)

# stacked
sbar <- ggplot(scadaDT, aes(x=Source, fill=Protocol)) + geom_bar()
sbar <- sbar + ggtitle("Frequency of Packets by Source/Protocol) + ylab("Frequency")
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


# Number of Packets by Destination
scadaDT[, .(Count=.N) ,by=Source]
# scadaDT[,(plot(Source, main="Frequency by Source", xlab="Source", ylab="Frequency"))]

hp <- ggplot(scadaDT, aes(x=Source)) + geom_histogram()
hp <- hp + ggtitle("Histogram of Packets by Source") + ylab("Count")
hp + theme(axis.text.x = element_text(angle=90))
rm(hp)

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
scadaDT[, .(Count=.N) ,by=Destination]
# scadaDT[,(plot(Destination, main="Frequency by Destination", xlab="Destination", ylab="Frequency"))]

hp <- ggplot(scadaDT, aes(x=Destination)) + geom_histogram()
hp <- hp + ggtitle("Histogram of Packets by Destination") + ylab("Count")
hp + theme(axis.text.x = element_text(angle=90))
rm(hp)

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
scadaDT[, .(Count=.N) , by=.(Source,Protocol)]
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
scadaDT[, .(Count=.N) , by=.(Destination,Protocol)]
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

# boxplots
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
         scales="free", main="Boxplots Packet Lengths by Protocol",
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
  + scale_fill_brewer(palette=1)
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
  scale_fill_brewer()
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


bxp <- ggplot(scadaTCPConvDT, aes(Address.A, Duration))
bxp + geom_boxplot()

bxp <- ggplot(scadaTCPConvDT, aes(Port.A, Duration)) + geom_boxplot()
bxp

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
x <- scadaTCPConvDT[,Packets]
bw <- diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))
hp <- ggplot(scadaTCPConvDT, aes(x=Packets)) + geom_histogram(binwidth=bw)
hp <- hp + facet_grid(Address.B ~ Address.A, scales="free_y")
hp <- hp + ggtitle("Histogram of Packet per Destination/Source") + ylab("Count")
hp + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.text.y = element_text(angle=0))
rm(x, bw, hp)


# qplot(scadaEndPtsDT[,Address], geom="histogram", binwidth = 0.5, main = "Number of Ports Per Address",
#       xlab = "Address", ylab = "Number of Ports", fill = I("blue"), alpha = I(".2"), col = I("red"))

breaks <- c(0, 1800, 3600, 5400, 7200, 9000, 10800, 12600, 14400, 16200, 18000, 19800)
## all protocols
# acast to convert Source to variables on y and Time on x
tSource <- acast(scadaDT[, .(Count=.N),
                 by=.(Time=cut(Time, breaks=breaks, labels=breaks[1:11]),
                      Source)][order(Time, Source)]
                     , Source~Time, value.var="Count")

# replace NA->0
tSource[is.na(tSource)] <- 0

# heatmap
tSourceH <- heatmap(tSource, Rowv=NA, Colv=NA, margins=c(6,11), col=cm.colors(256),
                       main="Heatmap of Packet Frequency by Source",
                       xlab="Start Time", ylab="Source Address")


## Modbus
# acast to convert Source to variables on y and Time on x
tModSource <- acast( scadaDT[Protocol=="Modbus/TCP", .(Count=.N),
                             by=.(Time=cut(Time, breaks=breaks[1:11], labels=breaks[1:10]),
                                  Source)][order(Time, Source)]
                     , Source~Time, value.var="Count")
# replace NA->0
tModSource[is.na(tModSource)] <- 0

# heatmap
hmcol <- brewer.pal(11,"GnBu")
par(cex.main=.8)
tModSourceH <- heatmap(tModSource, Rowv=NA, Colv=NA, margins=c(5,8), col=hmcol, cexRow=1, cexCol=1,
                       main="Heatmap of Modbus/TCP Packet Frequency by Source", cellnote,
                       xlab="Start Time", ylab="Source Address", scale=c("row"))

rm(breaks, tModSource, hmcol, tModSourceH, tSource, tSourceH)

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

## TODO: analyze modbus data
modbus <- as.data.table(read.csv("~/Bureau/data/scadaCops/modbus/data.out", header=FALSE))

#### EXAMPLES

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