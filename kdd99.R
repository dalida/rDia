darpa <- kddcup.testdata
names(darpa) <- c("duration",
                  "protocol_type",
                  "service",
                  "flag",
                  "src_bytes",
                  "dst_bytes",
                  "land",
                  "wrong_fragment",
                  "urgent",
                  "hot",
                  "num_failed_logins",
                  "logged_in",
                  "num_compromised",
                  "root_shell",
                  "su_attempted",
                  "num_root",
                  "num_file_creations",
                  "num_shells",
                  "num_access_files",
                  "num_outbound_cmds",
                  "is_host_login",
                  "is_guest_login",
                  "count",
                  "srv_count",
                  "serror_rate",
                  "srv_serror_rate",
                  "rerror_rate",
                  "srv_rerror_rate",
                  "same_srv_rate",
                  "diff_srv_rate",
                  "srv_diff_host_rate",
                  "dst_host_count",
                  "dst_host_srv_count",
                  "dst_host_same_srv_rate",
                  "dst_host_diff_srv_rate",
                  "dst_host_same_src_port_rate",
                  "dst_host_srv_diff_host_rate",
                  "dst_host_serror_rate",
                  "dst_host_srv_serror_rate",
                  "dst_host_rerror_rate",
                  "dst_host_srv_rerror_rate"
)
attach(darpa)
str(darpa)
# > str(darpa)
# 'data.frame':  311029 obs. of  41 variables:
#   $ duration                   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ protocol_type              : Factor w/ 3 levels "icmp","tcp","udp": 3 3 3 3 3 3 3 3 3 2 ...
# $ service                    : Factor w/ 65 levels "auth","bgp","courier",..: 45 45 45 45 45 45 9 45 45 20 ...
# $ flag                       : Factor w/ 11 levels "OTH","REJ","RSTO",..: 10 10 10 10 10 10 10 10 10 10 ...
# $ src_bytes                  : int  105 105 105 105 105 105 29 105 105 223 ...
# $ dst_bytes                  : int  146 146 146 146 146 146 0 146 146 185 ...
# $ land                       : int  0 0 0 0 0 0 0 0 0 0 ...
# $ wrong_fragment             : int  0 0 0 0 0 0 0 0 0 0 ...
# $ urgent                     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ hot                        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ num_failed_logins          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ logged_in                  : int  0 0 0 0 0 0 0 0 0 1 ...
# $ num_compromised            : int  0 0 0 0 0 0 0 0 0 0 ...
# $ root_shell                 : int  0 0 0 0 0 0 0 0 0 0 ...
# $ su_attempted               : int  0 0 0 0 0 0 0 0 0 0 ...
# $ num_root                   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ num_file_creations         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ num_shells                 : int  0 0 0 0 0 0 0 0 0 0 ...
# $ num_access_files           : int  0 0 0 0 0 0 0 0 0 0 ...
# $ num_outbound_cmds          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ is_host_login              : int  0 0 0 0 0 0 0 0 0 0 ...
# $ is_guest_login             : int  0 0 0 0 0 0 0 0 0 0 ...
# $ count                      : int  1 1 1 2 2 2 2 1 2 4 ...
# $ srv_count                  : int  1 1 1 2 2 2 1 1 2 4 ...
# $ serror_rate                : num  0 0 0 0 0 0 0 0 0 0 ...
# $ srv_serror_rate            : num  0 0 0 0 0 0 0 0 0 0 ...
# $ rerror_rate                : num  0 0 0 0 0 0 0 0 0 0 ...
# $ srv_rerror_rate            : num  0 0 0 0 0 0 0 0 0 0 ...
# $ same_srv_rate              : num  1 1 1 1 1 1 0.5 1 1 1 ...
# $ diff_srv_rate              : num  0 0 0 0 0 0 1 0 0 0 ...
# $ srv_diff_host_rate         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ dst_host_count             : int  255 255 255 255 255 255 10 255 255 71 ...
# $ dst_host_srv_count         : int  254 254 254 254 254 255 3 253 254 255 ...
# $ dst_host_same_srv_rate     : num  1 1 1 1 1 1 0.3 0.99 1 1 ...
# $ dst_host_diff_srv_rate     : num  0.01 0.01 0.01 0.01 0.01 0 0.3 0.01 0.01 0 ...
# $ dst_host_same_src_port_rate: num  0 0 0 0 0.01 0.01 0.3 0 0 0.01 ...
# $ dst_host_srv_diff_host_rate: num  0 0 0 0 0 0 0 0 0 0.01 ...
# $ dst_host_serror_rate       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ dst_host_srv_serror_rate   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ dst_host_rerror_rate       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ dst_host_srv_rerror_rate   : num  0 0 0 0 0 0 0 0 0 0 ...

summary(darpa)
# > summary(darpa)
# duration       protocol_type     service            flag          src_bytes          dst_bytes            land         
# Min.   :    0.0   icmp:164969   ecr_i   :164352   SF     :248379   Min.   :       0   Min.   :      0   Min.   :0.00e+00  
# 1st Qu.:    0.0   tcp :119357   private : 78510   REJ    : 41945   1st Qu.:     105   1st Qu.:      0   1st Qu.:0.00e+00  
# Median :    0.0   udp : 26703   http    : 41237   S0     : 18012   Median :     520   Median :      0   Median :0.00e+00  
# Mean   :   17.9                 smtp    :  8268   RSTO   :  1393   Mean   :    1732   Mean   :    748   Mean   :2.89e-05  
# 3rd Qu.:    0.0                 pop_3   :  3972   RSTR   :   872   3rd Qu.:    1032   3rd Qu.:      0   3rd Qu.:0.00e+00  
# Max.   :57715.0                 domain_u:  3160   S3     :   289   Max.   :62825648   Max.   :5203179   Max.   :1.00e+00  
# (Other) : 11530   (Other):   139                                                          
# wrong_fragment         urgent              hot            num_failed_logins    logged_in      num_compromised      root_shell       
# Min.   :0.000000   Min.   :0.00e+00   Min.   :  0.00000   Min.   :0.000000   Min.   :0.0000   Min.   :  0.0000   Min.   :0.0000000  
# 1st Qu.:0.000000   1st Qu.:0.00e+00   1st Qu.:  0.00000   1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:  0.0000   1st Qu.:0.0000000  
# Median :0.000000   Median :0.00e+00   Median :  0.00000   Median :0.000000   Median :0.0000   Median :  0.0000   Median :0.0000000  
# Mean   :0.000762   Mean   :5.14e-05   Mean   :  0.01468   Mean   :0.002363   Mean   :0.1725   Mean   :  0.0112   Mean   :0.0001993  
# 3rd Qu.:0.000000   3rd Qu.:0.00e+00   3rd Qu.:  0.00000   3rd Qu.:0.000000   3rd Qu.:0.0000   3rd Qu.:  0.0000   3rd Qu.:0.0000000  
# Max.   :3.000000   Max.   :3.00e+00   Max.   :101.00000   Max.   :4.000000   Max.   :1.0000   Max.   :796.0000   Max.   :1.0000000  
# 
# su_attempted         num_root        num_file_creations   num_shells      num_access_files   num_outbound_cmds is_host_login     
# Min.   :0.00e+00   Min.   :  0.0000   Min.   :0.0e+00    Min.   :0.0e+00   Min.   :0.000000   Min.   :0         Min.   :0.00e+00  
# 1st Qu.:0.00e+00   1st Qu.:  0.0000   1st Qu.:0.0e+00    1st Qu.:0.0e+00   1st Qu.:0.000000   1st Qu.:0         1st Qu.:0.00e+00  
# Median :0.00e+00   Median :  0.0000   Median :0.0e+00    Median :0.0e+00   Median :0.000000   Median :0         Median :0.00e+00  
# Mean   :2.25e-05   Mean   :  0.0084   Mean   :9.6e-04    Mean   :8.4e-05   Mean   :0.000772   Mean   :0         Mean   :3.86e-05  
# 3rd Qu.:0.00e+00   3rd Qu.:  0.0000   3rd Qu.:0.0e+00    3rd Qu.:0.0e+00   3rd Qu.:0.000000   3rd Qu.:0         3rd Qu.:0.00e+00  
# Max.   :2.00e+00   Max.   :878.0000   Max.   :1.0e+02    Max.   :5.0e+00   Max.   :4.000000   Max.   :0         Max.   :1.00e+00  
# 
# is_guest_login         count         srv_count      serror_rate      srv_serror_rate    rerror_rate     srv_rerror_rate  same_srv_rate   
# Min.   :0.000000   Min.   :  0.0   Min.   :  0.0   Min.   :0.00000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:0.000000   1st Qu.: 17.0   1st Qu.:  7.0   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:1.0000  
# Median :0.000000   Median :212.0   Median :126.0   Median :0.00000   Median :0.00000   Median :0.0000   Median :0.0000   Median :1.0000  
# Mean   :0.002424   Mean   :269.2   Mean   :235.6   Mean   :0.05922   Mean   :0.05919   Mean   :0.1426   Mean   :0.1422   Mean   :0.8157  
# 3rd Qu.:0.000000   3rd Qu.:511.0   3rd Qu.:511.0   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.0000  
# Max.   :1.000000   Max.   :511.0   Max.   :511.0   Max.   :1.00000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
# 
# diff_srv_rate     srv_diff_host_rate dst_host_count  dst_host_srv_count dst_host_same_srv_rate dst_host_diff_srv_rate
# Min.   :0.00000   Min.   :0.00000    Min.   :  0.0   Min.   :  0.0      Min.   :0.0000         Min.   :0.00000       
# 1st Qu.:0.00000   1st Qu.:0.00000    1st Qu.:255.0   1st Qu.:244.0      1st Qu.:0.9700         1st Qu.:0.00000       
# Median :0.00000   Median :0.00000    Median :255.0   Median :255.0      Median :1.0000         Median :0.00000       
# Mean   :0.02445   Mean   :0.02535    Mean   :235.3   Mean   :199.2      Mean   :0.7935         Mean   :0.02495       
# 3rd Qu.:0.00000   3rd Qu.:0.00000    3rd Qu.:255.0   3rd Qu.:255.0      3rd Qu.:1.0000         3rd Qu.:0.01000       
# Max.   :1.00000   Max.   :1.00000    Max.   :255.0   Max.   :255.0      Max.   :1.0000         Max.   :1.00000       
# 
# dst_host_same_src_port_rate dst_host_srv_diff_host_rate dst_host_serror_rate dst_host_srv_serror_rate dst_host_rerror_rate
# Min.   :0.0000              Min.   :0.000000            Min.   :0.00000      Min.   :0.00000          Min.   :0.0000      
# 1st Qu.:0.0000              1st Qu.:0.000000            1st Qu.:0.00000      1st Qu.:0.00000          1st Qu.:0.0000      
# Median :1.0000              Median :0.000000            Median :0.00000      Median :0.00000          Median :0.0000      
# Mean   :0.5479              Mean   :0.004566            Mean   :0.05876      Mean   :0.05879          Mean   :0.1427      
# 3rd Qu.:1.0000              3rd Qu.:0.000000            3rd Qu.:0.00000      3rd Qu.:0.00000          3rd Qu.:0.0000      
# Max.   :1.0000              Max.   :1.000000            Max.   :1.00000      Max.   :1.00000          Max.   :1.0000      
# 
# dst_host_srv_rerror_rate
# Min.   :0.0000          
# 1st Qu.:0.0000          
# Median :0.0000          
# Mean   :0.1417          
# 3rd Qu.:0.0000          
# Max.   :1.0000

par(mfrow=c(2,2))
protocols <- darpa$protocol_type
plot(protocol, main="Protocols", xlab="Protocol", ylab="Frequency")

services <- darpa$service
plot(services, main="Services", xlab="Service", ylab="Frequency", col="red")

flags <- darpa$flag
plot(flags, main="Flags", xlab="flags", ylab="Frequency", col="blue")

src_byptes <- darpa$src_bytes
summary(src_bytes)

#plot(src_bytes)
