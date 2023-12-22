# Анализ логов с помощью языка R
Хвощев Алексей, БИСО-03-20

## Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Научиться работать с пакетом tidyverse
3.  Научиться анализировать метаданные с помощью языка R

## Ход работы

## Подготовка данных для работы

В самом начале нужно подготовить данные для работы, для этого
импортируем данные из файла .log и .csv и преобразуем их

``` r
library(tidyverse)
```

    Warning: пакет 'tidyverse' был собран под R версии 4.3.2

    Warning: пакет 'ggplot2' был собран под R версии 4.3.2

    Warning: пакет 'tidyr' был собран под R версии 4.3.2

    Warning: пакет 'readr' был собран под R версии 4.3.2

    Warning: пакет 'dplyr' был собран под R версии 4.3.2

    Warning: пакет 'forcats' был собран под R версии 4.3.2

    Warning: пакет 'lubridate' был собран под R версии 4.3.2

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
dataLog <- read.csv("dns.log",sep="\t")
dataCSV <- read.csv("header.csv")
dataCSV
```

              Field       Type
    1           ts       time 
    2          uid      string
    3           id      recor 
    4                       d 
    5        proto       proto
    6     trans_id       count
    7        query      string
    8       qclass       count
    9  qclass_name      string
    10       qtype       count
    11  qtype_name      string
    12       rcode       count
    13  rcode_name      string
    14          QR       bool 
    15          AA       bool 
    16       TC RD  bool bool 
    17          RA       bool 
    18           Z       count
    19     answers      vector
    20        TTLs      vector
    21    rejected       bool 
                                                                                           Description
    1                                                                    Timestamp of the DNS request 
    2                                                                     Unique id of the connection 
    3                                                ID record with orig/resp host/port. See conn.log 
    4                                                                                                 
    5                                                        Protocol of DNS transaction – TCP or UDP 
    6                                       16 bit identifier assigned by DNS client; responses match 
    7                                                                Domain name subject of the query 
    8                                                                Value specifying the query class 
    9                                           Descriptive name of the query class (e.g. C_INTERNET) 
    10                                                                Value specifying the query type 
    11                                                     Name of the query type (e.g. A, AAAA, PTR) 
    12                                                        Response code value in the DNS response 
    13                                 Descriptive name of the response code (e.g. NOERROR, NXDOMAIN) 
    14                                        Was this a query or a response? T = response, F = query 
    15                                    Authoritative Answer. T = server is authoritative for query 
    16 Truncation. T = message was truncated Recursion Desired. T = request recursive lookup of query 
    17                                     Recursion Available. T = server supports recursive queries 
    18                                      Reserved field, should be zero in all queries & responses 
    19                                           List of resource descriptions in answer to the query 
    20                                                               Caching intervals of the answers 
    21                                               Whether the DNS query was rejected by the server 

``` r
dataCSV[3,"Field"] <- "id.orig_h"
dataCSV[3,"Type"] <- "addr"
dataCSV[4,"Field"] <- "id.resp_h"
dataCSV[4,"Type"] <- "addr"
dataCSV[4,"Description"] <- "Response IP"
dataCSV <- dataCSV %>% add_row(Field = "id.orig_p", Type = "port", Description = "Origin port", .before = 4)
dataCSV <- dataCSV %>% add_row(Field = 'id.resp_p', Type = "port", Description = "Response port", .before = 6)
dataCSV
```

              Field       Type
    1           ts       time 
    2          uid      string
    3     id.orig_h       addr
    4     id.orig_p       port
    5     id.resp_h       addr
    6     id.resp_p       port
    7        proto       proto
    8     trans_id       count
    9        query      string
    10      qclass       count
    11 qclass_name      string
    12       qtype       count
    13  qtype_name      string
    14       rcode       count
    15  rcode_name      string
    16          QR       bool 
    17          AA       bool 
    18       TC RD  bool bool 
    19          RA       bool 
    20           Z       count
    21     answers      vector
    22        TTLs      vector
    23    rejected       bool 
                                                                                           Description
    1                                                                    Timestamp of the DNS request 
    2                                                                     Unique id of the connection 
    3                                                ID record with orig/resp host/port. See conn.log 
    4                                                                                      Origin port
    5                                                                                      Response IP
    6                                                                                    Response port
    7                                                        Protocol of DNS transaction – TCP or UDP 
    8                                       16 bit identifier assigned by DNS client; responses match 
    9                                                                Domain name subject of the query 
    10                                                               Value specifying the query class 
    11                                          Descriptive name of the query class (e.g. C_INTERNET) 
    12                                                                Value specifying the query type 
    13                                                     Name of the query type (e.g. A, AAAA, PTR) 
    14                                                        Response code value in the DNS response 
    15                                 Descriptive name of the response code (e.g. NOERROR, NXDOMAIN) 
    16                                        Was this a query or a response? T = response, F = query 
    17                                    Authoritative Answer. T = server is authoritative for query 
    18 Truncation. T = message was truncated Recursion Desired. T = request recursive lookup of query 
    19                                     Recursion Available. T = server supports recursive queries 
    20                                      Reserved field, should be zero in all queries & responses 
    21                                           List of resource descriptions in answer to the query 
    22                                                               Caching intervals of the answers 
    23                                               Whether the DNS query was rejected by the server 

``` r
normal <- dataCSV[,1]
colnames(dataLog) <- normal
dataLog$ts <- as.POSIXct(dataLog$ts,origin = "1970-01-01")
```

``` r
glimpse(dataCSV)
```

    Rows: 23
    Columns: 3
    $ Field       <chr> "ts ", "uid ", "id.orig_h", "id.orig_p", "id.resp_h", "id.…
    $ Type        <chr> "time ", "string", "addr", "port", "addr", "port", "proto"…
    $ Description <chr> "Timestamp of the DNS request ", "Unique id of the connect…

``` r
glimpse(dataLog)
```

    Rows: 427,935
    Columns: 24
    $ `ts `          <dbl> 1331901006, 1331901015, 1331901016, 1331901017, 1331901…
    $ `uid `         <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jljz…
    $ id.orig_h      <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76", …
    $ id.orig_p      <int> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137…
    $ id.resp_h      <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255",…
    $ id.resp_p      <int> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, …
    $ `proto `       <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp",…
    $ `trans_id `    <int> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 62187,…
    $ `query `       <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x…
    $ `qclass `      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", …
    $ `qclass_name ` <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET",…
    $ `qtype `       <chr> "33", "32", "32", "32", "32", "32", "32", "32", "32", "…
    $ `qtype_name `  <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", …
    $ `rcode `       <chr> "0", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", …
    $ `rcode_name `  <chr> "NOERROR", "-", "-", "-", "-", "-", "-", "-", "-", "-",…
    $ `QR `          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    $ `AA `          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    $ `TC RD `       <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
    $ `RA `          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    $ `Z `           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1…
    $ `answers `     <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", …
    $ `TTLs `        <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", …
    $ `rejected `    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    $ ts             <dttm> 2012-03-16 16:30:05, 2012-03-16 16:30:15, 2012-03-16 1…

## Какое соотношение участников обмена внутри сети и участников обращений к внешним ресурсам?

## Найтип топ-10 участников в сети, проявляющих большую активность?

``` r
ansUsers <- dataLog %>% group_by(id.orig_h) %>% summarise(req = n()) %>% arrange(desc(req)) %>% top_n(10,req)
ansUsers
```

    # A tibble: 10 × 2
       id.orig_h         req
       <chr>           <int>
     1 10.10.117.210   75943
     2 192.168.202.93  26522
     3 192.168.202.103 18121
     4 192.168.202.76  16978
     5 192.168.202.97  16176
     6 192.168.202.141 14967
     7 10.10.117.209   14222
     8 192.168.202.110 13372
     9 192.168.203.63  12148
    10 192.168.202.106 10784

## Найти топ-10 доменов, к которым обращались пользователи и количество обращений

``` r
ansDomains <- dataLog %>% group_by(domain = tolower(`query `)) %>% summarise(req = n()) %>% arrange(desc(req)) %>% top_n(10,req)
ansDomains
```

    # A tibble: 10 × 2
       domain                                                                    req
       <chr>                                                                   <int>
     1 "teredo.ipv6.microsoft.com"                                             39273
     2 "tools.google.com"                                                      14057
     3 "www.apple.com"                                                         13390
     4 "time.apple.com"                                                        13109
     5 "safebrowsing.clients.google.com"                                       11658
     6 "wpad"                                                                  11429
     7 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x… 10401
     8 "isatap"                                                                 9712
     9 "44.206.168.192.in-addr.arpa"                                            7248
    10 "hpe8aa67"                                                               6929

## Определить базовые статистические характеристики интервала времени между последовательным обращениям к топ-10 доменам

``` r
ansDomainsF <- dataLog %>% filter(tolower(`query `) %in% ansDomains$domain) %>% arrange(ts)
interval <- diff(ansDomainsF$ts)
summary(interval)
```

      Length    Class     Mode 
      137205 difftime  numeric 

## Часто вредоносное программное обеспечение использует DNS канал в качестве каналауправления, периодически отправляя запросы на подконтрольный злоумышленникам DNS сервер. По периодическим запросам на один и тот же домен можно выявить скрытый DNS канал. Есть ли такие IP адреса в исследуемом датасете?

``` r
ipshki <- dataLog %>% group_by(ip = id.orig_h, domain = tolower(`query `)) %>% summarise(req = n(), .groups = "drop") %>% filter(req > 1)
unqIpshki <- unique(ipshki$ip)
unqIpshki %>% length()
```

    [1] 240

``` r
unqIpshki
```

      [1] "10.10.10.10"                         
      [2] "10.10.117.209"                       
      [3] "10.10.117.210"                       
      [4] "128.244.37.196"                      
      [5] "169.254.109.123"                     
      [6] "169.254.228.26"                      
      [7] "172.16.42.42"                        
      [8] "192.168.0.199"                       
      [9] "192.168.0.3"                         
     [10] "192.168.100.130"                     
     [11] "192.168.202.100"                     
     [12] "192.168.202.101"                     
     [13] "192.168.202.102"                     
     [14] "192.168.202.103"                     
     [15] "192.168.202.106"                     
     [16] "192.168.202.107"                     
     [17] "192.168.202.108"                     
     [18] "192.168.202.109"                     
     [19] "192.168.202.110"                     
     [20] "192.168.202.112"                     
     [21] "192.168.202.113"                     
     [22] "192.168.202.115"                     
     [23] "192.168.202.116"                     
     [24] "192.168.202.117"                     
     [25] "192.168.202.118"                     
     [26] "192.168.202.119"                     
     [27] "192.168.202.120"                     
     [28] "192.168.202.121"                     
     [29] "192.168.202.122"                     
     [30] "192.168.202.123"                     
     [31] "192.168.202.124"                     
     [32] "192.168.202.125"                     
     [33] "192.168.202.126"                     
     [34] "192.168.202.128"                     
     [35] "192.168.202.129"                     
     [36] "192.168.202.130"                     
     [37] "192.168.202.131"                     
     [38] "192.168.202.132"                     
     [39] "192.168.202.133"                     
     [40] "192.168.202.134"                     
     [41] "192.168.202.135"                     
     [42] "192.168.202.136"                     
     [43] "192.168.202.137"                     
     [44] "192.168.202.138"                     
     [45] "192.168.202.139"                     
     [46] "192.168.202.140"                     
     [47] "192.168.202.141"                     
     [48] "192.168.202.143"                     
     [49] "192.168.202.144"                     
     [50] "192.168.202.145"                     
     [51] "192.168.202.146"                     
     [52] "192.168.202.147"                     
     [53] "192.168.202.148"                     
     [54] "192.168.202.149"                     
     [55] "192.168.202.150"                     
     [56] "192.168.202.151"                     
     [57] "192.168.202.152"                     
     [58] "192.168.202.153"                     
     [59] "192.168.202.154"                     
     [60] "192.168.202.155"                     
     [61] "192.168.202.156"                     
     [62] "192.168.202.157"                     
     [63] "192.168.202.222"                     
     [64] "192.168.202.240"                     
     [65] "192.168.202.33"                      
     [66] "192.168.202.4"                       
     [67] "192.168.202.49"                      
     [68] "192.168.202.62"                      
     [69] "192.168.202.63"                      
     [70] "192.168.202.64"                      
     [71] "192.168.202.65"                      
     [72] "192.168.202.67"                      
     [73] "192.168.202.68"                      
     [74] "192.168.202.71"                      
     [75] "192.168.202.73"                      
     [76] "192.168.202.74"                      
     [77] "192.168.202.75"                      
     [78] "192.168.202.76"                      
     [79] "192.168.202.77"                      
     [80] "192.168.202.78"                      
     [81] "192.168.202.79"                      
     [82] "192.168.202.80"                      
     [83] "192.168.202.81"                      
     [84] "192.168.202.83"                      
     [85] "192.168.202.84"                      
     [86] "192.168.202.85"                      
     [87] "192.168.202.86"                      
     [88] "192.168.202.87"                      
     [89] "192.168.202.88"                      
     [90] "192.168.202.89"                      
     [91] "192.168.202.90"                      
     [92] "192.168.202.91"                      
     [93] "192.168.202.92"                      
     [94] "192.168.202.93"                      
     [95] "192.168.202.94"                      
     [96] "192.168.202.95"                      
     [97] "192.168.202.96"                      
     [98] "192.168.202.97"                      
     [99] "192.168.202.98"                      
    [100] "192.168.203.45"                      
    [101] "192.168.203.61"                      
    [102] "192.168.203.62"                      
    [103] "192.168.203.63"                      
    [104] "192.168.203.64"                      
    [105] "192.168.203.66"                      
    [106] "192.168.204.45"                      
    [107] "192.168.204.59"                      
    [108] "192.168.204.60"                      
    [109] "192.168.204.70"                      
    [110] "192.168.204.71"                      
    [111] "192.168.204.72"                      
    [112] "192.168.205.253"                     
    [113] "192.168.207.4"                       
    [114] "192.168.208.18"                      
    [115] "192.168.21.1"                        
    [116] "192.168.21.100"                      
    [117] "192.168.21.102"                      
    [118] "192.168.21.103"                      
    [119] "192.168.21.152"                      
    [120] "192.168.21.202"                      
    [121] "192.168.21.203"                      
    [122] "192.168.21.25"                       
    [123] "192.168.21.252"                      
    [124] "192.168.21.253"                      
    [125] "192.168.21.254"                      
    [126] "192.168.22.1"                        
    [127] "192.168.22.100"                      
    [128] "192.168.22.101"                      
    [129] "192.168.22.102"                      
    [130] "192.168.22.103"                      
    [131] "192.168.22.152"                      
    [132] "192.168.22.202"                      
    [133] "192.168.22.203"                      
    [134] "192.168.22.25"                       
    [135] "192.168.22.252"                      
    [136] "192.168.22.253"                      
    [137] "192.168.22.254"                      
    [138] "192.168.229.1"                       
    [139] "192.168.229.101"                     
    [140] "192.168.229.252"                     
    [141] "192.168.229.254"                     
    [142] "192.168.23.1"                        
    [143] "192.168.23.100"                      
    [144] "192.168.23.101"                      
    [145] "192.168.23.102"                      
    [146] "192.168.23.103"                      
    [147] "192.168.23.152"                      
    [148] "192.168.23.202"                      
    [149] "192.168.23.203"                      
    [150] "192.168.23.25"                       
    [151] "192.168.23.252"                      
    [152] "192.168.23.253"                      
    [153] "192.168.23.254"                      
    [154] "192.168.24.1"                        
    [155] "192.168.24.102"                      
    [156] "192.168.24.103"                      
    [157] "192.168.24.152"                      
    [158] "192.168.24.202"                      
    [159] "192.168.24.203"                      
    [160] "192.168.24.25"                       
    [161] "192.168.24.254"                      
    [162] "192.168.25.1"                        
    [163] "192.168.25.100"                      
    [164] "192.168.25.101"                      
    [165] "192.168.25.102"                      
    [166] "192.168.25.103"                      
    [167] "192.168.25.152"                      
    [168] "192.168.25.202"                      
    [169] "192.168.25.203"                      
    [170] "192.168.25.25"                       
    [171] "192.168.25.254"                      
    [172] "192.168.26.1"                        
    [173] "192.168.26.103"                      
    [174] "192.168.26.152"                      
    [175] "192.168.26.25"                       
    [176] "192.168.26.254"                      
    [177] "192.168.27.1"                        
    [178] "192.168.27.100"                      
    [179] "192.168.27.101"                      
    [180] "192.168.27.102"                      
    [181] "192.168.27.103"                      
    [182] "192.168.27.152"                      
    [183] "192.168.27.202"                      
    [184] "192.168.27.203"                      
    [185] "192.168.27.25"                       
    [186] "192.168.27.253"                      
    [187] "192.168.27.254"                      
    [188] "192.168.28.1"                        
    [189] "192.168.28.101"                      
    [190] "192.168.28.102"                      
    [191] "192.168.28.103"                      
    [192] "192.168.28.152"                      
    [193] "192.168.28.202"                      
    [194] "192.168.28.25"                       
    [195] "192.168.28.253"                      
    [196] "192.168.28.254"                      
    [197] "192.168.51.38"                       
    [198] "192.168.95.166"                      
    [199] "2001:dbb:c18:202:20c:29ff:fe41:4be7" 
    [200] "2001:dbb:c18:202:20c:29ff:fe78:1023" 
    [201] "2001:dbb:c18:202:20c:29ff:fe93:571e" 
    [202] "2001:dbb:c18:202:216:d3ff:fe4b:70d"  
    [203] "2001:dbb:c18:202:4172:3555:4717:3e0c"
    [204] "2001:dbb:c18:202:4c3a:e571:4cfc:b70c"
    [205] "2001:dbb:c18:202:6128:bec8:28c6:8c7b"
    [206] "2001:dbb:c18:202:71f3:e219:3f6a:4311"
    [207] "2001:dbb:c18:202:9c6b:be4e:1289:2663"
    [208] "2001:dbb:c18:202:a1dd:6355:28ae:da1e"
    [209] "2001:dbb:c18:202:a800:4ff:fe00:a04"  
    [210] "2001:dbb:c18:202:b9ac:6976:ae1c:e10b"
    [211] "2001:dbb:c18:202:bc5c:15c1:ec81:1e08"
    [212] "2001:dbb:c18:202:d4bc:e39f:84ad:5001"
    [213] "2001:dbb:c18:202:d557:eac5:3728:41ee"
    [214] "2001:dbb:c18:202:f2de:f1ff:fe9b:ad6a"
    [215] "2001:dbb:c18:203:226:18ff:fef9:be98" 
    [216] "2001:dbb:c18:204:a800:4ff:fe00:a04"  
    [217] "fe80::11a:f507:d853:a03d"            
    [218] "fe80::20c:29ff:fe41:4be7"            
    [219] "fe80::216:d3ff:fe4b:70d"             
    [220] "fe80::223:dfff:fe97:4e12"            
    [221] "fe80::3e07:54ff:fe1c:a665"           
    [222] "fe80::3e07:54ff:fe41:3ed3"           
    [223] "fe80::3ed0:f8ff:fe34:4765"           
    [224] "fe80::4172:3555:4717:3e0c"           
    [225] "fe80::4c3a:e571:4cfc:b70c"           
    [226] "fe80::4c9b:aad8:8a6a:7bb0"           
    [227] "fe80::62c5:47ff:fe93:381e"           
    [228] "fe80::62fb:42ff:feef:5440"           
    [229] "fe80::65ca:c6cd:7ae0:ac8c"           
    [230] "fe80::88f4:9f4d:feec:8072"           
    [231] "fe80::9c8a:5786:bbb0:3db8"           
    [232] "fe80::a800:4ff:fe00:a04"             
    [233] "fe80::ba8d:12ff:fe12:3f90"           
    [234] "fe80::ba8d:12ff:fe53:a8d8"           
    [235] "fe80::bc5c:15c1:ec81:1e08"           
    [236] "fe80::c62c:3ff:fe30:7333"            
    [237] "fe80::c62c:3ff:fe37:efc"             
    [238] "fe80::d69a:20ff:fef9:b49c"           
    [239] "fe80::d840:5635:ef48:b032"           
    [240] "fe80::f2de:f1ff:fe9b:ad6a"           

## Определите местоположение и организацию-провайдера для топ-10 доменов. Для этого можно использовать сторонние сервисы.
