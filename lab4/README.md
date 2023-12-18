# lab3

## Цель работы

1.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R
3.  Закрепить навыки исследования метаданных DNS трафика

## Ход работы

Для начала загрузим библиотеку:

``` r
library(dplyr)
```

    Warning: пакет 'dplyr' был собран под R версии 4.3.1


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    Warning: пакет 'tidyverse' был собран под R версии 4.3.1

    Warning: пакет 'ggplot2' был собран под R версии 4.3.1

    Warning: пакет 'tibble' был собран под R версии 4.3.1

    Warning: пакет 'tidyr' был собран под R версии 4.3.1

    Warning: пакет 'readr' был собран под R версии 4.3.1

    Warning: пакет 'purrr' был собран под R версии 4.3.1

    Warning: пакет 'forcats' был собран под R версии 4.3.1

    Warning: пакет 'lubridate' был собран под R версии 4.3.1

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ forcats   1.0.0     ✔ readr     2.1.4
    ✔ ggplot2   3.4.4     ✔ stringr   1.5.0
    ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ✔ purrr     1.0.2     ✔ tidyr     1.3.0

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

### Подготовка данных

1.Импортируйте данные DNS

``` r
head <- read.csv("header.csv")
log<-read.csv("dns.log", sep = '\t', header = FALSE)
```

2.Добавьте пропущенные данные о структуре данных (назначении столбцов)

``` r
names(log) <- c("ts","uid","id.orig_h","id.orig_p","id.resp_h","id.resp_p","proto","trans_id","query","qclass","qclass_name","qtype","qtype_name","rcode","rcode_name","AA","TC", "RD","RA","Z","answers","TTLs","rejected")
```

3.Преобразуйте данные в столбцах в нужный формат

4.Посмотрите общую структуру данных с помощью функции glimpse()

``` r
glimpse(log)
```

    Rows: 427,935
    Columns: 23
    $ ts          <dbl> 1331901006, 1331901015, 1331901016, 1331901017, 1331901006…
    $ uid         <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jljz7Bs…
    $ id.orig_h   <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76", "19…
    $ id.orig_p   <int> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 1…
    $ id.resp_h   <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255", "1…
    $ id.resp_p   <int> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137…
    $ proto       <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp", "u…
    $ trans_id    <int> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 62187, 62…
    $ query       <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\…
    $ qclass      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"…
    $ qclass_name <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET", "C…
    $ qtype       <chr> "33", "32", "32", "32", "32", "32", "32", "32", "32", "32"…
    $ qtype_name  <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB…
    $ rcode       <chr> "0", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ rcode_name  <chr> "NOERROR", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-…
    $ AA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ TC          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ RD          <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRU…
    $ RA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ Z           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0…
    $ answers     <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ TTLs        <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ rejected    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…

###Анализ 4. Сколько участников информационного обмена в сети Доброй
Организации?

``` r
log %>% count(log$id.orig_h) %>% nrow()
```

    [1] 253

1.  Какое соотношение участников обмена внутри сети и участников
    обращений к внешним ресурсам?

``` r
ins <- filter(log, qtype_name == 'A'| qtype_name == 'AA' | qtype_name =='AAA' | qtype_name == 'AAAA') %>% group_by(uid) %>% count() %>% nrow() 
outs <- filter(log, qtype_name != 'A', qtype_name !='AA', qtype_name !='AAA', qtype_name !='AAAA') %>% group_by(uid) %>% count() %>% nrow()
ins/outs
```

    [1] 1.96667

1.  Найдите топ-10 участников сети, проявляющих наибольшую сетевую
    активность.

``` r
select(log,uid) %>% group_by(uid) %>% count() %>% arrange(desc(n)) %>% head(10)
```

    # A tibble: 10 × 2
    # Groups:   uid [10]
       uid                    n
       <chr>              <int>
     1 CHwsqo48JzsgOOx5u5 18622
     2 C69INN8eedNqxIAs2   9082
     3 CQX0cW23Ci1D08eA78  8914
     4 CM87gu1xIgp1q0nWej  8101
     5 CzNRck2zqMl2K4BvIh  7724
     6 CFHJkD5qSIAnIDAnb   6140
     7 CrASxG4WWqN5lFYZpd  5403
     8 Cvfa4A2CK3vpoyJO9   4621
     9 CZ6P023bXFwrV0Waxj  2829
    10 Cq7OOsGzpAIeJK3x7   2318

1.  Найдите топ-10 доменов, к которым обращаются пользователи сети и
    соответственное количество обращений

``` r
domens <- log %>% filter(query !='-', qtype_name == 'A'| qtype_name == 'AA' | qtype_name =='AAA' | qtype_name == 'AAAA') %>% select(query) %>% group_by(query) %>% count() %>% arrange(desc(n)) %>% head(10)
domens
```

    # A tibble: 10 × 2
    # Groups:   query [10]
       query                               n
       <chr>                           <int>
     1 teredo.ipv6.microsoft.com       39273
     2 tools.google.com                14057
     3 www.apple.com                   13390
     4 safebrowsing.clients.google.com 11658
     5 imap.gmail.com                   5543
     6 stats.norton.com                 5537
     7 www.google.com                   5171
     8 ratings-wrs.symantec.com         4464
     9 api.twitter.com                  4348
    10 api.facebook.com                 4137

1.  Опеределите базовые статистические характеристики (функция
    summary()) интервала времени между последовательным обращениями к
    топ-10 доменам.

``` r
summary(diff((log %>% filter(tolower(query) %in% domens$query) %>% arrange(ts))$ts))
```

        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
        0.00     0.00     0.00     1.08     0.31 49924.53 

1.  Часто вредоносное программное обеспечение использует DNS канал в
    качестве канала управления, периодически отправляя запросы на
    подконтрольный злоумышленникам DNS сервер. По периодическим запросам
    на один и тот же домен можно выявить скрытый DNS канал. Есть ли
    такие IP адреса в исследуемом датасете?

``` r
t <- log %>% group_by(id.orig_h, query) %>% summarise(total = n()) %>% filter(total > 1)
```

    `summarise()` has grouped output by 'id.orig_h'. You can override using the
    `.groups` argument.

``` r
unique(t$id.orig_h)%>% head()
```

    [1] "10.10.10.10"     "10.10.117.209"   "10.10.117.210"   "128.244.37.196" 
    [5] "169.254.109.123" "169.254.228.26" 
