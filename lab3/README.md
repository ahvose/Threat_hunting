# Обработка данных в языке R - 2
Хвощев Алексей, БИСО-03-20

## Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Научиться работать с пакетом dlpyr

## Ход работы

## Подключим нужные нам пакеты для работы

``` r
library(dplyr)
```

    Warning: пакет 'dplyr' был собран под R версии 4.3.2


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

``` r
library(nycflights13)
```

    Warning: пакет 'nycflights13' был собран под R версии 4.3.2

## Сколько датафреймов в пакете nycflights13?

Их 5

## Сколько строк в каждом из датафреймов?

``` r
nycflights13::weather %>% nrow()
```

    [1] 26115

``` r
nycflights13::planes %>% nrow()
```

    [1] 3322

``` r
nycflights13::airports %>% nrow()
```

    [1] 1458

``` r
nycflights13::airlines %>% nrow()
```

    [1] 16

``` r
nycflights13::flights %>% nrow()
```

    [1] 336776

## Сколько стоблцов в каждом из датафреймов

``` r
nycflights13::weather %>% ncol()
```

    [1] 15

``` r
nycflights13::planes %>% ncol()
```

    [1] 9

``` r
nycflights13::airports %>% ncol()
```

    [1] 8

``` r
nycflights13::airlines %>% ncol()
```

    [1] 2

``` r
nycflights13::flights %>% ncol()
```

    [1] 19

## Как посмотреть на сам датафрейм?

``` r
nycflights13::flights %>% glimpse()
```

    Rows: 336,776
    Columns: 19
    $ year           <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2…
    $ month          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ day            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ dep_time       <int> 517, 533, 542, 544, 554, 554, 555, 557, 557, 558, 558, …
    $ sched_dep_time <int> 515, 529, 540, 545, 600, 558, 600, 600, 600, 600, 600, …
    $ dep_delay      <dbl> 2, 4, 2, -1, -6, -4, -5, -3, -3, -2, -2, -2, -2, -2, -1…
    $ arr_time       <int> 830, 850, 923, 1004, 812, 740, 913, 709, 838, 753, 849,…
    $ sched_arr_time <int> 819, 830, 850, 1022, 837, 728, 854, 723, 846, 745, 851,…
    $ arr_delay      <dbl> 11, 20, 33, -18, -25, 12, 19, -14, -8, 8, -2, -3, 7, -1…
    $ carrier        <chr> "UA", "UA", "AA", "B6", "DL", "UA", "B6", "EV", "B6", "…
    $ flight         <int> 1545, 1714, 1141, 725, 461, 1696, 507, 5708, 79, 301, 4…
    $ tailnum        <chr> "N14228", "N24211", "N619AA", "N804JB", "N668DN", "N394…
    $ origin         <chr> "EWR", "LGA", "JFK", "JFK", "LGA", "EWR", "EWR", "LGA"A…
    $ dest           <chr> "IAH", "IAH", "MIA", "BQN", "ATL", "ORD", "FLL", "IAD",…
    $ time_hour  <dttm> 2013-01-01 01:00:00, 2013-01-01 02:00:00, 2013-01-01 03:00…

## Сколько компаний перевозчиков учитывает наборы данных?

``` r
nycflights13::airlines %>% nrow()
```

    [1] 16

## Сколько рейсов было принято аэропортом John F Kennedy Intl в мае?

``` r
nycflights13::flights %>% filter(month == 5, origin == "JFK") %>% nrow
```

    Warning: Using one column matrices in `filter()` was deprecated in dplyr 1.1.0.
    ℹ Please use one dimensional logical vectors instead.

    [1] 0

## Самый северный аэропорт?

``` r
nycflights13::airports %>% arrange(desc(lat)) %>% select(name,alt) %>% top_n(1)
```

    # A tibble: 1 × 2
      name                      lat
      <chr>                   <dbl>
    1 Dillant Hopkins Airport  72.3

## Самый высокогорный аэропорт?

``` r
nycflights13::airports %>% arrange(desc(alt)) %>% select(name,alt) %>% top_n(1)
```

    Selecting by alt

    # A tibble: 1 × 2
      name        alt
      <chr>     <dbl>
    1 Telluride  9078

## Какие бортовые номера у старых самолётов?

``` r
nycflights13::planes %>% arrange(year)
```

    # A tibble: 1 × 2
      tailnum  year
      <chr>   <int>
    1 N381AA   1956

## Какая средняя температура воздуха была в аэропорту John F Kennedy Intl в сентябре

``` r
nycflights13::weather %>% filter(month == 9, origin == "JFK") %>% summarise("temperature" = ((temp_mean = mean(temp, 0, na.rm = TRUE))-32)*0.55556)
```

    # A tibble: 1 × 1
      temperature
            <dbl>
    1        19.4

## Самолеты какой авиакомпании совершили больше всего вылетов в июне?

``` r
temp <- nycflights13::flights %>% group_by(carrier) %>% summarise("count_of_flights" = n())
a1 <- arrange(temp, desc(count_of_flights))
top <- top_n(a1, 1)
```

    Selecting by count_of_flights

``` r
filter(nycflights13::airlines, carrier == top$carrier)
```

    # A tibble: 1 × 2
      carrier name                 
      <chr>   <chr>                
    1 UA      United Air Lines Inc.

## Какие самолёты (какой авиакомпании) задержались чаще других в 2013 г.?

``` r
temp1 <- nycflights13::flights %>% group_by(carrier) %>% filter(dep_delay > 0, arr_delay > 0) %>% summarise("counts" = n())
a2 <- arrange(temp1, desc(counts))
top2 <- top_n(a2, 1)
```

    Selecting by counts

``` r
filter(nycflights13::airlines, carrier == top2$carrier)
```

    # A tibble: 1 × 2
      carrier name                    
      <chr>   <chr>                   
    1 EV      ExpressJet Airlines Inc.
