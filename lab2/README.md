# lab2

## Цель работы

Проанализировать встроенный в пакет dplyr набор данных starwars с
помощью языка R и ответить на вопросы

## Ход работы

Загрузим пакет dplyr:

``` r
library(dplyr)
```

    Warning: пакет 'dplyr' был собран под R версии 4.3.1


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

1.Сколько строк в датафрейме?

``` r
starwars |> nrow()
```

    [1] 87

2.Сколько столбцов в датафрейме?

``` r
starwars |> ncol()
```

    [1] 14

3.Как просмотреть примерный вид датафрейма?

``` r
starwars |> glimpse()
```

    Rows: 87
    Columns: 14
    $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
    $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
    $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
    $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
    $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
    $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
    $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
    $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
    $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
    $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
    $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
    $ films      <list> <"The Empire Strikes Back", "Revenge of the Sith", "Return…
    $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
    $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

4.Сколько уникальных рас персонажей (species) представлено в данных?

``` r
unique(starwars$species)
```

     [1] "Human"          "Droid"          "Wookiee"        "Rodian"        
     [5] "Hutt"           "Yoda's species" "Trandoshan"     "Mon Calamari"  
     [9] "Ewok"           "Sullustan"      "Neimodian"      "Gungan"        
    [13] NA               "Toydarian"      "Dug"            "Zabrak"        
    [17] "Twi'lek"        "Vulptereen"     "Xexto"          "Toong"         
    [21] "Cerean"         "Nautolan"       "Tholothian"     "Iktotchi"      
    [25] "Quermian"       "Kel Dor"        "Chagrian"       "Geonosian"     
    [29] "Mirialan"       "Clawdite"       "Besalisk"       "Kaminoan"      
    [33] "Aleena"         "Skakoan"        "Muun"           "Togruta"       
    [37] "Kaleesh"        "Pau'an"        

5.Найти самого высокого персонажа.

``` r
starwars |> slice_max (height) |> select (name, height)
```

    # A tibble: 1 × 2
      name        height
      <chr>        <int>
    1 Yarael Poof    264

6.Найти всех персонажей ниже 170

``` r
starwars |> filter (height < 170) 
```

    # A tibble: 23 × 14
       name     height  mass hair_color skin_color eye_color birth_year sex   gender
       <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
     1 C-3PO       167    75 <NA>       gold       yellow           112 none  mascu…
     2 R2-D2        96    32 <NA>       white, bl… red               33 none  mascu…
     3 Leia Or…    150    49 brown      light      brown             19 fema… femin…
     4 Beru Wh…    165    75 brown      light      blue              47 fema… femin…
     5 R5-D4        97    32 <NA>       white, red red               NA none  mascu…
     6 Yoda         66    17 white      green      brown            896 male  mascu…
     7 Mon Mot…    150    NA auburn     fair       blue              48 fema… femin…
     8 Wicket …     88    20 brown      brown      brown              8 male  mascu…
     9 Nien Nu…    160    68 none       grey       black             NA male  mascu…
    10 Watto       137    NA black      blue, grey yellow            NA male  mascu…
    # ℹ 13 more rows
    # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
    #   vehicles <list>, starships <list>

7.Подсчитать ИМТ (индекс массы тела) для всех персонажей. ИМТ подсчитать
по формуле 𝐼 = 𝑚 ℎ2 , где 𝑚 – масса (weight), а ℎ – рост (height).

``` r
starwars |> mutate(IMT= mass / (height/100) ** 2) |> select (name, IMT)
```

    # A tibble: 87 × 2
       name                 IMT
       <chr>              <dbl>
     1 Luke Skywalker      26.0
     2 C-3PO               26.9
     3 R2-D2               34.7
     4 Darth Vader         33.3
     5 Leia Organa         21.8
     6 Owen Lars           37.9
     7 Beru Whitesun lars  27.5
     8 R5-D4               34.0
     9 Biggs Darklighter   25.1
    10 Obi-Wan Kenobi      23.2
    # ℹ 77 more rows

8.Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по
отношению массы (mass) к росту (height) персонажей.

``` r
starwars |> mutate(elong = mass / height) |> slice_max (elong, n = 10) |> select(name, elong)
```

    # A tibble: 10 × 2
       name                  elong
       <chr>                 <dbl>
     1 Jabba Desilijic Tiure 7.76 
     2 Grievous              0.736
     3 IG-88                 0.7  
     4 Owen Lars             0.674
     5 Darth Vader           0.673
     6 Jek Tono Porkins      0.611
     7 Bossk                 0.595
     8 Tarfful               0.581
     9 Dexter Jettster       0.515
    10 Chewbacca             0.491

9.Найти средний возраст персонажей каждой расы вселенной Звездных войн.

``` r
starwars |> group_by (species) |>  summarise("Mean age" = mean(birth_year, na.rm = TRUE)) |> na.omit()
```

    # A tibble: 15 × 2
       species        `Mean age`
       <chr>               <dbl>
     1 Cerean               92  
     2 Droid                53.3
     3 Ewok                  8  
     4 Gungan               52  
     5 Human                53.4
     6 Hutt                600  
     7 Kel Dor              22  
     8 Mirialan             49  
     9 Mon Calamari         41  
    10 Rodian               44  
    11 Trandoshan           53  
    12 Twi'lek              48  
    13 Wookiee             200  
    14 Yoda's species      896  
    15 Zabrak               54  

10.Найти самый распространенный цвет глаз персонажей вселенной Звездных
войн

``` r
starwars |> group_by (eye_color) |>  summarise("count" = n()) |> slice_max(count)
```

    # A tibble: 1 × 2
      eye_color count
      <chr>     <int>
    1 brown        21

11.Подсчитать среднюю длину имени в каждой расе вселенной Звездных войн.

``` r
starwars |> mutate ("length" = nchar((name))) |> group_by(species) |> summarise( mean(length))
```

    # A tibble: 38 × 2
       species   `mean(length)`
       <chr>              <dbl>
     1 Aleena             13   
     2 Besalisk           15   
     3 Cerean             12   
     4 Chagrian           10   
     5 Clawdite           10   
     6 Droid               4.83
     7 Dug                 7   
     8 Ewok               21   
     9 Geonosian          17   
    10 Gungan             11.7 
    # ℹ 28 more rows
