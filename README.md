### CBR System (Prever o preço de Carros utilizando o algoritmo K-Nearest Neighbors)


    library(tidyverse)

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.7.2     ✔ stringr 1.2.0
    ## ✔ readr   1.1.1     ✔ forcats 0.2.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

    file <- read.csv("data.csv")
    file[1:10,]

    ##    Make      Model Year            Engine.Fuel.Type Engine.HP
    ## 1   BMW 1 Series M 2011 premium unleaded (required)       335
    ## 2   BMW   1 Series 2011 premium unleaded (required)       300
    ## 3   BMW   1 Series 2011 premium unleaded (required)       300
    ## 4   BMW   1 Series 2011 premium unleaded (required)       230
    ## 5   BMW   1 Series 2011 premium unleaded (required)       230
    ## 6   BMW   1 Series 2012 premium unleaded (required)       230
    ## 7   BMW   1 Series 2012 premium unleaded (required)       300
    ## 8   BMW   1 Series 2012 premium unleaded (required)       300
    ## 9   BMW   1 Series 2012 premium unleaded (required)       230
    ## 10  BMW   1 Series 2013 premium unleaded (required)       230
    ##    Engine.Cylinders Transmission.Type    Driven_Wheels Number.of.Doors
    ## 1                 6            MANUAL rear wheel drive               2
    ## 2                 6            MANUAL rear wheel drive               2
    ## 3                 6            MANUAL rear wheel drive               2
    ## 4                 6            MANUAL rear wheel drive               2
    ## 5                 6            MANUAL rear wheel drive               2
    ## 6                 6            MANUAL rear wheel drive               2
    ## 7                 6            MANUAL rear wheel drive               2
    ## 8                 6            MANUAL rear wheel drive               2
    ## 9                 6            MANUAL rear wheel drive               2
    ## 10                6            MANUAL rear wheel drive               2
    ##                          Market.Category Vehicle.Size Vehicle.Style
    ## 1  Factory Tuner,Luxury,High-Performance      Compact         Coupe
    ## 2                     Luxury,Performance      Compact   Convertible
    ## 3                Luxury,High-Performance      Compact         Coupe
    ## 4                     Luxury,Performance      Compact         Coupe
    ## 5                                 Luxury      Compact   Convertible
    ## 6                     Luxury,Performance      Compact         Coupe
    ## 7                     Luxury,Performance      Compact   Convertible
    ## 8                Luxury,High-Performance      Compact         Coupe
    ## 9                                 Luxury      Compact   Convertible
    ## 10                                Luxury      Compact   Convertible
    ##    highway.MPG city.mpg Popularity  MSRP
    ## 1           26       19       3916 46135
    ## 2           28       19       3916 40650
    ## 3           28       20       3916 36350
    ## 4           28       18       3916 29450
    ## 5           28       18       3916 34500
    ## 6           28       18       3916 31200
    ## 7           26       17       3916 44100
    ## 8           28       20       3916 39300
    ## 9           28       18       3916 36900
    ## 10          27       18       3916 37200

    nv_dataset <- file %>%
      mutate(
        Make = as.numeric(factor(Make)),
        Model = as.numeric(factor(Model)),
        `Engine.Fuel.Type` = as.numeric(factor(`Engine.Fuel.Type`)),
        `Transmission.Type` = as.numeric(factor(`Transmission.Type`)),
        Driven_Wheels = as.numeric(factor(Driven_Wheels)),
        `Market.Category` = as.numeric(factor(`Market.Category`)),
        `Vehicle.Size` = as.numeric(factor(`Vehicle.Size`)),
        `Vehicle.Style` = as.numeric(factor(`Vehicle.Style`))) 

    nv_dataset[1:10,]

    ##    Make Model Year Engine.Fuel.Type Engine.HP Engine.Cylinders
    ## 1     6     5 2011               10       335                6
    ## 2     6     4 2011               10       300                6
    ## 3     6     4 2011               10       300                6
    ## 4     6     4 2011               10       230                6
    ## 5     6     4 2011               10       230                6
    ## 6     6     4 2012               10       230                6
    ## 7     6     4 2012               10       300                6
    ## 8     6     4 2012               10       300                6
    ## 9     6     4 2012               10       230                6
    ## 10    6     4 2013               10       230                6
    ##    Transmission.Type Driven_Wheels Number.of.Doors Market.Category
    ## 1                  4             4               2              39
    ## 2                  4             4               2              68
    ## 3                  4             4               2              65
    ## 4                  4             4               2              68
    ## 5                  4             4               2              64
    ## 6                  4             4               2              68
    ## 7                  4             4               2              68
    ## 8                  4             4               2              65
    ## 9                  4             4               2              64
    ## 10                 4             4               2              64
    ##    Vehicle.Size Vehicle.Style highway.MPG city.mpg Popularity  MSRP
    ## 1             1             9          26       19       3916 46135
    ## 2             1             7          28       19       3916 40650
    ## 3             1             9          28       20       3916 36350
    ## 4             1             9          28       18       3916 29450
    ## 5             1             7          28       18       3916 34500
    ## 6             1             9          28       18       3916 31200
    ## 7             1             7          26       17       3916 44100
    ## 8             1             9          28       20       3916 39300
    ## 9             1             7          28       18       3916 36900
    ## 10            1             7          27       18       3916 37200

Agora é possível rodar KNN aproveitando todos os preditores.

    num.na.data <- apply(nv_dataset, 1, 
                         function(x){any(is.na(x))})
                                        noquote(paste('Dados incompletos: ', sum(num.na.data)))

    ## [1] Dados incompletos:  99

Há 99 especificações de carros com dados incompletos (ou não possuem a
funcionalidade ou estão simplesmente faltando).

    nv_dataset <- na.omit(nv_dataset)
    num.na.data <- apply(nv_dataset, 1, 
                         function(x){any(is.na(x))})
                                        noquote(paste('Dados incompletos: ', sum(num.na.data)))

    ## [1] Dados incompletos:  0

    nv_dataset$id <- 1:nrow(nv_dataset)

Particionando os dados

    train <- nv_dataset %>% sample_frac(.8)
    test <- anti_join(nv_dataset, train, by = "id") 

Treinamento

    set.seed(13)

    preco_real <- c(test %>% select(MSRP))$MSRP

      model.knn <- caret::train(MSRP ~ .,
               tuneGrid = expand.grid(k = seq(1,10,1)),
               data = train,
               method = "knn")

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

      preco_pred <- predict(model.knn, test %>% select(-MSRP))
      hydroGOF::mse(preco_real, preco_pred)

    ## [1] 155209998

    model.knn

    ## k-Nearest Neighbors 
    ## 
    ## 9452 samples
    ##   16 predictors
    ## 
    ## No pre-processing
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 9452, 9452, 9452, 9452, 9452, 9452, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   k   RMSE      Rsquared   MAE     
    ##    1  26801.36  0.7987763  4958.552
    ##    2  29086.10  0.7680688  5208.811
    ##    3  30520.40  0.7470573  5468.921
    ##    4  31941.92  0.7265677  5756.977
    ##    5  33631.44  0.6994707  6088.100
    ##    6  34433.96  0.6870114  6389.077
    ##    7  35516.45  0.6694156  6689.085
    ##    8  36266.27  0.6557802  6999.081
    ##    9  36960.92  0.6435110  7262.145
    ##   10  37690.40  0.6304601  7565.936
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was k = 1.

Observa-se que o modelo KNN não se mostrou adequado para prever o preço
desses carros. O melhor modelo agrupou K = 1 e assim procurou reproduzir
as observações de treino ou aumentou o erro em outros modelos para k
&gt; 1. Espera-se que o padrão continue com mais dados, como o erro
cresceu linearmente com o K, no exemplo acima.

    plot(model.knn)

![](CBR_System__copy__files/figure-markdown_strict/unnamed-chunk-9-1.png)
