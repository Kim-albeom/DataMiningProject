Breast_Cancer_code_01
================

## 1. Intro & Purpose

요약하자면, 나는 이 커널에서 많은 분류 방법을 사용했습니다. 이 커널이 이
분야의 초보자에게 도움이 되기를 바랍니다!

각 함수를 더 잘 이해하기 위해 함수 바로 위에 라이브러리를 작성했습니다.

## 2. Data Importing & Cleaning & Inspecting

### 2-1) Import dataset

breast_cancer_data means ‘wisconsin breast cancer data’

``` r
breast_cancer_data <- read.csv("./data/breast_cancer.csv", header=T, stringsAsFactors=F)

tail(breast_cancer_data)
```

    ##         id diagnosis radius_mean texture_mean perimeter_mean area_mean
    ## 564 926125         M       20.92        25.09         143.00    1347.0
    ## 565 926424         M       21.56        22.39         142.00    1479.0
    ## 566 926682         M       20.13        28.25         131.20    1261.0
    ## 567 926954         M       16.60        28.08         108.30     858.1
    ## 568 927241         M       20.60        29.33         140.10    1265.0
    ## 569  92751         B        7.76        24.54          47.92     181.0
    ##     smoothness_mean compactness_mean concavity_mean concave.points_mean
    ## 564         0.10990          0.22360        0.31740             0.14740
    ## 565         0.11100          0.11590        0.24390             0.13890
    ## 566         0.09780          0.10340        0.14400             0.09791
    ## 567         0.08455          0.10230        0.09251             0.05302
    ## 568         0.11780          0.27700        0.35140             0.15200
    ## 569         0.05263          0.04362        0.00000             0.00000
    ##     symmetry_mean fractal_dimension_mean radius_se texture_se perimeter_se
    ## 564        0.2149                0.06879    0.9622      1.026        8.758
    ## 565        0.1726                0.05623    1.1760      1.256        7.673
    ## 566        0.1752                0.05533    0.7655      2.463        5.203
    ## 567        0.1590                0.05648    0.4564      1.075        3.425
    ## 568        0.2397                0.07016    0.7260      1.595        5.772
    ## 569        0.1587                0.05884    0.3857      1.428        2.548
    ##     area_se smoothness_se compactness_se concavity_se concave.points_se
    ## 564  118.80      0.006399        0.04310      0.07845           0.02624
    ## 565  158.70      0.010300        0.02891      0.05198           0.02454
    ## 566   99.04      0.005769        0.02423      0.03950           0.01678
    ## 567   48.55      0.005903        0.03731      0.04730           0.01557
    ## 568   86.22      0.006522        0.06158      0.07117           0.01664
    ## 569   19.15      0.007189        0.00466      0.00000           0.00000
    ##     symmetry_se fractal_dimension_se radius_worst texture_worst perimeter_worst
    ## 564     0.02057             0.006213       24.290         29.41          179.10
    ## 565     0.01114             0.004239       25.450         26.40          166.10
    ## 566     0.01898             0.002498       23.690         38.25          155.00
    ## 567     0.01318             0.003892       18.980         34.12          126.70
    ## 568     0.02324             0.006185       25.740         39.42          184.60
    ## 569     0.02676             0.002783        9.456         30.37           59.16
    ##     area_worst smoothness_worst compactness_worst concavity_worst
    ## 564     1819.0          0.14070           0.41860          0.6599
    ## 565     2027.0          0.14100           0.21130          0.4107
    ## 566     1731.0          0.11660           0.19220          0.3215
    ## 567     1124.0          0.11390           0.30940          0.3403
    ## 568     1821.0          0.16500           0.86810          0.9387
    ## 569      268.6          0.08996           0.06444          0.0000
    ##     concave.points_worst symmetry_worst fractal_dimension_worst  X
    ## 564               0.2542         0.2929                 0.09873 NA
    ## 565               0.2216         0.2060                 0.07115 NA
    ## 566               0.1628         0.2572                 0.06637 NA
    ## 567               0.1418         0.2218                 0.07820 NA
    ## 568               0.2650         0.4087                 0.12400 NA
    ## 569               0.0000         0.2871                 0.07039 NA

### 2-2) Remove NULL Data

``` r
breast_cancer_data$X <- NULL
```

### 2-3) Reshape the datasets

``` r
# breast_cancer_data 첫 번째 열을 제외한 모든 열로 구성
breast_cancer_data <- breast_cancer_data[,-1]

# breast_cancer_data$diagnosis의 각 값이 "B"인지 확인하고, 
#"B"이면 "Benign"으로, 그렇지 않으면 "Malignant"으로 변환
breast_cancer_data$diagnosis <- factor(ifelse(breast_cancer_data$diagnosis=="B","Benign","Malignant"))
```

### 2-4) Inspect the datasets

#### structure

``` r
str(breast_cancer_data)
```

    ## 'data.frame':    569 obs. of  31 variables:
    ##  $ diagnosis              : Factor w/ 2 levels "Benign","Malignant": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ radius_mean            : num  18 20.6 19.7 11.4 20.3 ...
    ##  $ texture_mean           : num  10.4 17.8 21.2 20.4 14.3 ...
    ##  $ perimeter_mean         : num  122.8 132.9 130 77.6 135.1 ...
    ##  $ area_mean              : num  1001 1326 1203 386 1297 ...
    ##  $ smoothness_mean        : num  0.1184 0.0847 0.1096 0.1425 0.1003 ...
    ##  $ compactness_mean       : num  0.2776 0.0786 0.1599 0.2839 0.1328 ...
    ##  $ concavity_mean         : num  0.3001 0.0869 0.1974 0.2414 0.198 ...
    ##  $ concave.points_mean    : num  0.1471 0.0702 0.1279 0.1052 0.1043 ...
    ##  $ symmetry_mean          : num  0.242 0.181 0.207 0.26 0.181 ...
    ##  $ fractal_dimension_mean : num  0.0787 0.0567 0.06 0.0974 0.0588 ...
    ##  $ radius_se              : num  1.095 0.543 0.746 0.496 0.757 ...
    ##  $ texture_se             : num  0.905 0.734 0.787 1.156 0.781 ...
    ##  $ perimeter_se           : num  8.59 3.4 4.58 3.44 5.44 ...
    ##  $ area_se                : num  153.4 74.1 94 27.2 94.4 ...
    ##  $ smoothness_se          : num  0.0064 0.00522 0.00615 0.00911 0.01149 ...
    ##  $ compactness_se         : num  0.049 0.0131 0.0401 0.0746 0.0246 ...
    ##  $ concavity_se           : num  0.0537 0.0186 0.0383 0.0566 0.0569 ...
    ##  $ concave.points_se      : num  0.0159 0.0134 0.0206 0.0187 0.0188 ...
    ##  $ symmetry_se            : num  0.03 0.0139 0.0225 0.0596 0.0176 ...
    ##  $ fractal_dimension_se   : num  0.00619 0.00353 0.00457 0.00921 0.00511 ...
    ##  $ radius_worst           : num  25.4 25 23.6 14.9 22.5 ...
    ##  $ texture_worst          : num  17.3 23.4 25.5 26.5 16.7 ...
    ##  $ perimeter_worst        : num  184.6 158.8 152.5 98.9 152.2 ...
    ##  $ area_worst             : num  2019 1956 1709 568 1575 ...
    ##  $ smoothness_worst       : num  0.162 0.124 0.144 0.21 0.137 ...
    ##  $ compactness_worst      : num  0.666 0.187 0.424 0.866 0.205 ...
    ##  $ concavity_worst        : num  0.712 0.242 0.45 0.687 0.4 ...
    ##  $ concave.points_worst   : num  0.265 0.186 0.243 0.258 0.163 ...
    ##  $ symmetry_worst         : num  0.46 0.275 0.361 0.664 0.236 ...
    ##  $ fractal_dimension_worst: num  0.1189 0.089 0.0876 0.173 0.0768 ...

#### summary

``` r
summary(breast_cancer_data)
```

    ##      diagnosis    radius_mean      texture_mean   perimeter_mean  
    ##  Benign   :357   Min.   : 6.981   Min.   : 9.71   Min.   : 43.79  
    ##  Malignant:212   1st Qu.:11.700   1st Qu.:16.17   1st Qu.: 75.17  
    ##                  Median :13.370   Median :18.84   Median : 86.24  
    ##                  Mean   :14.127   Mean   :19.29   Mean   : 91.97  
    ##                  3rd Qu.:15.780   3rd Qu.:21.80   3rd Qu.:104.10  
    ##                  Max.   :28.110   Max.   :39.28   Max.   :188.50  
    ##    area_mean      smoothness_mean   compactness_mean  concavity_mean   
    ##  Min.   : 143.5   Min.   :0.05263   Min.   :0.01938   Min.   :0.00000  
    ##  1st Qu.: 420.3   1st Qu.:0.08637   1st Qu.:0.06492   1st Qu.:0.02956  
    ##  Median : 551.1   Median :0.09587   Median :0.09263   Median :0.06154  
    ##  Mean   : 654.9   Mean   :0.09636   Mean   :0.10434   Mean   :0.08880  
    ##  3rd Qu.: 782.7   3rd Qu.:0.10530   3rd Qu.:0.13040   3rd Qu.:0.13070  
    ##  Max.   :2501.0   Max.   :0.16340   Max.   :0.34540   Max.   :0.42680  
    ##  concave.points_mean symmetry_mean    fractal_dimension_mean   radius_se     
    ##  Min.   :0.00000     Min.   :0.1060   Min.   :0.04996        Min.   :0.1115  
    ##  1st Qu.:0.02031     1st Qu.:0.1619   1st Qu.:0.05770        1st Qu.:0.2324  
    ##  Median :0.03350     Median :0.1792   Median :0.06154        Median :0.3242  
    ##  Mean   :0.04892     Mean   :0.1812   Mean   :0.06280        Mean   :0.4052  
    ##  3rd Qu.:0.07400     3rd Qu.:0.1957   3rd Qu.:0.06612        3rd Qu.:0.4789  
    ##  Max.   :0.20120     Max.   :0.3040   Max.   :0.09744        Max.   :2.8730  
    ##    texture_se      perimeter_se       area_se        smoothness_se     
    ##  Min.   :0.3602   Min.   : 0.757   Min.   :  6.802   Min.   :0.001713  
    ##  1st Qu.:0.8339   1st Qu.: 1.606   1st Qu.: 17.850   1st Qu.:0.005169  
    ##  Median :1.1080   Median : 2.287   Median : 24.530   Median :0.006380  
    ##  Mean   :1.2169   Mean   : 2.866   Mean   : 40.337   Mean   :0.007041  
    ##  3rd Qu.:1.4740   3rd Qu.: 3.357   3rd Qu.: 45.190   3rd Qu.:0.008146  
    ##  Max.   :4.8850   Max.   :21.980   Max.   :542.200   Max.   :0.031130  
    ##  compactness_se      concavity_se     concave.points_se   symmetry_se      
    ##  Min.   :0.002252   Min.   :0.00000   Min.   :0.000000   Min.   :0.007882  
    ##  1st Qu.:0.013080   1st Qu.:0.01509   1st Qu.:0.007638   1st Qu.:0.015160  
    ##  Median :0.020450   Median :0.02589   Median :0.010930   Median :0.018730  
    ##  Mean   :0.025478   Mean   :0.03189   Mean   :0.011796   Mean   :0.020542  
    ##  3rd Qu.:0.032450   3rd Qu.:0.04205   3rd Qu.:0.014710   3rd Qu.:0.023480  
    ##  Max.   :0.135400   Max.   :0.39600   Max.   :0.052790   Max.   :0.078950  
    ##  fractal_dimension_se  radius_worst   texture_worst   perimeter_worst 
    ##  Min.   :0.0008948    Min.   : 7.93   Min.   :12.02   Min.   : 50.41  
    ##  1st Qu.:0.0022480    1st Qu.:13.01   1st Qu.:21.08   1st Qu.: 84.11  
    ##  Median :0.0031870    Median :14.97   Median :25.41   Median : 97.66  
    ##  Mean   :0.0037949    Mean   :16.27   Mean   :25.68   Mean   :107.26  
    ##  3rd Qu.:0.0045580    3rd Qu.:18.79   3rd Qu.:29.72   3rd Qu.:125.40  
    ##  Max.   :0.0298400    Max.   :36.04   Max.   :49.54   Max.   :251.20  
    ##    area_worst     smoothness_worst  compactness_worst concavity_worst 
    ##  Min.   : 185.2   Min.   :0.07117   Min.   :0.02729   Min.   :0.0000  
    ##  1st Qu.: 515.3   1st Qu.:0.11660   1st Qu.:0.14720   1st Qu.:0.1145  
    ##  Median : 686.5   Median :0.13130   Median :0.21190   Median :0.2267  
    ##  Mean   : 880.6   Mean   :0.13237   Mean   :0.25427   Mean   :0.2722  
    ##  3rd Qu.:1084.0   3rd Qu.:0.14600   3rd Qu.:0.33910   3rd Qu.:0.3829  
    ##  Max.   :4254.0   Max.   :0.22260   Max.   :1.05800   Max.   :1.2520  
    ##  concave.points_worst symmetry_worst   fractal_dimension_worst
    ##  Min.   :0.00000      Min.   :0.1565   Min.   :0.05504        
    ##  1st Qu.:0.06493      1st Qu.:0.2504   1st Qu.:0.07146        
    ##  Median :0.09993      Median :0.2822   Median :0.08004        
    ##  Mean   :0.11461      Mean   :0.2901   Mean   :0.08395        
    ##  3rd Qu.:0.16140      3rd Qu.:0.3179   3rd Qu.:0.09208        
    ##  Max.   :0.29100      Max.   :0.6638   Max.   :0.20750

#### head

``` r
tail(breast_cancer_data)
```

    ##     diagnosis radius_mean texture_mean perimeter_mean area_mean smoothness_mean
    ## 564 Malignant       20.92        25.09         143.00    1347.0         0.10990
    ## 565 Malignant       21.56        22.39         142.00    1479.0         0.11100
    ## 566 Malignant       20.13        28.25         131.20    1261.0         0.09780
    ## 567 Malignant       16.60        28.08         108.30     858.1         0.08455
    ## 568 Malignant       20.60        29.33         140.10    1265.0         0.11780
    ## 569    Benign        7.76        24.54          47.92     181.0         0.05263
    ##     compactness_mean concavity_mean concave.points_mean symmetry_mean
    ## 564          0.22360        0.31740             0.14740        0.2149
    ## 565          0.11590        0.24390             0.13890        0.1726
    ## 566          0.10340        0.14400             0.09791        0.1752
    ## 567          0.10230        0.09251             0.05302        0.1590
    ## 568          0.27700        0.35140             0.15200        0.2397
    ## 569          0.04362        0.00000             0.00000        0.1587
    ##     fractal_dimension_mean radius_se texture_se perimeter_se area_se
    ## 564                0.06879    0.9622      1.026        8.758  118.80
    ## 565                0.05623    1.1760      1.256        7.673  158.70
    ## 566                0.05533    0.7655      2.463        5.203   99.04
    ## 567                0.05648    0.4564      1.075        3.425   48.55
    ## 568                0.07016    0.7260      1.595        5.772   86.22
    ## 569                0.05884    0.3857      1.428        2.548   19.15
    ##     smoothness_se compactness_se concavity_se concave.points_se symmetry_se
    ## 564      0.006399        0.04310      0.07845           0.02624     0.02057
    ## 565      0.010300        0.02891      0.05198           0.02454     0.01114
    ## 566      0.005769        0.02423      0.03950           0.01678     0.01898
    ## 567      0.005903        0.03731      0.04730           0.01557     0.01318
    ## 568      0.006522        0.06158      0.07117           0.01664     0.02324
    ## 569      0.007189        0.00466      0.00000           0.00000     0.02676
    ##     fractal_dimension_se radius_worst texture_worst perimeter_worst area_worst
    ## 564             0.006213       24.290         29.41          179.10     1819.0
    ## 565             0.004239       25.450         26.40          166.10     2027.0
    ## 566             0.002498       23.690         38.25          155.00     1731.0
    ## 567             0.003892       18.980         34.12          126.70     1124.0
    ## 568             0.006185       25.740         39.42          184.60     1821.0
    ## 569             0.002783        9.456         30.37           59.16      268.6
    ##     smoothness_worst compactness_worst concavity_worst concave.points_worst
    ## 564          0.14070           0.41860          0.6599               0.2542
    ## 565          0.14100           0.21130          0.4107               0.2216
    ## 566          0.11660           0.19220          0.3215               0.1628
    ## 567          0.11390           0.30940          0.3403               0.1418
    ## 568          0.16500           0.86810          0.9387               0.2650
    ## 569          0.08996           0.06444          0.0000               0.0000
    ##     symmetry_worst fractal_dimension_worst
    ## 564         0.2929                 0.09873
    ## 565         0.2060                 0.07115
    ## 566         0.2572                 0.06637
    ## 567         0.2218                 0.07820
    ## 568         0.4087                 0.12400
    ## 569         0.2871                 0.07039

------------------------------------------------------------------------

## 3. Analyze the Correlation between variables

### 3-1) Correlation between each variables

correalation plot를 그리는 여러 가지 방법

각 데이터에 서로 다른 함수(mean, se, worst)를 적용해봤음

#### Mean

``` r
# suppressWarnings() : 경고메세지 무시
suppressWarnings({
  library(PerformanceAnalytics)
  chart.Correlation(breast_cancer_data[,c(2:11)],histogram=TRUE, col="grey10", pch=1, main="Cancer Mean")
})
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

#### SE

``` r
library(psych)
pairs.panels(breast_cancer_data[,c(12:21)], method="pearson",
             hist.col = "#1fbbfa", density=TRUE, ellipses=TRUE, show.points = TRUE,
             pch=1, lm=TRUE, cex.cor=1, smoother=F, stars = T, main="Cancer SE")
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

#### Worst

``` r
library(ggplot2)
library(GGally)
ggpairs(breast_cancer_data[,c(22:31)],)+ theme_bw()+
labs(title="Cancer Worst")+
theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=13))
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### 3-2) See the relation between each variables (diagnosis included)

combined data보다 diagnosis가 포함된 플롯을 보는 것이 훨씬 더 중요하다고
생각\[3-1\]

#### Mean

``` r
ggpairs(breast_cancer_data[,c(2:11,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
labs(title="Cancer Mean")+
theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

#### SE

``` r
ggpairs(breast_cancer_data[,c(12:21,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
labs(title="Cancer SE")+
theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

#### Worst

``` r
ggpairs(breast_cancer_data[,c(22:31,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
labs(title="Cancer Worst")+
theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### 3-3) See the ggcorr plot

By ggcorr, we can see the correlation value more directly than above
graph.

#### Mean

``` r
ggcorr(breast_cancer_data[,c(2:11)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
labs(title="Cancer Mean")+
theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

#### SE

``` r
ggcorr(breast_cancer_data[,c(12:21)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
labs(title="Cancer SE")+
theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

#### Worst

``` r
ggcorr(breast_cancer_data[,c(22:31)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
labs(title="Cancer Worst")+
theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

------------------------------------------------------------------------

## 4. Principal Component Analysis (PCA)

변수가 너무 많으면 아래와 같은 문제가 발생할 수 있습니다.

- 컴퓨터 처리량 증가

- 너무 복잡한 시각화 문제

- 분석에 영향을 주지 않는 변수를 포함시켜 효율성을 떨어뜨림

- 데이터 해석을 어렵게 만든다

위의 ggcorr 플롯을 보면 \[3-3\] 상관관계 값이 높다는 것은 변수 간에
“다중 공선성”이 있음을 의미합니다.

-\> 상관관계가 높은 변수를 줄여 하나의 주요 구성요소를 모델 개발에
사용합니다.

\*\*주성분의 수를 결정할 때, 누적 기여율을 사용 또는 screeplot을
사용하고 고유값 곡선이 수평으로 놓여 있는 주성분의 이전 단계를
사용합니다.

PCA는 스케일 차이로 인한 데이터 왜곡을 방지하기 위해 표준화된 데이터를
사용합니다.

``` r
library(factoextra)
breast_cancer_pca <- transform(breast_cancer_data)  
```

### 4-1) Summary

PCA 결과 Cumulative Proportion(누적비율)율이 85% 이상이면 number of
principal components(주성분의 수)로 판단할 수 있다.

- View Point(시점) : Cumulative Proportion

For example, if cumulative proportion of PC4 is 88.7, it means **the sum
of proportion of PC1~PC4** is 88.7

#### All

The cumulative proportion from PC1 to PC6 is about 88.7%. (85%이상)

It means that PC1~PC6 can explain 88.7% of the whole data.

``` r
all_pca <- prcomp(breast_cancer_pca[,-1], cor=TRUE, scale = TRUE)
summary(all_pca)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     3.6444 2.3857 1.67867 1.40735 1.28403 1.09880 0.82172
    ## Proportion of Variance 0.4427 0.1897 0.09393 0.06602 0.05496 0.04025 0.02251
    ## Cumulative Proportion  0.4427 0.6324 0.72636 0.79239 0.84734 0.88759 0.91010
    ##                            PC8    PC9    PC10   PC11    PC12    PC13    PC14
    ## Standard deviation     0.69037 0.6457 0.59219 0.5421 0.51104 0.49128 0.39624
    ## Proportion of Variance 0.01589 0.0139 0.01169 0.0098 0.00871 0.00805 0.00523
    ## Cumulative Proportion  0.92598 0.9399 0.95157 0.9614 0.97007 0.97812 0.98335
    ##                           PC15    PC16    PC17    PC18    PC19    PC20   PC21
    ## Standard deviation     0.30681 0.28260 0.24372 0.22939 0.22244 0.17652 0.1731
    ## Proportion of Variance 0.00314 0.00266 0.00198 0.00175 0.00165 0.00104 0.0010
    ## Cumulative Proportion  0.98649 0.98915 0.99113 0.99288 0.99453 0.99557 0.9966
    ##                           PC22    PC23   PC24    PC25    PC26    PC27    PC28
    ## Standard deviation     0.16565 0.15602 0.1344 0.12442 0.09043 0.08307 0.03987
    ## Proportion of Variance 0.00091 0.00081 0.0006 0.00052 0.00027 0.00023 0.00005
    ## Cumulative Proportion  0.99749 0.99830 0.9989 0.99942 0.99969 0.99992 0.99997
    ##                           PC29    PC30
    ## Standard deviation     0.02736 0.01153
    ## Proportion of Variance 0.00002 0.00000
    ## Cumulative Proportion  1.00000 1.00000

#### Mean

The cumulative proportion from PC1 to PC3 is about 88.7%. (above 85%)

``` r
mean_pca <- prcomp(breast_cancer_pca[,c(2:11)], scale = TRUE)
summary(mean_pca)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3    PC4     PC5     PC6     PC7
    ## Standard deviation     2.3406 1.5870 0.93841 0.7064 0.61036 0.35234 0.28299
    ## Proportion of Variance 0.5479 0.2519 0.08806 0.0499 0.03725 0.01241 0.00801
    ## Cumulative Proportion  0.5479 0.7997 0.88779 0.9377 0.97495 0.98736 0.99537
    ##                            PC8     PC9    PC10
    ## Standard deviation     0.18679 0.10552 0.01680
    ## Proportion of Variance 0.00349 0.00111 0.00003
    ## Cumulative Proportion  0.99886 0.99997 1.00000

#### SE

The cumulative proportion from PC1 to PC4 is about 86.7%. (above 85%)

``` r
se_pca <- prcomp(breast_cancer_pca[,c(12:21)], scale = TRUE)
summary(se_pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     2.1779 1.4406 1.1245 0.77095 0.75991 0.57939 0.43512
    ## Proportion of Variance 0.4743 0.2075 0.1264 0.05944 0.05775 0.03357 0.01893
    ## Cumulative Proportion  0.4743 0.6819 0.8083 0.86774 0.92548 0.95905 0.97798
    ##                           PC8     PC9    PC10
    ## Standard deviation     0.3962 0.20436 0.14635
    ## Proportion of Variance 0.0157 0.00418 0.00214
    ## Cumulative Proportion  0.9937 0.99786 1.00000

#### Worst

The cumulative proportion from PC1 to PC3 is about 85.8%. (above 85%)

``` r
worst_pca <- prcomp(breast_cancer_pca[,c(22:31)], scale = TRUE)
summary(worst_pca)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     2.3869 1.4443 0.89597 0.73531 0.71741 0.42862 0.28959
    ## Proportion of Variance 0.5697 0.2086 0.08028 0.05407 0.05147 0.01837 0.00839
    ## Cumulative Proportion  0.5697 0.7783 0.85860 0.91267 0.96413 0.98251 0.99089
    ##                            PC8     PC9    PC10
    ## Standard deviation     0.26802 0.12343 0.06326
    ## Proportion of Variance 0.00718 0.00152 0.00040
    ## Cumulative Proportion  0.99808 0.99960 1.00000

### 4-2) Screeplot

principal components(주성분)으로 설명되는 The percentage of
variability(변동성의 백분율)은 screeplot를 통해 확인할수 있음

=\> View Point : principal components where the line lies.

#### All

Line lies at point PC6

``` r
fviz_eig(all_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
labs(title = "Cancer All Variances - PCA",
         x = "Principal Components", y = "% of variances")
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

#### Mean

Line lies at point PC4

``` r
fviz_eig(mean_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
labs(title = "Cancer Mean Variances - PCA",
         x = "Principal Components", y = "% of variances")
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

#### SE

Line lies at point PC4

``` r
fviz_eig(se_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
labs(title = "Cancer SE Variances - PCA",
         x = "Principal Components", y = "% of variances")
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

#### Worst

Line lies at point PC4

``` r
fviz_eig(worst_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
labs(title = "Cancer Worst Variances - PCA",
         x = "Principal Components", y = "% of variances")
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

### 4-3) Get PCA Variables

#### All

##### Get PCA Variables

``` r
all_var <- get_pca_var(all_pca)
all_var
```

    ## Principal Component Analysis Results for variables
    ##  ===================================================
    ##   Name       Description                                    
    ## 1 "$coord"   "Coordinates for the variables"                
    ## 2 "$cor"     "Correlations between variables and dimensions"
    ## 3 "$cos2"    "Cos2 for the variables"                       
    ## 4 "$contrib" "contributions of the variables"

##### Quality of representation of PCA

Correlation between variables and PCA

``` r
library("corrplot")
corrplot(all_var$cos2, is.corr=FALSE)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

##### Contributions of variables to PCA

To highlight the most contributing variables for each components

``` r
corrplot(all_var$contrib, is.corr=FALSE)    
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

##### Contributions of variables to PC1 & PC2

``` r
library(gridExtra)
p1 <- fviz_contrib(all_pca, choice="var", axes=1, fill="pink", color="grey", top=10)
p2 <- fviz_contrib(all_pca, choice="var", axes=2, fill="skyblue", color="grey", top=10)
grid.arrange(p1,p2,ncol=2)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

#### Mean

##### Get PCA Variables

``` r
mean_var <- get_pca_var(mean_pca)
mean_var
```

    ## Principal Component Analysis Results for variables
    ##  ===================================================
    ##   Name       Description                                    
    ## 1 "$coord"   "Coordinates for the variables"                
    ## 2 "$cor"     "Correlations between variables and dimensions"
    ## 3 "$cos2"    "Cos2 for the variables"                       
    ## 4 "$contrib" "contributions of the variables"

##### Quality of representation of PCA

Correlation between variables and PCA

``` r
library("corrplot")
corrplot(mean_var$cos2, is.corr=FALSE)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

##### Contributions of variables to PCA

To highlight the most contributing variables for each components

``` r
corrplot(mean_var$contrib, is.corr=FALSE)   
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

##### Contributions of variables to PC1 & PC2

``` r
library(gridExtra)
p1 <- fviz_contrib(mean_pca, choice="var", axes=1, fill="pink", color="grey", top=10)
p2 <- fviz_contrib(mean_pca, choice="var", axes=2, fill="skyblue", color="grey", top=10)
grid.arrange(p1,p2,ncol=2)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

#### SE

##### Get PCA Variables

``` r
se_var <- get_pca_var(se_pca)
se_var
```

    ## Principal Component Analysis Results for variables
    ##  ===================================================
    ##   Name       Description                                    
    ## 1 "$coord"   "Coordinates for the variables"                
    ## 2 "$cor"     "Correlations between variables and dimensions"
    ## 3 "$cos2"    "Cos2 for the variables"                       
    ## 4 "$contrib" "contributions of the variables"

##### Quality of representation of PCA

Correlation between variables and PCA

``` r
library("corrplot")
corrplot(se_var$cos2, is.corr=FALSE)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

##### Contributions of variables to PCA

To highlight the most contributing variables for each components

``` r
corrplot(se_var$contrib, is.corr=FALSE) 
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

##### Contributions of variables to PC1 & PC2

``` r
library(gridExtra)
p1 <- fviz_contrib(se_pca, choice="var", axes=1, fill="pink", color="grey", top=10)
p2 <- fviz_contrib(se_pca, choice="var", axes=2, fill="skyblue", color="grey", top=10)
grid.arrange(p1,p2,ncol=2)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

#### Worst

##### Get PCA Variables

``` r
worst_var <- get_pca_var(worst_pca)
worst_var
```

    ## Principal Component Analysis Results for variables
    ##  ===================================================
    ##   Name       Description                                    
    ## 1 "$coord"   "Coordinates for the variables"                
    ## 2 "$cor"     "Correlations between variables and dimensions"
    ## 3 "$cos2"    "Cos2 for the variables"                       
    ## 4 "$contrib" "contributions of the variables"

##### Quality of representation of PCA

Correlation between variables and PCA

``` r
library("corrplot")
corrplot(worst_var$cos2, is.corr=FALSE)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

##### Contributions of variables to PCA

To highlight the most contributing variables for each components

``` r
corrplot(worst_var$contrib, is.corr=FALSE)  
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

##### Contributions of variables to PC1 & PC2

``` r
library(gridExtra)
p1 <- fviz_contrib(worst_pca, choice="var", axes=1, fill="pink", color="grey", top=10)
p2 <- fviz_contrib(worst_pca, choice="var", axes=2, fill="skyblue", color="grey", top=10)
grid.arrange(p1,p2,ncol=2)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

### 4-4) See the plot - color variables by groups

value centers : 위에서 선택한 optimal principal component 값을 넣음

#### All

optimal PC value : PC1~PC6

``` r
set.seed(218)
res.all <- kmeans(all_var$coord, centers = 6, nstart = 25)
grp <- as.factor(res.all$cluster)

fviz_pca_var(all_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

#### Mean

optimal PC value : PC1~PC3

``` r
set.seed(218)
res.mean <- kmeans(mean_var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.mean$cluster)

fviz_pca_var(mean_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

#### SE

optimal PC value : PC1~PC4

``` r
set.seed(218)
res.se <- kmeans(se_var$coord, centers = 4, nstart = 25)
grp <- as.factor(res.se$cluster)

fviz_pca_var(se_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

#### Worst

optimal PC value : PC1~PC3

``` r
set.seed(218)
res.worst <- kmeans(worst_var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.worst$cluster)

fviz_pca_var(worst_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

### 4-5) See the Biplot

``` r
library("factoextra")
```

#### All

``` r
fviz_pca_biplot(all_pca, col.ind = breast_cancer_data$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

#### Mean

``` r
fviz_pca_biplot(mean_pca, col.ind = breast_cancer_data$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

#### SE

``` r
fviz_pca_biplot(se_pca, col.ind = breast_cancer_data$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

#### Worst

``` r
fviz_pca_biplot(worst_pca, col.ind = breast_cancer_data$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

------------------------------------------------------------------------

## 5. 모든 ML 방법을 적용하고 서로 비교하여 가장 적합한 것을 선택합니다.

### 5-1) Make test & train dataset for testing classification ML methods

Shuffle the wbcd data(100%) & Make train dataset(70%), test dataset(30%)

분류 ML 방법 테스트를 위한 테스트 및 학습 데이터 세트 만들기 wbcd 데이터
섞기(100%) & 기차 데이터세트 만들기(70%), 테스트 데이터세트(30%)

``` r
nrows <- NROW(breast_cancer_data)
set.seed(218)                           ## fix random value
index <- sample(1:nrows, 0.7 * nrows)   ## shuffle and divide

#train <- wbcd                          ## 569 test data (100%)
train <- breast_cancer_data[index,]                 ## 398 test data (70%)
test <- breast_cancer_data[-index,]                     ## 171 test data (30%)
```

### 5-2) Check the proportion of diagnosis (Benign / Malignant)

#### train

``` r
prop.table(table(train$diagnosis))
```

    ## 
    ##    Benign Malignant 
    ## 0.6180905 0.3819095

#### test

``` r
prop.table(table(test$diagnosis))
```

    ## 
    ##    Benign Malignant 
    ## 0.6491228 0.3508772

### 5-3) Apply every ML methods(that I know) to data

``` r
library(caret)
```

#### C5.0

``` r
library(C50)
learn_c50 <- C5.0(train[,-1],train$diagnosis)
pre_c50 <- predict(learn_c50, test[,-1])
cm_c50 <- confusionMatrix(pre_c50, test$diagnosis)
cm_c50
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       108         4
    ##   Malignant      3        56
    ##                                           
    ##                Accuracy : 0.9591          
    ##                  95% CI : (0.9175, 0.9834)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.9098          
    ##                                           
    ##  Mcnemar's Test P-Value : 1               
    ##                                           
    ##             Sensitivity : 0.9730          
    ##             Specificity : 0.9333          
    ##          Pos Pred Value : 0.9643          
    ##          Neg Pred Value : 0.9492          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6316          
    ##    Detection Prevalence : 0.6550          
    ##       Balanced Accuracy : 0.9532          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

#### C5.0 - Tune

##### Choose ‘trials’ which shows best predict performance in C5.0

``` r
library(C50)

acc_test <- numeric()
accuracy1 <- NULL; accuracy2 <- NULL

for(i in 1:50){
    learn_imp_c50 <- C5.0(train[,-1],train$diagnosis,trials = i)      
    p_c50 <- predict(learn_imp_c50, test[,-1]) 
    accuracy1 <- confusionMatrix(p_c50, test$diagnosis)
    accuracy2[i] <- accuracy1$overall[1]
}

acc <- data.frame(t= seq(1,50), cnt = accuracy2)

opt_t <- subset(acc, cnt==max(cnt))[1,]
sub <- paste("Optimal number of trials is", opt_t$t, "(accuracy :", opt_t$cnt,") in C5.0")

library(highcharter)
hchart(acc, 'line', hcaes(t, cnt)) %>%
  hc_title(text = "Accuracy With Varying Trials (C5.0)") %>%
  hc_subtitle(text = sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Trials")) %>%
  hc_yAxis(title = list(text = "Accuracy"))
```

<div class="highchart html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-f0b2584f0496ef1945c9" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-f0b2584f0496ef1945c9">{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"Accuracy With Varying Trials (C5.0)"},"yAxis":{"title":{"text":"Accuracy"},"type":"linear"},"credits":{"enabled":false},"exporting":{"enabled":false},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}}},"series":[{"group":"group","data":[{"t":1,"cnt":0.9590643274853801,"x":1,"y":0.9590643274853801},{"t":2,"cnt":0.9590643274853801,"x":2,"y":0.9590643274853801},{"t":3,"cnt":0.9824561403508771,"x":3,"y":0.9824561403508771},{"t":4,"cnt":0.9707602339181286,"x":4,"y":0.9707602339181286},{"t":5,"cnt":0.9824561403508771,"x":5,"y":0.9824561403508771},{"t":6,"cnt":0.9766081871345029,"x":6,"y":0.9766081871345029},{"t":7,"cnt":0.9824561403508771,"x":7,"y":0.9824561403508771},{"t":8,"cnt":0.9649122807017544,"x":8,"y":0.9649122807017544},{"t":9,"cnt":0.9707602339181286,"x":9,"y":0.9707602339181286},{"t":10,"cnt":0.9766081871345029,"x":10,"y":0.9766081871345029},{"t":11,"cnt":0.9707602339181286,"x":11,"y":0.9707602339181286},{"t":12,"cnt":0.9824561403508771,"x":12,"y":0.9824561403508771},{"t":13,"cnt":0.9766081871345029,"x":13,"y":0.9766081871345029},{"t":14,"cnt":0.9766081871345029,"x":14,"y":0.9766081871345029},{"t":15,"cnt":0.9707602339181286,"x":15,"y":0.9707602339181286},{"t":16,"cnt":0.9649122807017544,"x":16,"y":0.9649122807017544},{"t":17,"cnt":0.9649122807017544,"x":17,"y":0.9649122807017544},{"t":18,"cnt":0.9707602339181286,"x":18,"y":0.9707602339181286},{"t":19,"cnt":0.9766081871345029,"x":19,"y":0.9766081871345029},{"t":20,"cnt":0.9766081871345029,"x":20,"y":0.9766081871345029},{"t":21,"cnt":0.9707602339181286,"x":21,"y":0.9707602339181286},{"t":22,"cnt":0.9590643274853801,"x":22,"y":0.9590643274853801},{"t":23,"cnt":0.9590643274853801,"x":23,"y":0.9590643274853801},{"t":24,"cnt":0.9590643274853801,"x":24,"y":0.9590643274853801},{"t":25,"cnt":0.9590643274853801,"x":25,"y":0.9590643274853801},{"t":26,"cnt":0.9590643274853801,"x":26,"y":0.9590643274853801},{"t":27,"cnt":0.9649122807017544,"x":27,"y":0.9649122807017544},{"t":28,"cnt":0.9649122807017544,"x":28,"y":0.9649122807017544},{"t":29,"cnt":0.9590643274853801,"x":29,"y":0.9590643274853801},{"t":30,"cnt":0.9649122807017544,"x":30,"y":0.9649122807017544},{"t":31,"cnt":0.9649122807017544,"x":31,"y":0.9649122807017544},{"t":32,"cnt":0.9649122807017544,"x":32,"y":0.9649122807017544},{"t":33,"cnt":0.9649122807017544,"x":33,"y":0.9649122807017544},{"t":34,"cnt":0.9649122807017544,"x":34,"y":0.9649122807017544},{"t":35,"cnt":0.9649122807017544,"x":35,"y":0.9649122807017544},{"t":36,"cnt":0.9649122807017544,"x":36,"y":0.9649122807017544},{"t":37,"cnt":0.9649122807017544,"x":37,"y":0.9649122807017544},{"t":38,"cnt":0.9707602339181286,"x":38,"y":0.9707602339181286},{"t":39,"cnt":0.9649122807017544,"x":39,"y":0.9649122807017544},{"t":40,"cnt":0.9707602339181286,"x":40,"y":0.9707602339181286},{"t":41,"cnt":0.9707602339181286,"x":41,"y":0.9707602339181286},{"t":42,"cnt":0.9707602339181286,"x":42,"y":0.9707602339181286},{"t":43,"cnt":0.9707602339181286,"x":43,"y":0.9707602339181286},{"t":44,"cnt":0.9707602339181286,"x":44,"y":0.9707602339181286},{"t":45,"cnt":0.9707602339181286,"x":45,"y":0.9707602339181286},{"t":46,"cnt":0.9707602339181286,"x":46,"y":0.9707602339181286},{"t":47,"cnt":0.9707602339181286,"x":47,"y":0.9707602339181286},{"t":48,"cnt":0.9707602339181286,"x":48,"y":0.9707602339181286},{"t":49,"cnt":0.9707602339181286,"x":49,"y":0.9707602339181286},{"t":50,"cnt":0.9766081871345029,"x":50,"y":0.9766081871345029}],"type":"line"}],"xAxis":{"type":"linear","title":{"text":"Number of Trials"}},"subtitle":{"text":"Optimal number of trials is 3 (accuracy : 0.982456140350877 ) in C5.0"}},"theme":{"colors":["#0266C8","#F90101","#F2B50F","#00933B"],"chart":{"style":{"fontFamily":"Roboto","color":"#444444"}},"xAxis":{"gridLineWidth":1,"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"yAxis":{"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#C0C0C0","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadCSV":"Download CSV","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","downloadXLS":"Download XLS","drillUpText":"◁ Back to {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":" ","viewData":"View data table","viewFullscreen":"View in full screen","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":"Roboto","debug":false},"evals":[],"jsHooks":[]}</script>

##### Apply optimal trials to show best predict performance in C5.0

``` r
learn_imp_c50 <- C5.0(train[,-1],train$diagnosis,trials=opt_t$t)    
pre_imp_c50 <- predict(learn_imp_c50, test[,-1])
cm_imp_c50 <- confusionMatrix(pre_imp_c50, test$diagnosis)
cm_imp_c50
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       109         1
    ##   Malignant      2        59
    ##                                           
    ##                Accuracy : 0.9825          
    ##                  95% CI : (0.9496, 0.9964)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.9616          
    ##                                           
    ##  Mcnemar's Test P-Value : 1               
    ##                                           
    ##             Sensitivity : 0.9820          
    ##             Specificity : 0.9833          
    ##          Pos Pred Value : 0.9909          
    ##          Neg Pred Value : 0.9672          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6374          
    ##    Detection Prevalence : 0.6433          
    ##       Balanced Accuracy : 0.9827          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

#### rpart

``` r
library(rpart)
learn_rp <- rpart(diagnosis~.,data=train,control=rpart.control(minsplit=2))
pre_rp <- predict(learn_rp, test[,-1], type="class")
cm_rp  <- confusionMatrix(pre_rp, test$diagnosis)   
cm_rp
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       108         5
    ##   Malignant      3        55
    ##                                           
    ##                Accuracy : 0.9532          
    ##                  95% CI : (0.9099, 0.9796)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.8965          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.7237          
    ##                                           
    ##             Sensitivity : 0.9730          
    ##             Specificity : 0.9167          
    ##          Pos Pred Value : 0.9558          
    ##          Neg Pred Value : 0.9483          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6316          
    ##    Detection Prevalence : 0.6608          
    ##       Balanced Accuracy : 0.9448          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

#### Prune

``` r
learn_pru <- prune(learn_rp, cp=learn_rp$cptable[which.min(learn_rp$cptable[,"xerror"]),"CP"])
pre_pru <- predict(learn_pru, test[,-1], type="class")
cm_pru <-confusionMatrix(pre_pru, test$diagnosis)           
cm_pru
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       108         5
    ##   Malignant      3        55
    ##                                           
    ##                Accuracy : 0.9532          
    ##                  95% CI : (0.9099, 0.9796)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.8965          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.7237          
    ##                                           
    ##             Sensitivity : 0.9730          
    ##             Specificity : 0.9167          
    ##          Pos Pred Value : 0.9558          
    ##          Neg Pred Value : 0.9483          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6316          
    ##    Detection Prevalence : 0.6608          
    ##       Balanced Accuracy : 0.9448          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

#### OneR

``` r
library("RWeka")
learn_1r <- OneR(diagnosis~., data=train)
pre_1r <- predict(learn_1r, test[,-1])
cm_1r   <- confusionMatrix(pre_1r, test$diagnosis)
cm_1r
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       108        10
    ##   Malignant      3        50
    ##                                           
    ##                Accuracy : 0.924           
    ##                  95% CI : (0.8735, 0.9589)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : < 2e-16         
    ##                                           
    ##                   Kappa : 0.8285          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.09609         
    ##                                           
    ##             Sensitivity : 0.9730          
    ##             Specificity : 0.8333          
    ##          Pos Pred Value : 0.9153          
    ##          Neg Pred Value : 0.9434          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6316          
    ##    Detection Prevalence : 0.6901          
    ##       Balanced Accuracy : 0.9032          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

#### JRip

``` r
learn_jrip <- JRip(diagnosis ~ ., data=train)
pre_jrip <- predict(learn_jrip, test[,-1])
cm_jrip <- confusionMatrix(pre_jrip, test$diagnosis)        
cm_jrip
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       104         2
    ##   Malignant      7        58
    ##                                           
    ##                Accuracy : 0.9474          
    ##                  95% CI : (0.9024, 0.9757)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.8866          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.1824          
    ##                                           
    ##             Sensitivity : 0.9369          
    ##             Specificity : 0.9667          
    ##          Pos Pred Value : 0.9811          
    ##          Neg Pred Value : 0.8923          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6082          
    ##    Detection Prevalence : 0.6199          
    ##       Balanced Accuracy : 0.9518          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

#### naiveBayes

##### Choose ‘laplace’ which shows best predict performance in naiveBayes

It shows that “laplace” function is not effective to naiveBayes predict
performance. So, It’s okay not to use laplace option for tuning.

``` r
library(e1071)

acc_test <- numeric()
accuracy1 <- NULL; accuracy2 <- NULL

for(i in 1:30){
    learn_imp_nb <- naiveBayes(train[,-1], train$diagnosis, laplace=i)    
    p_nb <- predict(learn_imp_nb, test[,-1]) 
    accuracy1 <- confusionMatrix(p_nb, test$diagnosis)
    accuracy2[i] <- accuracy1$overall[1]
}

acc <- data.frame(l= seq(1,30), cnt = accuracy2)

opt_l <- subset(acc, cnt==max(cnt))[1,]
sub <- paste("Optimal number of laplace is", opt_l$l, "(accuracy :", opt_l$cnt,") in naiveBayes")

library(highcharter)
hchart(acc, 'line', hcaes(l, cnt)) %>%
  hc_title(text = "Accuracy With Varying Laplace (naiveBayes)") %>%
  hc_subtitle(text = sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Laplace")) %>%
  hc_yAxis(title = list(text = "Accuracy"))
```

<div class="highchart html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-b024844113d7c51f27d6" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-b024844113d7c51f27d6">{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"Accuracy With Varying Laplace (naiveBayes)"},"yAxis":{"title":{"text":"Accuracy"},"type":"linear"},"credits":{"enabled":false},"exporting":{"enabled":false},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}}},"series":[{"group":"group","data":[{"l":1,"cnt":0.9415204678362573,"x":1,"y":0.9415204678362573},{"l":2,"cnt":0.9415204678362573,"x":2,"y":0.9415204678362573},{"l":3,"cnt":0.9415204678362573,"x":3,"y":0.9415204678362573},{"l":4,"cnt":0.9415204678362573,"x":4,"y":0.9415204678362573},{"l":5,"cnt":0.9415204678362573,"x":5,"y":0.9415204678362573},{"l":6,"cnt":0.9415204678362573,"x":6,"y":0.9415204678362573},{"l":7,"cnt":0.9415204678362573,"x":7,"y":0.9415204678362573},{"l":8,"cnt":0.9415204678362573,"x":8,"y":0.9415204678362573},{"l":9,"cnt":0.9415204678362573,"x":9,"y":0.9415204678362573},{"l":10,"cnt":0.9415204678362573,"x":10,"y":0.9415204678362573},{"l":11,"cnt":0.9415204678362573,"x":11,"y":0.9415204678362573},{"l":12,"cnt":0.9415204678362573,"x":12,"y":0.9415204678362573},{"l":13,"cnt":0.9415204678362573,"x":13,"y":0.9415204678362573},{"l":14,"cnt":0.9415204678362573,"x":14,"y":0.9415204678362573},{"l":15,"cnt":0.9415204678362573,"x":15,"y":0.9415204678362573},{"l":16,"cnt":0.9415204678362573,"x":16,"y":0.9415204678362573},{"l":17,"cnt":0.9415204678362573,"x":17,"y":0.9415204678362573},{"l":18,"cnt":0.9415204678362573,"x":18,"y":0.9415204678362573},{"l":19,"cnt":0.9415204678362573,"x":19,"y":0.9415204678362573},{"l":20,"cnt":0.9415204678362573,"x":20,"y":0.9415204678362573},{"l":21,"cnt":0.9415204678362573,"x":21,"y":0.9415204678362573},{"l":22,"cnt":0.9415204678362573,"x":22,"y":0.9415204678362573},{"l":23,"cnt":0.9415204678362573,"x":23,"y":0.9415204678362573},{"l":24,"cnt":0.9415204678362573,"x":24,"y":0.9415204678362573},{"l":25,"cnt":0.9415204678362573,"x":25,"y":0.9415204678362573},{"l":26,"cnt":0.9415204678362573,"x":26,"y":0.9415204678362573},{"l":27,"cnt":0.9415204678362573,"x":27,"y":0.9415204678362573},{"l":28,"cnt":0.9415204678362573,"x":28,"y":0.9415204678362573},{"l":29,"cnt":0.9415204678362573,"x":29,"y":0.9415204678362573},{"l":30,"cnt":0.9415204678362573,"x":30,"y":0.9415204678362573}],"type":"line"}],"xAxis":{"type":"linear","title":{"text":"Number of Laplace"}},"subtitle":{"text":"Optimal number of laplace is 1 (accuracy : 0.941520467836257 ) in naiveBayes"}},"theme":{"colors":["#0266C8","#F90101","#F2B50F","#00933B"],"chart":{"style":{"fontFamily":"Roboto","color":"#444444"}},"xAxis":{"gridLineWidth":1,"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"yAxis":{"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#C0C0C0","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadCSV":"Download CSV","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","downloadXLS":"Download XLS","drillUpText":"◁ Back to {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":" ","viewData":"View data table","viewFullscreen":"View in full screen","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":"Roboto","debug":false},"evals":[],"jsHooks":[]}</script>

##### naiveBayes without laplace

``` r
learn_nb <- naiveBayes(train[,-1], train$diagnosis)
pre_nb <- predict(learn_nb, test[,-1])
cm_nb <- confusionMatrix(pre_nb, test$diagnosis)        
cm_nb
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       107         6
    ##   Malignant      4        54
    ##                                           
    ##                Accuracy : 0.9415          
    ##                  95% CI : (0.8951, 0.9716)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.8706          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.7518          
    ##                                           
    ##             Sensitivity : 0.9640          
    ##             Specificity : 0.9000          
    ##          Pos Pred Value : 0.9469          
    ##          Neg Pred Value : 0.9310          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6257          
    ##    Detection Prevalence : 0.6608          
    ##       Balanced Accuracy : 0.9320          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

#### randomForest

``` r
library(randomForest)
learn_rf <- randomForest(diagnosis~., data=train, ntree=500, proximity=T, importance=T)
pre_rf   <- predict(learn_rf, test[,-1])
cm_rf    <- confusionMatrix(pre_rf, test$diagnosis)
cm_rf
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       111         4
    ##   Malignant      0        56
    ##                                           
    ##                Accuracy : 0.9766          
    ##                  95% CI : (0.9412, 0.9936)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.9478          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.1336          
    ##                                           
    ##             Sensitivity : 1.0000          
    ##             Specificity : 0.9333          
    ##          Pos Pred Value : 0.9652          
    ##          Neg Pred Value : 1.0000          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6491          
    ##    Detection Prevalence : 0.6725          
    ##       Balanced Accuracy : 0.9667          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

``` r
plot(learn_rf, main="Random Forest (Error Rate vs. Number of Trees)")
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-64-1.png)<!-- -->

##### Prediction Plot

I can’t explain this plot exactly.

Anybody who can describe this plot, please let me know. I’m happy to add
in my kernel.

``` r
plot(margin(learn_rf,test$diagnosis))
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

##### Variance Importance Plot

- MeanDecreaseAccuracy : radius_worst \> concave.points_worst \>
  area_worst \> perimeter_worst

Important parameters for accuracy improvement are determined by the
“MeanDecreaseAccuracy”.

- MeanDecreaseGini : perimeter_worst \> radius_worst \> area_worst \>
  concave.points_worst

Important parameters for improving node impurities are determined by the
“MeanDecreaseGini”.

``` r
varImpPlot(learn_rf)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

#### ctree

``` r
library(party)
learn_ct <- ctree(diagnosis~., data=train, controls=ctree_control(maxdepth=2))
pre_ct   <- predict(learn_ct, test[,-1])
cm_ct    <- confusionMatrix(pre_ct, test$diagnosis)
cm_ct
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       109         4
    ##   Malignant      2        56
    ##                                          
    ##                Accuracy : 0.9649         
    ##                  95% CI : (0.9252, 0.987)
    ##     No Information Rate : 0.6491         
    ##     P-Value [Acc > NIR] : <2e-16         
    ##                                          
    ##                   Kappa : 0.9224         
    ##                                          
    ##  Mcnemar's Test P-Value : 0.6831         
    ##                                          
    ##             Sensitivity : 0.9820         
    ##             Specificity : 0.9333         
    ##          Pos Pred Value : 0.9646         
    ##          Neg Pred Value : 0.9655         
    ##              Prevalence : 0.6491         
    ##          Detection Rate : 0.6374         
    ##    Detection Prevalence : 0.6608         
    ##       Balanced Accuracy : 0.9577         
    ##                                          
    ##        'Positive' Class : Benign         
    ## 

#### KNN - Tune

##### Choose ‘k’ which shows best predict performance in KNN

``` r
library(class)

acc_test <- numeric() 

for(i in 1:30){
    predict <- knn(train=train[,-1], test=test[,-1], cl=train[,1], k=i, prob=T)
    acc_test <- c(acc_test,mean(predict==test[,1]))
}

acc <- data.frame(k= seq(1,30), cnt = acc_test)

opt_k <- subset(acc, cnt==max(cnt))[1,]
sub <- paste("Optimal number of k is", opt_k$k, "(accuracy :", opt_k$cnt,") in KNN")

library(highcharter)
hchart(acc, 'line', hcaes(k, cnt)) %>%
  hc_title(text = "Accuracy With Varying K (KNN)") %>%
  hc_subtitle(text = sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Neighbors(k)")) %>%
  hc_yAxis(title = list(text = "Accuracy"))
```

<div class="highchart html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-8f46c5b1a61ce5dae396" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-8f46c5b1a61ce5dae396">{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"Accuracy With Varying K (KNN)"},"yAxis":{"title":{"text":"Accuracy"},"type":"linear"},"credits":{"enabled":false},"exporting":{"enabled":false},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}}},"series":[{"group":"group","data":[{"k":1,"cnt":0.935672514619883,"x":1,"y":0.935672514619883},{"k":2,"cnt":0.9415204678362573,"x":2,"y":0.9415204678362573},{"k":3,"cnt":0.9298245614035088,"x":3,"y":0.9298245614035088},{"k":4,"cnt":0.9298245614035088,"x":4,"y":0.9298245614035088},{"k":5,"cnt":0.9298245614035088,"x":5,"y":0.9298245614035088},{"k":6,"cnt":0.9298245614035088,"x":6,"y":0.9298245614035088},{"k":7,"cnt":0.9181286549707602,"x":7,"y":0.9181286549707602},{"k":8,"cnt":0.9181286549707602,"x":8,"y":0.9181286549707602},{"k":9,"cnt":0.9181286549707602,"x":9,"y":0.9181286549707602},{"k":10,"cnt":0.9181286549707602,"x":10,"y":0.9181286549707602},{"k":11,"cnt":0.9181286549707602,"x":11,"y":0.9181286549707602},{"k":12,"cnt":0.9181286549707602,"x":12,"y":0.9181286549707602},{"k":13,"cnt":0.9181286549707602,"x":13,"y":0.9181286549707602},{"k":14,"cnt":0.9181286549707602,"x":14,"y":0.9181286549707602},{"k":15,"cnt":0.9181286549707602,"x":15,"y":0.9181286549707602},{"k":16,"cnt":0.9181286549707602,"x":16,"y":0.9181286549707602},{"k":17,"cnt":0.9181286549707602,"x":17,"y":0.9181286549707602},{"k":18,"cnt":0.9181286549707602,"x":18,"y":0.9181286549707602},{"k":19,"cnt":0.9122807017543859,"x":19,"y":0.9122807017543859},{"k":20,"cnt":0.9122807017543859,"x":20,"y":0.9122807017543859},{"k":21,"cnt":0.9181286549707602,"x":21,"y":0.9181286549707602},{"k":22,"cnt":0.9181286549707602,"x":22,"y":0.9181286549707602},{"k":23,"cnt":0.9064327485380117,"x":23,"y":0.9064327485380117},{"k":24,"cnt":0.9122807017543859,"x":24,"y":0.9122807017543859},{"k":25,"cnt":0.9122807017543859,"x":25,"y":0.9122807017543859},{"k":26,"cnt":0.9181286549707602,"x":26,"y":0.9181286549707602},{"k":27,"cnt":0.9122807017543859,"x":27,"y":0.9122807017543859},{"k":28,"cnt":0.9064327485380117,"x":28,"y":0.9064327485380117},{"k":29,"cnt":0.9064327485380117,"x":29,"y":0.9064327485380117},{"k":30,"cnt":0.9064327485380117,"x":30,"y":0.9064327485380117}],"type":"line"}],"xAxis":{"type":"linear","title":{"text":"Number of Neighbors(k)"}},"subtitle":{"text":"Optimal number of k is 2 (accuracy : 0.941520467836257 ) in KNN"}},"theme":{"colors":["#0266C8","#F90101","#F2B50F","#00933B"],"chart":{"style":{"fontFamily":"Roboto","color":"#444444"}},"xAxis":{"gridLineWidth":1,"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"yAxis":{"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#C0C0C0","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadCSV":"Download CSV","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","downloadXLS":"Download XLS","drillUpText":"◁ Back to {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":" ","viewData":"View data table","viewFullscreen":"View in full screen","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":"Roboto","debug":false},"evals":[],"jsHooks":[]}</script>

##### Apply optimal K to show best predict performance in KNN

``` r
pre_knn <- knn(train = train[,-1], test = test[,-1], cl = train[,1], k=opt_k$k, prob=T)
cm_knn  <- confusionMatrix(pre_knn, test$diagnosis)
cm_knn
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       107         7
    ##   Malignant      4        53
    ##                                           
    ##                Accuracy : 0.9357          
    ##                  95% CI : (0.8878, 0.9675)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.8571          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.5465          
    ##                                           
    ##             Sensitivity : 0.9640          
    ##             Specificity : 0.8833          
    ##          Pos Pred Value : 0.9386          
    ##          Neg Pred Value : 0.9298          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6257          
    ##    Detection Prevalence : 0.6667          
    ##       Balanced Accuracy : 0.9236          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

#### K-Means

##### Make KMEANS predict function

we have to make function to predict using kmeans methods, since orgin
predict function don’t support kmeans.

``` r
predict.kmeans <- function(newdata, object){
    centers <- object$centers
    n_centers <- nrow(centers)
    dist_mat <- as.matrix(dist(rbind(centers, newdata)))
    dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
    max.col(-dist_mat)
}
```

##### apply kmeans

you have to apply centers to 2, since there are only two factors(benign,
malignant)

``` r
library(caret)
learn_kmeans <- kmeans(train[,-1], centers=2)

pre_kmeans <- predict.kmeans(test[,-1],learn_kmeans)
pre_kmeans <- as.factor(ifelse(pre_kmeans == 1,"Benign","Malignant"))
cm_kmeans <- confusionMatrix(pre_kmeans, test$diagnosis)
cm_kmeans
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign         0        35
    ##   Malignant    111        25
    ##                                           
    ##                Accuracy : 0.1462          
    ##                  95% CI : (0.0969, 0.2082)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : -0.4518         
    ##                                           
    ##  Mcnemar's Test P-Value : 5.399e-10       
    ##                                           
    ##             Sensitivity : 0.0000          
    ##             Specificity : 0.4167          
    ##          Pos Pred Value : 0.0000          
    ##          Neg Pred Value : 0.1838          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.0000          
    ##    Detection Prevalence : 0.2047          
    ##       Balanced Accuracy : 0.2083          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

##### plot

``` r
library(factoextra)
learn_kmeans$cluster <- ifelse(learn_kmeans$cluster == 1,"Benign","Malignant")
fviz_cluster(learn_kmeans, data = train[,-1])
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-72-1.png)<!-- -->

#### GBM

``` r
library(gbm)
test_gbm <- gbm(diagnosis~., data=train, distribution="gaussian",n.trees = 10000,
                shrinkage = 0.01, interaction.depth = 4, bag.fraction=0.5, train.fraction=0.5,n.minobsinnode=10,cv.folds=3,keep.data=TRUE,verbose=FALSE,n.cores=1)
```

    ## CV: 1 
    ## CV: 2 
    ## CV: 3

``` r
best.iter <- gbm.perf(test_gbm, method="cv",plot.it=FALSE)
fitControl = trainControl(method="cv", number=5, returnResamp="all")
learn_gbm = train(diagnosis~., data=train, method="gbm", distribution="bernoulli", trControl=fitControl, verbose=F, tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))
pre_gbm <- predict(learn_gbm, test[,-1])
cm_gbm <- confusionMatrix(pre_gbm, test$diagnosis)
cm_gbm
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       109         4
    ##   Malignant      2        56
    ##                                          
    ##                Accuracy : 0.9649         
    ##                  95% CI : (0.9252, 0.987)
    ##     No Information Rate : 0.6491         
    ##     P-Value [Acc > NIR] : <2e-16         
    ##                                          
    ##                   Kappa : 0.9224         
    ##                                          
    ##  Mcnemar's Test P-Value : 0.6831         
    ##                                          
    ##             Sensitivity : 0.9820         
    ##             Specificity : 0.9333         
    ##          Pos Pred Value : 0.9646         
    ##          Neg Pred Value : 0.9655         
    ##              Prevalence : 0.6491         
    ##          Detection Rate : 0.6374         
    ##    Detection Prevalence : 0.6608         
    ##       Balanced Accuracy : 0.9577         
    ##                                          
    ##        'Positive' Class : Benign         
    ## 

#### adaBoost

``` r
library(rpart)
library(ada)
control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
learn_ada <- ada(diagnosis~., data = train, test.x = train[,-1], test.y = train[,1], type = "gentle", control = control, iter = 70)
pre_ada <- predict(learn_ada, test[,-1])
cm_ada <- confusionMatrix(pre_ada, test$diagnosis)
cm_ada
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       111         2
    ##   Malignant      0        58
    ##                                           
    ##                Accuracy : 0.9883          
    ##                  95% CI : (0.9584, 0.9986)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.9741          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.4795          
    ##                                           
    ##             Sensitivity : 1.0000          
    ##             Specificity : 0.9667          
    ##          Pos Pred Value : 0.9823          
    ##          Neg Pred Value : 1.0000          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6491          
    ##    Detection Prevalence : 0.6608          
    ##       Balanced Accuracy : 0.9833          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

#### SVM

``` r
learn_svm <- svm(diagnosis~., data=train)
pre_svm <- predict(learn_svm, test[,-1])
cm_svm <- confusionMatrix(pre_svm, test$diagnosis)
cm_svm
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       109         1
    ##   Malignant      2        59
    ##                                           
    ##                Accuracy : 0.9825          
    ##                  95% CI : (0.9496, 0.9964)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.9616          
    ##                                           
    ##  Mcnemar's Test P-Value : 1               
    ##                                           
    ##             Sensitivity : 0.9820          
    ##             Specificity : 0.9833          
    ##          Pos Pred Value : 0.9909          
    ##          Neg Pred Value : 0.9672          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6374          
    ##    Detection Prevalence : 0.6433          
    ##       Balanced Accuracy : 0.9827          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

#### SVM - Tune

##### Choose ‘gamma, cost’ which shows best predict performance in SVM

``` r
gamma <- seq(0,0.1,0.005)
cost <- 2^(0:5)
parms <- expand.grid(cost=cost, gamma=gamma)    ## 231

acc_test <- numeric()
accuracy1 <- NULL; accuracy2 <- NULL

for(i in 1:NROW(parms)){        
        learn_svm <- svm(diagnosis~., data=train, gamma=parms$gamma[i], cost=parms$cost[i])
        pre_svm <- predict(learn_svm, test[,-1])
        accuracy1 <- confusionMatrix(pre_svm, test$diagnosis)
        accuracy2[i] <- accuracy1$overall[1]
}

acc <- data.frame(p= seq(1,NROW(parms)), cnt = accuracy2)

opt_p <- subset(acc, cnt==max(cnt))[1,]
sub <- paste("Optimal number of parameter is", opt_p$p, "(accuracy :", opt_p$cnt,") in SVM")

library(highcharter)
hchart(acc, 'line', hcaes(p, cnt)) %>%
  hc_title(text = "Accuracy With Varying Parameters (SVM)") %>%
  hc_subtitle(text = sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Parameters")) %>%
  hc_yAxis(title = list(text = "Accuracy"))
```

<div class="highchart html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-5f883f0c6c1bb2dfc5a4" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-5f883f0c6c1bb2dfc5a4">{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"Accuracy With Varying Parameters (SVM)"},"yAxis":{"title":{"text":"Accuracy"},"type":"linear"},"credits":{"enabled":false},"exporting":{"enabled":false},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}}},"series":[{"group":"group","data":[{"p":1,"cnt":0.6491228070175439,"x":1,"y":0.6491228070175439},{"p":2,"cnt":0.6491228070175439,"x":2,"y":0.6491228070175439},{"p":3,"cnt":0.6491228070175439,"x":3,"y":0.6491228070175439},{"p":4,"cnt":0.6491228070175439,"x":4,"y":0.6491228070175439},{"p":5,"cnt":0.6491228070175439,"x":5,"y":0.6491228070175439},{"p":6,"cnt":0.6491228070175439,"x":6,"y":0.6491228070175439},{"p":7,"cnt":0.9824561403508771,"x":7,"y":0.9824561403508771},{"p":8,"cnt":0.9766081871345029,"x":8,"y":0.9766081871345029},{"p":9,"cnt":0.9824561403508771,"x":9,"y":0.9824561403508771},{"p":10,"cnt":0.9883040935672515,"x":10,"y":0.9883040935672515},{"p":11,"cnt":0.9824561403508771,"x":11,"y":0.9824561403508771},{"p":12,"cnt":0.9824561403508771,"x":12,"y":0.9824561403508771},{"p":13,"cnt":0.9766081871345029,"x":13,"y":0.9766081871345029},{"p":14,"cnt":0.9824561403508771,"x":14,"y":0.9824561403508771},{"p":15,"cnt":0.9883040935672515,"x":15,"y":0.9883040935672515},{"p":16,"cnt":0.9883040935672515,"x":16,"y":0.9883040935672515},{"p":17,"cnt":0.9824561403508771,"x":17,"y":0.9824561403508771},{"p":18,"cnt":0.9883040935672515,"x":18,"y":0.9883040935672515},{"p":19,"cnt":0.9883040935672515,"x":19,"y":0.9883040935672515},{"p":20,"cnt":0.9883040935672515,"x":20,"y":0.9883040935672515},{"p":21,"cnt":0.9883040935672515,"x":21,"y":0.9883040935672515},{"p":22,"cnt":0.9883040935672515,"x":22,"y":0.9883040935672515},{"p":23,"cnt":0.9883040935672515,"x":23,"y":0.9883040935672515},{"p":24,"cnt":0.9824561403508771,"x":24,"y":0.9824561403508771},{"p":25,"cnt":0.9883040935672515,"x":25,"y":0.9883040935672515},{"p":26,"cnt":0.9883040935672515,"x":26,"y":0.9883040935672515},{"p":27,"cnt":0.9824561403508771,"x":27,"y":0.9824561403508771},{"p":28,"cnt":0.9883040935672515,"x":28,"y":0.9883040935672515},{"p":29,"cnt":0.9824561403508771,"x":29,"y":0.9824561403508771},{"p":30,"cnt":0.9883040935672515,"x":30,"y":0.9883040935672515},{"p":31,"cnt":0.9824561403508771,"x":31,"y":0.9824561403508771},{"p":32,"cnt":0.9824561403508771,"x":32,"y":0.9824561403508771},{"p":33,"cnt":0.9824561403508771,"x":33,"y":0.9824561403508771},{"p":34,"cnt":0.9883040935672515,"x":34,"y":0.9883040935672515},{"p":35,"cnt":0.9824561403508771,"x":35,"y":0.9824561403508771},{"p":36,"cnt":0.9824561403508771,"x":36,"y":0.9824561403508771},{"p":37,"cnt":0.9766081871345029,"x":37,"y":0.9766081871345029},{"p":38,"cnt":0.9824561403508771,"x":38,"y":0.9824561403508771},{"p":39,"cnt":0.9824561403508771,"x":39,"y":0.9824561403508771},{"p":40,"cnt":0.9883040935672515,"x":40,"y":0.9883040935672515},{"p":41,"cnt":0.9883040935672515,"x":41,"y":0.9883040935672515},{"p":42,"cnt":0.9766081871345029,"x":42,"y":0.9766081871345029},{"p":43,"cnt":0.9824561403508771,"x":43,"y":0.9824561403508771},{"p":44,"cnt":0.9824561403508771,"x":44,"y":0.9824561403508771},{"p":45,"cnt":0.9883040935672515,"x":45,"y":0.9883040935672515},{"p":46,"cnt":0.9883040935672515,"x":46,"y":0.9883040935672515},{"p":47,"cnt":0.9883040935672515,"x":47,"y":0.9883040935672515},{"p":48,"cnt":0.9766081871345029,"x":48,"y":0.9766081871345029},{"p":49,"cnt":0.9824561403508771,"x":49,"y":0.9824561403508771},{"p":50,"cnt":0.9824561403508771,"x":50,"y":0.9824561403508771},{"p":51,"cnt":0.9883040935672515,"x":51,"y":0.9883040935672515},{"p":52,"cnt":0.9883040935672515,"x":52,"y":0.9883040935672515},{"p":53,"cnt":0.9883040935672515,"x":53,"y":0.9883040935672515},{"p":54,"cnt":0.9766081871345029,"x":54,"y":0.9766081871345029},{"p":55,"cnt":0.9824561403508771,"x":55,"y":0.9824561403508771},{"p":56,"cnt":0.9824561403508771,"x":56,"y":0.9824561403508771},{"p":57,"cnt":0.9883040935672515,"x":57,"y":0.9883040935672515},{"p":58,"cnt":0.9883040935672515,"x":58,"y":0.9883040935672515},{"p":59,"cnt":0.9883040935672515,"x":59,"y":0.9883040935672515},{"p":60,"cnt":0.9707602339181286,"x":60,"y":0.9707602339181286},{"p":61,"cnt":0.9824561403508771,"x":61,"y":0.9824561403508771},{"p":62,"cnt":0.9824561403508771,"x":62,"y":0.9824561403508771},{"p":63,"cnt":0.9883040935672515,"x":63,"y":0.9883040935672515},{"p":64,"cnt":0.9883040935672515,"x":64,"y":0.9883040935672515},{"p":65,"cnt":0.9883040935672515,"x":65,"y":0.9883040935672515},{"p":66,"cnt":0.9707602339181286,"x":66,"y":0.9707602339181286},{"p":67,"cnt":0.9824561403508771,"x":67,"y":0.9824561403508771},{"p":68,"cnt":0.9824561403508771,"x":68,"y":0.9824561403508771},{"p":69,"cnt":0.9883040935672515,"x":69,"y":0.9883040935672515},{"p":70,"cnt":0.9883040935672515,"x":70,"y":0.9883040935672515},{"p":71,"cnt":0.9824561403508771,"x":71,"y":0.9824561403508771},{"p":72,"cnt":0.9707602339181286,"x":72,"y":0.9707602339181286},{"p":73,"cnt":0.9824561403508771,"x":73,"y":0.9824561403508771},{"p":74,"cnt":0.9883040935672515,"x":74,"y":0.9883040935672515},{"p":75,"cnt":0.9883040935672515,"x":75,"y":0.9883040935672515},{"p":76,"cnt":0.9883040935672515,"x":76,"y":0.9883040935672515},{"p":77,"cnt":0.9766081871345029,"x":77,"y":0.9766081871345029},{"p":78,"cnt":0.9707602339181286,"x":78,"y":0.9707602339181286},{"p":79,"cnt":0.9824561403508771,"x":79,"y":0.9824561403508771},{"p":80,"cnt":0.9883040935672515,"x":80,"y":0.9883040935672515},{"p":81,"cnt":0.9824561403508771,"x":81,"y":0.9824561403508771},{"p":82,"cnt":0.9883040935672515,"x":82,"y":0.9883040935672515},{"p":83,"cnt":0.9766081871345029,"x":83,"y":0.9766081871345029},{"p":84,"cnt":0.9707602339181286,"x":84,"y":0.9707602339181286},{"p":85,"cnt":0.9824561403508771,"x":85,"y":0.9824561403508771},{"p":86,"cnt":0.9883040935672515,"x":86,"y":0.9883040935672515},{"p":87,"cnt":0.9824561403508771,"x":87,"y":0.9824561403508771},{"p":88,"cnt":0.9883040935672515,"x":88,"y":0.9883040935672515},{"p":89,"cnt":0.9766081871345029,"x":89,"y":0.9766081871345029},{"p":90,"cnt":0.9707602339181286,"x":90,"y":0.9707602339181286},{"p":91,"cnt":0.9883040935672515,"x":91,"y":0.9883040935672515},{"p":92,"cnt":0.9883040935672515,"x":92,"y":0.9883040935672515},{"p":93,"cnt":0.9824561403508771,"x":93,"y":0.9824561403508771},{"p":94,"cnt":0.9883040935672515,"x":94,"y":0.9883040935672515},{"p":95,"cnt":0.9766081871345029,"x":95,"y":0.9766081871345029},{"p":96,"cnt":0.9766081871345029,"x":96,"y":0.9766081871345029},{"p":97,"cnt":0.9883040935672515,"x":97,"y":0.9883040935672515},{"p":98,"cnt":0.9883040935672515,"x":98,"y":0.9883040935672515},{"p":99,"cnt":0.9824561403508771,"x":99,"y":0.9824561403508771},{"p":100,"cnt":0.9824561403508771,"x":100,"y":0.9824561403508771},{"p":101,"cnt":0.9824561403508771,"x":101,"y":0.9824561403508771},{"p":102,"cnt":0.9766081871345029,"x":102,"y":0.9766081871345029},{"p":103,"cnt":0.9824561403508771,"x":103,"y":0.9824561403508771},{"p":104,"cnt":0.9824561403508771,"x":104,"y":0.9824561403508771},{"p":105,"cnt":0.9824561403508771,"x":105,"y":0.9824561403508771},{"p":106,"cnt":0.9824561403508771,"x":106,"y":0.9824561403508771},{"p":107,"cnt":0.9824561403508771,"x":107,"y":0.9824561403508771},{"p":108,"cnt":0.9766081871345029,"x":108,"y":0.9766081871345029},{"p":109,"cnt":0.9824561403508771,"x":109,"y":0.9824561403508771},{"p":110,"cnt":0.9824561403508771,"x":110,"y":0.9824561403508771},{"p":111,"cnt":0.9824561403508771,"x":111,"y":0.9824561403508771},{"p":112,"cnt":0.9824561403508771,"x":112,"y":0.9824561403508771},{"p":113,"cnt":0.9824561403508771,"x":113,"y":0.9824561403508771},{"p":114,"cnt":0.9824561403508771,"x":114,"y":0.9824561403508771},{"p":115,"cnt":0.9824561403508771,"x":115,"y":0.9824561403508771},{"p":116,"cnt":0.9824561403508771,"x":116,"y":0.9824561403508771},{"p":117,"cnt":0.9824561403508771,"x":117,"y":0.9824561403508771},{"p":118,"cnt":0.9824561403508771,"x":118,"y":0.9824561403508771},{"p":119,"cnt":0.9824561403508771,"x":119,"y":0.9824561403508771},{"p":120,"cnt":0.9824561403508771,"x":120,"y":0.9824561403508771},{"p":121,"cnt":0.9824561403508771,"x":121,"y":0.9824561403508771},{"p":122,"cnt":0.9824561403508771,"x":122,"y":0.9824561403508771},{"p":123,"cnt":0.9824561403508771,"x":123,"y":0.9824561403508771},{"p":124,"cnt":0.9824561403508771,"x":124,"y":0.9824561403508771},{"p":125,"cnt":0.9766081871345029,"x":125,"y":0.9766081871345029},{"p":126,"cnt":0.9766081871345029,"x":126,"y":0.9766081871345029}],"type":"line"}],"xAxis":{"type":"linear","title":{"text":"Number of Parameters"}},"subtitle":{"text":"Optimal number of parameter is 10 (accuracy : 0.988304093567251 ) in SVM"}},"theme":{"colors":["#0266C8","#F90101","#F2B50F","#00933B"],"chart":{"style":{"fontFamily":"Roboto","color":"#444444"}},"xAxis":{"gridLineWidth":1,"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"yAxis":{"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#C0C0C0","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadCSV":"Download CSV","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","downloadXLS":"Download XLS","drillUpText":"◁ Back to {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":" ","viewData":"View data table","viewFullscreen":"View in full screen","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":"Roboto","debug":false},"evals":[],"jsHooks":[]}</script>

##### Apply optimal parameters(gamma, cost) to show best predict performance in SVM

``` r
learn_imp_svm <- svm(diagnosis~., data=train, cost=parms$cost[opt_p$p], gamma=parms$gamma[opt_p$p])
pre_imp_svm <- predict(learn_imp_svm, test[,-1])
cm_imp_svm <- confusionMatrix(pre_imp_svm, test$diagnosis)
cm_imp_svm
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign       110         1
    ##   Malignant      1        59
    ##                                           
    ##                Accuracy : 0.9883          
    ##                  95% CI : (0.9584, 0.9986)
    ##     No Information Rate : 0.6491          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.9743          
    ##                                           
    ##  Mcnemar's Test P-Value : 1               
    ##                                           
    ##             Sensitivity : 0.9910          
    ##             Specificity : 0.9833          
    ##          Pos Pred Value : 0.9910          
    ##          Neg Pred Value : 0.9833          
    ##              Prevalence : 0.6491          
    ##          Detection Rate : 0.6433          
    ##    Detection Prevalence : 0.6491          
    ##       Balanced Accuracy : 0.9872          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

### 5-4) Visualize to compare the accuracy of all methods

``` r
col <- c("#ed3b3b", "#0099ff")
par(mfrow=c(3,5))
fourfoldplot(cm_c50$table, color = col, conf.level = 0, margin = 1, main=paste("C5.0 (",round(cm_c50$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_imp_c50$table, color = col, conf.level = 0, margin = 1, main=paste("Tune C5.0 (",round(cm_imp_c50$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_rp$table, color = col, conf.level = 0, margin = 1, main=paste("RPart (",round(cm_rp$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_pru$table, color = col, conf.level = 0, margin = 1, main=paste("Prune (",round(cm_pru$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_1r$table, color = col, conf.level = 0, margin = 1, main=paste("OneR (",round(cm_1r$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_jrip$table, color = col, conf.level = 0, margin = 1, main=paste("JRip (",round(cm_jrip$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_ct$table, color = col, conf.level = 0, margin = 1, main=paste("CTree (",round(cm_ct$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_nb$table, color = col, conf.level = 0, margin = 1, main=paste("NaiveBayes (",round(cm_nb$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_knn$table, color = col, conf.level = 0, margin = 1, main=paste("Tune KNN (",round(cm_knn$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_kmeans$table, color = col, conf.level = 0, margin = 1, main=paste("KMeans (",round(cm_kmeans$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_rf$table, color = col, conf.level = 0, margin = 1, main=paste("RandomForest (",round(cm_rf$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_gbm$table, color = col, conf.level = 0, margin = 1, main=paste("GBM (",round(cm_gbm$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_ada$table, color = col, conf.level = 0, margin = 1, main=paste("AdaBoost (",round(cm_ada$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_svm$table, color = col, conf.level = 0, margin = 1, main=paste("SVM (",round(cm_svm$overall[1]*100),"%)",sep=""))
fourfoldplot(cm_imp_svm$table, color = col, conf.level = 0, margin = 1, main=paste("Tune SVM (",round(cm_imp_svm$overall[1]*100),"%)",sep=""))
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-78-1.png)<!-- -->

### 5-5) Select a best prediction model according to high accuracy

``` r
opt_predict <- c(cm_c50$overall[1], cm_imp_c50$overall[1], cm_rp$overall[1], cm_pru$overall[1], cm_1r$overall[1], cm_jrip$overall[1], cm_ct$overall[1], cm_nb$overall[1], cm_knn$overall[1], cm_kmeans$overall[1], cm_rf$overall[1], cm_gbm$overall[1], cm_ada$overall[1], cm_svm$overall[1], cm_imp_svm$overall[1])
names(opt_predict) <- c("c50","imp_c50","rpart","prune","1r","jrip","ctree","nb","knn","kmeans","rf","gbm","ada","svm","imp_svm")
best_predict_model <- subset(opt_predict, opt_predict==max(opt_predict))
best_predict_model
```

    ##       ada   imp_svm 
    ## 0.9883041 0.9883041

------------------------------------------------------------------------

## 6. Prepare Patient data for testing function

If you want to make your own new data, make sure your data format is
same as below.

### 6-1) Import patient data

``` r
patient <- read.csv("./data/breast_cancer.csv", header=T, stringsAsFactors=F)
patient$X <- NULL
```

#### Malignant patient

``` r
M <- patient[19,]               ## 19th patient
M[,c(1,2)]                      ## Malignant
```

    ##        id diagnosis
    ## 19 849014         M

#### Benign patient

``` r
B <- patient[20,]               ## 20th patient          
B[,c(1,2)]                      ## Benign
```

    ##         id diagnosis
    ## 20 8510426         B

### 6-2) Delete diagnosis column for testing

``` r
M$diagnosis <- NULL
B$diagnosis <- NULL
```

------------------------------------------------------------------------

## 7. 환자의 암 진단 예측 Function

### 7-1) 환자 진단 Function

가장 좋은 예측 모델로 평가되었으므로 ’Improve SVM Algorithm’을 기본으로
사용하세요.

``` r
# for print output
cancer_diagnosis_predict_p <- function(new, method=learn_imp_svm) {
    new_pre <- predict(method, new[,-1])
    new_res <- as.character(new_pre)
    return(paste("Patient ID: ",new[,1],"  =>  Result: ", new_res, sep=""))
}

# for submission output
cancer_diagnosis_predict_s <- function(new, method=learn_imp_svm) {
    new_pre <- predict(method, new[,-1])
    new_res <- as.character(new_pre)
    return(new_res)
}
```

### 7-2) Testing Function (use only 1 test data)

#### Benign test data

- default = improve svm

``` r
cancer_diagnosis_predict_p(B)           
```

    ## [1] "Patient ID: 8510426  =>  Result: Benign"

- Use other ML methods

``` r
cancer_diagnosis_predict_p(B,learn_imp_c50)
```

    ## [1] "Patient ID: 8510426  =>  Result: Benign"

#### Malignant test data

- default = improve svm

``` r
cancer_diagnosis_predict_p(M)
```

    ## [1] "Patient ID: 849014  =>  Result: Malignant"

- Use other ML methods

``` r
cancer_diagnosis_predict_p(M,learn_imp_c50) 
```

    ## [1] "Patient ID: 849014  =>  Result: Malignant"

### 7-3) Make Submission Output (use test dataset)

``` r
library(knitr)

t <- patient[-index,]
orgin <- t$diagnosis
t$diagnosis <- NULL
r <- cancer_diagnosis_predict_s(t)

sub <- data.frame(id=t$id, predict_diagnosis=ifelse(r=='Malignant','M','B'), orgin_diagnosis=orgin)
sub$correct <- ifelse(sub$predict_diagnosis == sub$orgin_diagnosis, "True", "False")
kable(head(sub,10))
```

|       id | predict_diagnosis | orgin_diagnosis | correct |
|---------:|:------------------|:----------------|:--------|
| 84358402 | M                 | M               | True    |
|   843786 | M                 | M               | True    |
|   844359 | M                 | M               | True    |
| 84501001 | M                 | M               | True    |
| 84799002 | M                 | M               | True    |
|   849014 | M                 | M               | True    |
|  8510426 | B                 | B               | True    |
|  8510824 | B                 | B               | True    |
|   851509 | M                 | M               | True    |
|   852631 | M                 | M               | True    |

``` r
write.csv(sub[,c(1,2)], file='submission.csv', row.names=F)
```

------------------------------------------------------------------------

## 8. Visualize (Probabilty Density Function Graph)

I made this plot for doctors who diagnosis cancer for patients. From the
patient’s point of view, I visualized the diagnostic results in
probability density graph with **patients diagnosis strong line**
included, so that they can check their status at once.

If patient’s factor of cancer is above malignants’ factor average, I
colored it with red line. (Except ’\_worst’ variance)

### 8-1) Create Visualize Function

``` r
cancer_summary <- function(new,data) {

## [a] Reshape the new dataset for ggplot
library(reshape2)
m_train <- melt(data, id="diagnosis")
m_new <- melt(new[,-1])


## [b] Variable To Highlight the key factors (geom_vline-RED)
key_factors <- c("radius_mean","perimeter_mean","area_mean","perimeter_worst",
                 "texture_worst","radius_worst","symmetry_se","compactness_worst",
                 "concavity_worst","dimension_worst")

key_col <- ifelse(m_new$variable %in% key_factors,"red","black")


## [c] Save mean of Malignant value & colors
library(dplyr)
mal_mean <- subset(data, diagnosis=="Malignant", select=-1)
mal_mean <- apply(mal_mean,2,mean)

library(stringr)
mal_col <- ifelse((round(m_new$value,3) > mal_mean) & (str_count(m_new$variable, 'worst') < 1), "red", "black")



## [d] Save titles : Main title, Patient Diagnosis

title <- "Breast Cancer Diagnosis Plot"
subtitle <- cancer_diagnosis_predict_p(new)



## ★[e] View plot highlighting your manual key factor
library(ggplot2)

res_key <- ggplot(m_train, aes(x=value,color=diagnosis, fill=diagnosis))+
    geom_histogram(aes(y=..density..), alpha=0.5, position="identity", bins=50)+
    geom_density(alpha=.2)+
    scale_color_manual(values=c("#15c3c9","#f87b72"))+
    scale_fill_manual(values=c("#61d4d6","#f5a7a1"))+
    geom_vline(data=m_new, aes(xintercept=value), 
               color=key_col, size=1.5)+
    geom_label(data=m_new, aes(x=Inf, y=Inf, label=round(value,3)), nudge_y=2,  
               vjust = "top", hjust = "right", fill="white", color="black")+
    labs(title=paste(title,"(highlight Key Factors)"), subtitle=subtitle)+
    theme(plot.title = element_text(face='bold', colour='black', hjust=0.5, size=15))+
    theme(plot.subtitle=element_text(lineheight=0.8, hjust=0.5))+
    labs(caption="[Training 569 wisc cancer diagnostic patient data]")+
    facet_wrap(~variable, scales="free", ncol=5)



## ★[f] View plots highlighting values above average of malignant patient
res_mean <- ggplot(m_train, aes(x=value,color=diagnosis, fill=diagnosis))+
    geom_histogram(aes(y=..density..), alpha=0.5, position="identity", bins=50)+
    geom_density(alpha=.2)+
    scale_color_manual(values=c("#15c3c9","#f87b72"))+
    scale_fill_manual(values=c("#61d4d6","#f5a7a1"))+
    geom_vline(data=m_new, aes(xintercept=value), 
               color=mal_col, size=1.5)+
    geom_label(data=m_new, aes(x=Inf, y=Inf, label=round(value,3)), nudge_y=2,  
               vjust = "top", hjust = "right", fill="white", color="black")+
    labs(title=paste(title,"(highlight Above malignant average)"), subtitle=subtitle)+
    theme(plot.title = element_text(face='bold', colour='black', hjust=0.5, size=15))+
    theme(plot.subtitle=element_text(lineheight=0.8, hjust=0.5, size=12))+
    labs(caption="[Training 569 wisc cancer diagnostic patient data]")+
    facet_wrap(~variable, scales="free", ncol=5)



## [g] output graph
res_mean
#res_key

}
```

### 8-2) Testing Function

#### Benign

``` r
cancer_summary(B, breast_cancer_data)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-90-1.png)<!-- -->

#### Malignant

``` r
cancer_summary(M, breast_cancer_data)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-91-1.png)<!-- -->

------------------------------------------------------------------------

## 9. Visualize (Radar)

This is radar plot to show patient’s status of each factor of cancer.

The grey colored area shows **Benigns’ Average Area**.

### 9-1) Create Visualize Function

``` r
cancer_radar <- function(new,data) {

## [a] Radar Function
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
        theta <- match.arg(theta, c("x", "y"))
        r <- ifelse(theta == "x", "y", "x")
        ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
                direction = sign(direction),
                is_linear = function(coord) TRUE)
}


## [b] Normalize Function -> you can use rescale instead.
normalize <- function(x) {
    return((x-min(x))/(max(x)-min(x)))
}


## [c] Get average from Normal(Benign) Data to set standards (Grey area)
b1 <- subset(data, diagnosis=="Benign", select=-1)
b2 <- as.data.frame(lapply(b1,normalize))           
be <- colMeans(b2)


## [d] Normalize Patient Data to compare with normal dataset
p_new <- (new[,-1]-apply(b1,2,min))/(apply(b1,2,max)-apply(b1,2,min))
max_value <- max(p_new)


## [e] Combine Two data (Normal, Patient)
cc_radar <- rbind(be,p_new)
cc_radar <- cbind(group=c("Normal","Patient"),cc_radar)

coc <- melt(cc_radar, id="group")
library(stringr)
coc$variable <- as.character(coc$variable)
coc$variable[str_count(coc$variable,'\\_')>1] <- sub('_', '.', coc$variable[str_count(coc$variable,'\\_')>1])
name <- unlist(strsplit(as.character(coc$variable),"_"))

coc$feature <- name[c(seq(1,length(name),2))]
coc$type <- name[c(seq(2,length(name),2))]  
coc$variable <- NULL

df <- coc[order(coc$feature),]


## [f] Save titles : Main title, Patient Diagnosis
title <- "Breast Cancer Diagnosis Radar"
subtitle <- cancer_diagnosis_predict_p(new)



## ★[g] Radar plot
res <- ggplot(df, aes(x=feature,y=value,group=group,fill=group,color=group))+
    geom_point()+geom_polygon(alpha=0.3)+coord_radar()+ylim(0,max_value)+
    scale_color_manual(values=c(NA,"#b10000"))+
    scale_fill_manual(values=c("#8e8e8e",NA))+
    facet_wrap(~type)+
    theme(panel.background=element_rect(fill = "white", colour= NA),
          panel.border=element_rect(fill = NA, colour="grey50"), 
          panel.grid.major=element_line(colour = "grey90", size = 0.2),
          panel.grid.minor=element_line(colour = "grey98", size = 0.5),
          legend.position="bottom",
          strip.background =  element_rect(fill = "grey80", colour = "grey50"),
          axis.text.y=element_text(colour=NA),
          axis.title.y=element_text(colour=NA),
          axis.ticks=element_line(colour = NA))+
          xlab("")+ylab("")+
    labs(title=title, subtitle=subtitle)+
    theme(plot.title = element_text(face='bold', colour='black', hjust=0.5, size=15))+
    theme(plot.subtitle=element_text(lineheight=0.8, hjust=0.5, size=12))+
    labs(caption="[Training 569 wisc cancer diagnostic patient data]")



## [h] output graph
res

}
```

### 9-2) Testing Function

#### Benign

``` r
library(ggplot2)   

cancer_radar(B,breast_cancer_data)
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-92-1.png)<!-- -->

#### Malignant

``` r
cancer_radar(M,breast_cancer_data)  
```

![](Breast_Cancer_code_01_files/figure-gfm/unnamed-chunk-93-1.png)<!-- -->

------------------------------------------------------------------------

# 10. Conclusion

저는 최근에 이진 분류를 사용하여 이 **wisconsin breast cancer
dataset**의 Python 버전을 업로드했습니다.

기존 ML 방법을 사용하지 않고 이 데이터 세트에 맞게 조정된 인공 신경망
모델을 만듭니다.

- **Python Version**
  [here](https://www.kaggle.com/mirichoi0218/ann-making-model-for-binary-classification)
