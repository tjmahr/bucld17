Growth curve models
================
Tristan Mahr
2017-10-31

-   [Set up](#set-up)
    -   [Add orthogonal polynomials](#add-orthogonal-polynomials)
    -   [Prep the datasets](#prep-the-datasets)
-   [Fit the vocabulary-less models](#fit-the-vocabulary-less-models)
-   [Fit the models that include vocabulary isze](#fit-the-models-that-include-vocabulary-isze)
    -   [Mispronunciations](#mispronunciations)
    -   [Real words](#real-words)
    -   [Nonword models](#nonword-models)

Set up
------

``` r
library(dplyr)
library(littlelisteners)
library(ggplot2)
library(lme4)

source("./plotting-helpers.R", encoding = "UTF8")
looks <- readr::read_csv("./data/model.csv.gz") %>% 
  mutate(
    Cond_Lab = Condition %>% 
      factor(c("real", "MP", "nonsense"),
             c("Real word", "Mispronunciation", "Nonword")))
```

### Add orthogonal polynomials

``` r
looks <- looks %>% 
  polypoly::poly_add_columns(Time, degree = 3, prefix = "ot") 
```

### Prep the datasets

For the models with vocabulary information, we need to remove any pairs of children in which one of the children is missing an EVT score. We identify those pairs.

``` r
no_vocab_pairs <- looks %>%
  distinct(ChildStudyID, Matching_PairNumber, EVT_Standard) %>%
  filter(is.na(EVT_Standard)) %>%
  select(Matching_PairNumber) %>%
  print()
#> # A tibble: 1 x 1
#>   Matching_PairNumber
#>                 <int>
#> 1                  15
```

Prepare a dataset for each condition.

``` r
d_mp <- looks %>% 
  filter(Condition == "MP")

d_rw <- looks %>% 
  filter(Condition == "real")

d_ns <- looks %>% 
  filter(Condition == "nonsense")

d_mp_evt <- d_mp %>% 
  anti_join(no_vocab_pairs, by = "Matching_PairNumber")

d_rw_evt <- d_rw %>% 
  anti_join(no_vocab_pairs, by = "Matching_PairNumber")

d_ns_evt <- d_ns %>% 
  anti_join(no_vocab_pairs, by = "Matching_PairNumber")
```

Fit the vocabulary-less models
------------------------------

First, we just look at the group effects on the growth curve shapes.

``` r
glmer_controls <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 2e5))

m_mp <- glmer(
  cbind(Target, Distractor) ~ 
    Group * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_mp)
summary(m_mp)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * (ot1 + ot2 + ot3) + (ot1 +  
#>     ot2 + ot3 | ChildStudyID)
#>    Data: d_mp
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11917.1  12020.4  -5940.6  11881.1     2276 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.40445 -0.41933 -0.00934  0.42238  2.39728 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.3694   0.6078                    
#>               ot1         3.8897   1.9722    0.46            
#>               ot2         0.8285   0.9102   -0.32 -0.32      
#>               ot3         0.4418   0.6647    0.15 -0.43 -0.07
#> Number of obs: 2294, groups:  ChildStudyID, 74
#> 
#> Fixed effects:
#>                         Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)            -0.004854   0.100360  -0.048   0.9614    
#> GroupNormalHearing      0.154643   0.141891   1.090   0.2758    
#> ot1                     0.327820   0.328509   0.998   0.3183    
#> ot2                     0.296101   0.158533   1.868   0.0618 .  
#> ot3                     0.111082   0.120885   0.919   0.3581    
#> GroupNormalHearing:ot1  0.734386   0.464265   1.582   0.1137    
#> GroupNormalHearing:ot2  0.229322   0.223408   1.026   0.3047    
#> GroupNormalHearing:ot3 -0.680799   0.169981  -4.005  6.2e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) GrpNrH ot1    ot2    ot3    GrNH:1 GrNH:2
#> GrpNrmlHrng -0.707                                          
#> ot1          0.451 -0.319                                   
#> ot2         -0.299  0.211 -0.298                            
#> ot3          0.132 -0.093 -0.386 -0.054                     
#> GrpNrmlHr:1 -0.319  0.452 -0.707  0.211  0.273              
#> GrpNrmlHr:2  0.212 -0.299  0.211 -0.709  0.039 -0.298       
#> GrpNrmlHr:3 -0.094  0.132  0.275  0.039 -0.711 -0.388 -0.052

m_rw <- glmer(
  cbind(Target, Distractor) ~ 
    Group * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_rw)
summary(m_rw)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * (ot1 + ot2 + ot3) + (ot1 +  
#>     ot2 + ot3 | ChildStudyID)
#>    Data: d_rw
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11558.8  11662.1  -5761.4  11522.8     2276 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -3.16093 -0.42522  0.00612  0.44104  2.44476 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.5225   0.7228                    
#>               ot1         4.8612   2.2048    0.49            
#>               ot2         1.7769   1.3330   -0.24 -0.19      
#>               ot3         0.6677   0.8171   -0.28 -0.46  0.34
#> Number of obs: 2294, groups:  ChildStudyID, 74
#> 
#> Fixed effects:
#>                        Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)              0.7337     0.1194   6.146 7.96e-10 ***
#> GroupNormalHearing       0.4500     0.1688   2.665 0.007697 ** 
#> ot1                      2.3730     0.3681   6.447 1.14e-10 ***
#> ot2                     -0.7607     0.2278  -3.340 0.000838 ***
#> ot3                     -0.1179     0.1468  -0.803 0.421818    
#> GroupNormalHearing:ot1   0.2147     0.5203   0.413 0.679883    
#> GroupNormalHearing:ot2  -0.5656     0.3216  -1.759 0.078560 .  
#> GroupNormalHearing:ot3   0.1678     0.2067   0.811 0.417100    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) GrpNrH ot1    ot2    ot3    GrNH:1 GrNH:2
#> GrpNrmlHrng -0.707                                          
#> ot1          0.488 -0.345                                   
#> ot2         -0.230  0.163 -0.170                            
#> ot3         -0.255  0.180 -0.407  0.314                     
#> GrpNrmlHr:1 -0.345  0.490 -0.707  0.121  0.288              
#> GrpNrmlHr:2  0.163 -0.231  0.121 -0.707 -0.220 -0.169       
#> GrpNrmlHr:3  0.181 -0.256  0.289 -0.221 -0.707 -0.412  0.317

m_ns <- glmer(
  cbind(Target, Distractor) ~ 
    Group * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_ns)
summary(m_ns)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * (ot1 + ot2 + ot3) + (ot1 +  
#>     ot2 + ot3 | ChildStudyID)
#>    Data: d_ns
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11534.9  11638.2  -5749.4  11498.9     2276 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.74598 -0.46161 -0.02695  0.45878  2.74382 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.3753   0.6127                    
#>               ot1         5.5977   2.3660    0.47            
#>               ot2         1.6033   1.2662    0.04  0.24      
#>               ot3         1.1856   1.0888   -0.16 -0.41 -0.30
#> Number of obs: 2294, groups:  ChildStudyID, 74
#> 
#> Fixed effects:
#>                        Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)            -0.74499    0.10148  -7.341 2.11e-13 ***
#> GroupNormalHearing     -0.55025    0.14341  -3.837 0.000125 ***
#> ot1                    -1.62901    0.39598  -4.114 3.89e-05 ***
#> ot2                     0.19217    0.21894   0.878 0.380103    
#> ot3                     0.05652    0.18919   0.299 0.765131    
#> GroupNormalHearing:ot1 -0.48871    0.55840  -0.875 0.381464    
#> GroupNormalHearing:ot2  0.30571    0.30795   0.993 0.320848    
#> GroupNormalHearing:ot3  0.01835    0.26714   0.069 0.945227    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) GrpNrH ot1    ot2    ot3    GrNH:1 GrNH:2
#> GrpNrmlHrng -0.707                                          
#> ot1          0.465 -0.329                                   
#> ot2          0.047 -0.033  0.239                            
#> ot3         -0.147  0.104 -0.374 -0.245                     
#> GrpNrmlHr:1 -0.330  0.465 -0.708 -0.168  0.265              
#> GrpNrmlHr:2 -0.033  0.044 -0.169 -0.709  0.175  0.235       
#> GrpNrmlHr:3  0.104 -0.149  0.266  0.175 -0.707 -0.380 -0.251
```

We can plot the growth curve fixed effects which describe how the average child in each group x condition performs.

<img src="03-models_files/figure-markdown_github-ascii_identifiers/overall-fits-1.png" width="80%" /><img src="03-models_files/figure-markdown_github-ascii_identifiers/overall-fits-2.png" width="80%" />

Fit the models that include vocabulary isze
-------------------------------------------

### Mispronunciations

<!-- The two groups significantly differ with respect to their intercept terms, and -->
<!-- not the shape of their growth curves. -->
<!-- Now, we have to refit the model the model to exclude _pairs_ of children where one of the children is missing an EVT2 score vocabulary scores. -->
Allow EVT to interact with time, time<sup>2</sup> and time<sup>3</sup> because these data are curvier.

``` r
m_mp_1a <- glmer(
  cbind(Target, Distractor) ~ 
    Group * (1 + ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_mp_evt)
summary(m_mp_1a)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + (ot1 +  
#>     ot2 + ot3 | ChildStudyID)
#>    Data: d_mp_evt
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11579.8  11682.6  -5771.9  11543.8     2214 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.39938 -0.42304 -0.00919  0.42036  2.39557 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.3718   0.6098                    
#>               ot1         3.8411   1.9599    0.47            
#>               ot2         0.8374   0.9151   -0.31 -0.35      
#>               ot3         0.4396   0.6630    0.17 -0.42 -0.07
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>                        Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)            -0.01067    0.10208  -0.105    0.917    
#> GroupNormalHearing      0.18040    0.14433   1.250    0.211    
#> ot1                     0.41115    0.33111   1.242    0.214    
#> ot2                     0.32094    0.16159   1.986    0.047 *  
#> ot3                     0.09754    0.12240   0.797    0.425    
#> GroupNormalHearing:ot1  0.69373    0.46795   1.482    0.138    
#> GroupNormalHearing:ot2  0.18965    0.22769   0.833    0.405    
#> GroupNormalHearing:ot3 -0.69506    0.17207  -4.039 5.36e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) GrpNrH ot1    ot2    ot3    GrNH:1 GrNH:2
#> GrpNrmlHrng -0.707                                          
#> ot1          0.460 -0.325                                   
#> ot2         -0.289  0.205 -0.320                            
#> ot3          0.157 -0.111 -0.370 -0.058                     
#> GrpNrmlHr:1 -0.325  0.461 -0.708  0.226  0.262              
#> GrpNrmlHr:2  0.205 -0.290  0.227 -0.710  0.041 -0.320       
#> GrpNrmlHr:3 -0.112  0.157  0.263  0.041 -0.711 -0.372 -0.056

m_mp_2a <- glmer(
  cbind(Target, Distractor) ~ 
    EVT_GSV_z * (1 + ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_mp_evt)
summary(m_mp_2a)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ EVT_GSV_z * (1 + ot1 + ot2 + ot3) +  
#>     (ot1 + ot2 + ot3 | ChildStudyID)
#>    Data: d_mp_evt
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11583.2  11685.9  -5773.6  11547.2     2214 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.37925 -0.42658 -0.00838  0.41904  2.36470 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.3245   0.5696                    
#>               ot1         3.8271   1.9563    0.45            
#>               ot2         0.8457   0.9196   -0.33 -0.33      
#>               ot3         0.5459   0.7388    0.17 -0.43 -0.11
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>               Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)    0.08006    0.06745   1.187 0.235242    
#> EVT_GSV_z      0.23520    0.06749   3.485 0.000492 ***
#> ot1            0.75864    0.23354   3.248 0.001160 ** 
#> ot2            0.41665    0.11438   3.643 0.000270 ***
#> ot3           -0.25415    0.09427  -2.696 0.007016 ** 
#> EVT_GSV_z:ot1  0.36940    0.23370   1.581 0.113956    
#> EVT_GSV_z:ot2  0.03004    0.11466   0.262 0.793306    
#> EVT_GSV_z:ot3 -0.12938    0.09459  -1.368 0.171359    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) EVT_GSV_z ot1    ot2    ot3    EVT_GSV_:1 EVT_GSV_:2
#> EVT_GSV_z    0.003                                                     
#> ot1          0.445  0.002                                              
#> ot2         -0.307 -0.001    -0.307                                    
#> ot3          0.157  0.000    -0.387 -0.088                             
#> EVT_GSV_z:1  0.002  0.445     0.003  0.000 -0.001                      
#> EVT_GSV_z:2 -0.001 -0.307     0.000  0.000  0.002 -0.305               
#> EVT_GSV_z:3  0.000  0.157    -0.001  0.002 -0.003 -0.386     -0.084

m_mp_3a <- glmer(
  cbind(Target, Distractor) ~ 
    Group * (1 + ot1 + ot2 + ot3) + 
    EVT_GSV_z * (1 + ot1 + ot2 + ot3) + 
    (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_mp_evt)
summary(m_mp_3a)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + EVT_GSV_z *  
#>     (1 + ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | ChildStudyID)
#>    Data: d_mp_evt
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11577.0  11702.7  -5766.5  11533.0     2210 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.40132 -0.42262 -0.00877  0.41963  2.38941 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.3244   0.5696                    
#>               ot1         3.7784   1.9438    0.46            
#>               ot2         0.8369   0.9148   -0.33 -0.35      
#>               ot3         0.4396   0.6630    0.18 -0.42 -0.07
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>                        Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)             0.08947    0.10034   0.892 0.372586    
#> GroupNormalHearing     -0.01859    0.14832  -0.125 0.900270    
#> ot1                     0.52447    0.34526   1.519 0.128742    
#> ot2                     0.31627    0.16972   1.863 0.062397 .  
#> ot3                     0.10543    0.12852   0.820 0.412007    
#> EVT_GSV_z               0.23911    0.07420   3.222 0.001272 ** 
#> GroupNormalHearing:ot1  0.46829    0.51007   0.918 0.358569    
#> GroupNormalHearing:ot2  0.19823    0.25006   0.793 0.427925    
#> GroupNormalHearing:ot3 -0.71001    0.18902  -3.756 0.000172 ***
#> ot1:EVT_GSV_z           0.27172    0.25528   1.064 0.287145    
#> ot2:EVT_GSV_z          -0.01120    0.12538  -0.089 0.928809    
#> ot3:EVT_GSV_z           0.01843    0.09493   0.194 0.846051    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) GrpNrH ot1    ot2    ot3    EVT_GS GrNH:1 GrNH:2 GrNH:3 o1:EVT o2:EVT
#> GrpNrmlHrng -0.740                                                                      
#> ot1          0.448 -0.332                                                               
#> ot2         -0.306  0.227 -0.321                                                        
#> ot3          0.160 -0.119 -0.376 -0.058                                                 
#> EVT_GSV_z    0.310 -0.416  0.139 -0.095  0.050                                          
#> GrpNrmlHr:1 -0.332  0.449 -0.740  0.238  0.279 -0.186                                   
#> GrpNrmlHr:2  0.228 -0.307  0.238 -0.742  0.044  0.128 -0.322                            
#> GrpNrmlHr:3 -0.119  0.160  0.280  0.044 -0.743 -0.067 -0.378 -0.058                     
#> o1:EVT_GSV_  0.139 -0.186  0.309 -0.099 -0.117  0.449 -0.415  0.134  0.158              
#> o2:EVT_GSV_ -0.095  0.128 -0.099  0.307 -0.017 -0.307  0.134 -0.414  0.025 -0.320       
#> o3:EVT_GSV_  0.050 -0.067 -0.117 -0.017  0.305  0.159  0.157  0.025 -0.414 -0.377 -0.053

m_mp_3b <- glmer(
  cbind(Target, Distractor) ~ 
    Group * EVT_GSV_z * (1 + ot1 + ot2 + ot3) + 
    (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_mp_evt)
summary(m_mp_3b)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * EVT_GSV_z * (1 + ot1 + ot2 +  
#>     ot3) + (ot1 + ot2 + ot3 | ChildStudyID)
#>    Data: d_mp_evt
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11582.9  11731.4  -5765.5  11530.9     2206 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.40132 -0.41913 -0.00741  0.41865  2.39107 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.3244   0.5696                    
#>               ot1         3.7072   1.9254    0.46            
#>               ot2         0.8316   0.9119   -0.33 -0.34      
#>               ot3         0.4365   0.6607    0.18 -0.42 -0.08
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>                                  Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)                       0.09196    0.10165   0.905 0.365658    
#> GroupNormalHearing               -0.01222    0.15422  -0.079 0.936843    
#> EVT_GSV_z                         0.24497    0.08374   2.925 0.003442 ** 
#> ot1                               0.45941    0.34663   1.325 0.185052    
#> ot2                               0.33355    0.17144   1.946 0.051705 .  
#> ot3                               0.11899    0.12980   0.917 0.359287    
#> GroupNormalHearing:EVT_GSV_z     -0.02703    0.18064  -0.150 0.881045    
#> GroupNormalHearing:ot1            0.29984    0.52558   0.570 0.568345    
#> GroupNormalHearing:ot2            0.24291    0.25915   0.937 0.348584    
#> GroupNormalHearing:ot3           -0.67521    0.19583  -3.448 0.000565 ***
#> EVT_GSV_z:ot1                     0.11674    0.28550   0.409 0.682615    
#> EVT_GSV_z:ot2                     0.02992    0.14123   0.212 0.832229    
#> EVT_GSV_z:ot3                     0.05073    0.10706   0.474 0.635600    
#> GroupNormalHearing:EVT_GSV_z:ot1  0.71998    0.61583   1.169 0.242351    
#> GroupNormalHearing:EVT_GSV_z:ot2 -0.19018    0.30364  -0.626 0.531105    
#> GroupNormalHearing:EVT_GSV_z:ot3 -0.14996    0.22917  -0.654 0.512874    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The last two plots are from the Group + Vocab models.

<img src="03-models_files/figure-markdown_github-ascii_identifiers/mp-plots-1.png" width="80%" /><img src="03-models_files/figure-markdown_github-ascii_identifiers/mp-plots-2.png" width="80%" /><img src="03-models_files/figure-markdown_github-ascii_identifiers/mp-plots-3.png" width="80%" /><img src="03-models_files/figure-markdown_github-ascii_identifiers/mp-plots-4.png" width="80%" />

Model comparison here should be taken with a grain of salt because we are adding predictors in batches of four coefficients (the predictor's main effect and interactions with time). AIC and BIC penalize fit measures based on number of parameters, so we are adding big penalities with each additional predictor. Consider this more of coarse look at how batches of coefficients improve model fit.

``` r
do.call(anova, unname(models))
#> Data: d_mp_evt
#> Models:
#> MODEL1: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + (ot1 + 
#> MODEL1:     ot2 + ot3 | ChildStudyID)
#> MODEL2: cbind(Target, Distractor) ~ EVT_GSV_z * (1 + ot1 + ot2 + ot3) + 
#> MODEL2:     (ot1 + ot2 + ot3 | ChildStudyID)
#> MODEL3: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + EVT_GSV_z * 
#> MODEL3:     (1 + ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | ChildStudyID)
#> MODEL4: cbind(Target, Distractor) ~ Group * EVT_GSV_z * (1 + ot1 + ot2 + 
#> MODEL4:     ot3) + (ot1 + ot2 + ot3 | ChildStudyID)
#>        Df   AIC   BIC  logLik deviance   Chisq Chi Df Pr(>Chisq)   
#> MODEL1 18 11580 11683 -5771.9    11544                             
#> MODEL2 18 11583 11686 -5773.6    11547  0.0000      0   1.000000   
#> MODEL3 22 11577 11703 -5766.5    11533 14.1251      4   0.006906 **
#> MODEL4 26 11583 11731 -5765.5    11531  2.1158      4   0.714474   
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Real words

Allow EVT to interact with time<sup>1</sup>, time<sup>2</sup> and time<sup>3</sup>.

``` r
m_rw_1a <- glmer(
  cbind(Target, Distractor) ~ 
    Group * (1 + ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_rw %>% anti_join(no_vocab_pairs))
summary(m_rw_1a)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + (ot1 +  
#>     ot2 + ot3 | ChildStudyID)
#>    Data: d_rw %>% anti_join(no_vocab_pairs)
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11229.5  11332.3  -5596.7  11193.5     2214 
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.1823 -0.4226  0.0039  0.4436  2.4463 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.5384   0.7337                    
#>               ot1         4.7604   2.1818    0.51            
#>               ot2         1.8305   1.3530   -0.24 -0.18      
#>               ot3         0.6799   0.8245   -0.28 -0.44  0.35
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>                        Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)              0.7343     0.1228   5.978 2.26e-09 ***
#> GroupNormalHearing       0.4530     0.1737   2.607 0.009124 ** 
#> ot1                      2.4761     0.3695   6.701 2.07e-11 ***
#> ot2                     -0.7779     0.2343  -3.321 0.000899 ***
#> ot3                     -0.1233     0.1502  -0.821 0.411515    
#> GroupNormalHearing:ot1   0.1734     0.5224   0.332 0.739879    
#> GroupNormalHearing:ot2  -0.5336     0.3307  -1.613 0.106676    
#> GroupNormalHearing:ot3   0.1441     0.2114   0.682 0.495441    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) GrpNrH ot1    ot2    ot3    GrNH:1 GrNH:2
#> GrpNrmlHrng -0.707                                          
#> ot1          0.501 -0.354                                   
#> ot2         -0.230  0.163 -0.167                            
#> ot3         -0.254  0.180 -0.397  0.326                     
#> GrpNrmlHr:1 -0.354  0.502 -0.707  0.119  0.281              
#> GrpNrmlHr:2  0.163 -0.231  0.118 -0.707 -0.229 -0.166       
#> GrpNrmlHr:3  0.180 -0.256  0.283 -0.230 -0.707 -0.402  0.330

m_rw_2a <- glmer(
  cbind(Target, Distractor) ~ 
   EVT_GSV_z * (1 + ot1 + ot2 + ot3) + ot2 + ot3 + (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_rw %>% anti_join(no_vocab_pairs))
summary(m_rw_2a)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ EVT_GSV_z * (1 + ot1 + ot2 + ot3) +  
#>     ot2 + ot3 + (ot1 + ot2 + ot3 | ChildStudyID)
#>    Data: d_rw %>% anti_join(no_vocab_pairs)
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11227.5  11330.3  -5595.8  11191.5     2214 
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.1859 -0.4273  0.0032  0.4436  2.4542 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.5300   0.7280                    
#>               ot1         4.6949   2.1668    0.48            
#>               ot2         1.7957   1.3400   -0.22 -0.16      
#>               ot3         0.6842   0.8271   -0.28 -0.45  0.35
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>               Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)    0.96168    0.08622  11.154  < 2e-16 ***
#> EVT_GSV_z      0.24461    0.08612   2.840  0.00451 ** 
#> ot1            2.56292    0.25962   9.872  < 2e-16 ***
#> ot2           -1.04557    0.16429  -6.364 1.96e-10 ***
#> ot3           -0.05153    0.10644  -0.484  0.62827    
#> EVT_GSV_z:ot1  0.26832    0.25862   1.038  0.29949    
#> EVT_GSV_z:ot2 -0.33198    0.16292  -2.038  0.04158 *  
#> EVT_GSV_z:ot3  0.06182    0.10485   0.590  0.55545    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) EVT_GSV_z ot1    ot2    ot3    EVT_GSV_:1 EVT_GSV_:2
#> EVT_GSV_z    0.003                                                     
#> ot1          0.480  0.002                                              
#> ot2         -0.213 -0.002    -0.145                                    
#> ot3         -0.257 -0.001    -0.407  0.334                             
#> EVT_GSV_z:1  0.002  0.479     0.002  0.001 -0.003                      
#> EVT_GSV_z:2 -0.002 -0.215     0.001  0.002  0.005 -0.152               
#> EVT_GSV_z:3 -0.001 -0.259    -0.002  0.005  0.000 -0.414      0.326

m_rw_3a <- glmer(
  cbind(Target, Distractor) ~ 
    Group * (1 + ot1 + ot2 + ot3) + 
    EVT_GSV_z * (1 + ot1 + ot2 + ot3) + 
    (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_rw %>% anti_join(no_vocab_pairs))
summary(m_rw_3a)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + EVT_GSV_z *  
#>     (1 + ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | ChildStudyID)
#>    Data: d_rw %>% anti_join(no_vocab_pairs)
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11230.8  11356.5  -5593.4  11186.8     2210 
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.1835 -0.4238  0.0012  0.4436  2.4479 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.5108   0.7147                    
#>               ot1         4.6931   2.1664    0.50            
#>               ot2         1.7721   1.3312   -0.21 -0.17      
#>               ot3         0.6816   0.8256   -0.30 -0.45  0.36
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>                        Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)             0.81056    0.12590   6.438 1.21e-10 ***
#> GroupNormalHearing      0.30150    0.18622   1.619 0.105438    
#> ot1                     2.59386    0.38612   6.718 1.85e-11 ***
#> ot2                    -0.88859    0.24305  -3.656 0.000256 ***
#> ot3                    -0.10784    0.15827  -0.681 0.495628    
#> EVT_GSV_z               0.18188    0.09303   1.955 0.050569 .  
#> GroupNormalHearing:ot1 -0.06123    0.57137  -0.107 0.914664    
#> GroupNormalHearing:ot2 -0.31230    0.35934  -0.869 0.384799    
#> GroupNormalHearing:ot3  0.11041    0.23354   0.473 0.636381    
#> ot1:EVT_GSV_z           0.28151    0.28485   0.988 0.323025    
#> ot2:EVT_GSV_z          -0.26686    0.17859  -1.494 0.135092    
#> ot3:EVT_GSV_z           0.03871    0.11550   0.335 0.737502    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) GrpNrH ot1    ot2    ot3    EVT_GS GrNH:1 GrNH:2 GrNH:3 o1:EVT o2:EVT
#> GrpNrmlHrng -0.740                                                                      
#> ot1          0.490 -0.363                                                               
#> ot2         -0.198  0.146 -0.148                                                        
#> ot3         -0.271  0.200 -0.404  0.340                                                 
#> EVT_GSV_z    0.310 -0.417  0.153 -0.061 -0.083                                          
#> GrpNrmlHr:1 -0.363  0.492 -0.740  0.109  0.299 -0.206                                   
#> GrpNrmlHr:2  0.146 -0.197  0.109 -0.741 -0.252  0.081 -0.145                            
#> GrpNrmlHr:3  0.201 -0.272  0.300 -0.252 -0.741  0.113 -0.406  0.343                     
#> o1:EVT_GSV_  0.153 -0.206  0.312 -0.043 -0.125  0.491 -0.420  0.058  0.168              
#> o2:EVT_GSV_ -0.061  0.082 -0.043  0.314  0.111 -0.199  0.058 -0.422 -0.147 -0.150       
#> o3:EVT_GSV_ -0.085  0.114 -0.127  0.111  0.313 -0.273  0.170 -0.147 -0.423 -0.411  0.338

m_rw_3b <- glmer(
  cbind(Target, Distractor) ~ 
    Group * EVT_GSV_z * (1 + ot1 + ot2 + ot3) + 
    (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_rw %>% anti_join(no_vocab_pairs))
summary(m_rw_3b)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * EVT_GSV_z * (1 + ot1 + ot2 +  
#>     ot3) + (ot1 + ot2 + ot3 | ChildStudyID)
#>    Data: d_rw %>% anti_join(no_vocab_pairs)
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11229.8  11378.3  -5588.9  11177.8     2206 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -3.14537 -0.42363  0.00127  0.44243  2.44166 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.4898   0.6998                    
#>               ot1         4.4008   2.0978    0.47            
#>               ot2         1.7476   1.3220   -0.24 -0.20      
#>               ot3         0.6570   0.8105   -0.27 -0.43  0.39
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>                                  Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)                       0.84577    0.12493   6.770 1.29e-11 ***
#> GroupNormalHearing                0.39437    0.18976   2.078 0.037687 *  
#> EVT_GSV_z                         0.26639    0.10278   2.592 0.009546 ** 
#> ot1                               2.72474    0.37928   7.184 6.77e-13 ***
#> ot2                              -0.85015    0.24477  -3.473 0.000514 ***
#> ot3                              -0.14386    0.15799  -0.911 0.362527    
#> GroupNormalHearing:EVT_GSV_z     -0.39406    0.22210  -1.774 0.076026 .  
#> GroupNormalHearing:ot1            0.28480    0.57711   0.493 0.621664    
#> GroupNormalHearing:ot2           -0.20813    0.37205  -0.559 0.575889    
#> GroupNormalHearing:ot3            0.01236    0.23981   0.052 0.958903    
#> EVT_GSV_z:ot1                     0.59507    0.31120   1.912 0.055858 .  
#> EVT_GSV_z:ot2                    -0.17237    0.19999  -0.862 0.388754    
#> EVT_GSV_z:ot3                    -0.05080    0.12810  -0.397 0.691673    
#> GroupNormalHearing:EVT_GSV_z:ot1 -1.46614    0.67440  -2.174 0.029705 *  
#> GroupNormalHearing:EVT_GSV_z:ot2 -0.43681    0.43398  -1.007 0.314162    
#> GroupNormalHearing:EVT_GSV_z:ot3  0.41554    0.27846   1.492 0.135633    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The last two plots are from the Group + Vocab model.

<img src="03-models_files/figure-markdown_github-ascii_identifiers/rw-plots-1.png" width="80%" /><img src="03-models_files/figure-markdown_github-ascii_identifiers/rw-plots-2.png" width="80%" /><img src="03-models_files/figure-markdown_github-ascii_identifiers/rw-plots-3.png" width="80%" />

This final plot is from the Group x Vocab model. I think the Normal Hearing side is overly sensitive to some outlying behavior in the high EVT normal hearing group.

``` r
ggplot(fits %>% filter(Model == "Group x Vocab.")) +
  aes(x = Time, y = fitted, color = quantile) + 
  hline_chance() +
  geom_line(
    aes(x = Time, y = fitted, group = interaction(Group, quantile)),
    size = 1) + 
  facet_wrap("Group_Lab") + 
  viridis::scale_color_viridis(discrete = TRUE, end = .85, option = "B") +
  labs(x = plot_text$x_time,
       y = plot_text$y_fits,
       color = "Within group EVT quantiles") + 
  legend_bottom(legend.justification = "right") + 
  align_axis_right() + 
  ggtitle("Vocabulary effects in correct production condition")
```

<img src="03-models_files/figure-markdown_github-ascii_identifiers/rw-plots-2-1.png" width="80%" />

``` r

d_rw %>% 
  anti_join(no_vocab_pairs) %>% 
  left_join(condition_labels) %>% 
  group_by(Group) %>% 
  mutate(EVT_Bin = ntile(EVT_GSV, 5) %>% factor()) %>% 
  ungroup() %>% 
  ggplot() +
    aes(x = Time, y = Prop, color = EVT_Bin) + 
    hline_chance() +
    stat_mean_se() +
    facet_wrap("Group_Lab") + 
    viridis::scale_color_viridis(discrete = TRUE, end = .85, option = "B") +
    labs(x = plot_text$x_time,
         y = plot_text$y_fits,
         color = "Within group EVT bins") + 
    legend_bottom(legend.justification = "right") + 
    align_axis_right() + 
    ggtitle("Vocabulary effects in correct production condition")
```

<img src="03-models_files/figure-markdown_github-ascii_identifiers/rw-plots-2-2.png" width="80%" />

``` r
do.call(anova, unname(models))
#> Data: d_rw %>% anti_join(no_vocab_pairs)
#> Models:
#> MODEL1: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + (ot1 + 
#> MODEL1:     ot2 + ot3 | ChildStudyID)
#> MODEL2: cbind(Target, Distractor) ~ EVT_GSV_z * (1 + ot1 + ot2 + ot3) + 
#> MODEL2:     ot2 + ot3 + (ot1 + ot2 + ot3 | ChildStudyID)
#> MODEL3: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + EVT_GSV_z * 
#> MODEL3:     (1 + ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | ChildStudyID)
#> MODEL4: cbind(Target, Distractor) ~ Group * EVT_GSV_z * (1 + ot1 + ot2 + 
#> MODEL4:     ot3) + (ot1 + ot2 + ot3 | ChildStudyID)
#>        Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#> MODEL1 18 11230 11332 -5596.7    11194                             
#> MODEL2 18 11228 11330 -5595.8    11192 1.9524      0    < 2e-16 ***
#> MODEL3 22 11231 11356 -5593.4    11187 4.6992      4    0.31957    
#> MODEL4 26 11230 11378 -5588.9    11178 9.0176      4    0.06066 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Nonword models

Allow EVT to interact with time<sup>1</sup>.

``` r
m_ns_1a <- glmer(
  cbind(Target, Distractor) ~ 
    Group * (1 + ot1) + ot2 + ot3 + (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_ns %>% anti_join(no_vocab_pairs))
summary(m_ns_1a)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * (1 + ot1) + ot2 + ot3 + (ot1 +  
#>     ot2 + ot3 | ChildStudyID)
#>    Data: d_ns %>% anti_join(no_vocab_pairs)
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11234.3  11325.6  -5601.1  11202.3     2216 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.75401 -0.45857 -0.02161  0.46262  2.79496 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.3713   0.6093                    
#>               ot1         5.6498   2.3769    0.46            
#>               ot2         1.6375   1.2797    0.06  0.24      
#>               ot3         1.0864   1.0423   -0.11 -0.38 -0.30
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>                        Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)            -0.73810    0.10243  -7.206 5.77e-13 ***
#> GroupNormalHearing     -0.53385    0.14506  -3.680 0.000233 ***
#> ot1                    -1.58575    0.38863  -4.080 4.50e-05 ***
#> ot2                     0.31183    0.15808   1.973 0.048545 *  
#> ot3                     0.04995    0.13045   0.383 0.701774    
#> GroupNormalHearing:ot1 -0.53819    0.52867  -1.018 0.308679    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) GrpNrH ot1    ot2    ot3   
#> GrpNrmlHrng -0.708                            
#> ot1          0.449 -0.303                     
#> ot2          0.040  0.004  0.176              
#> ot3         -0.073  0.003 -0.256 -0.244       
#> GrpNrmlHr:1 -0.316  0.447 -0.681  0.004  0.002

m_ns_2a <- glmer(
  cbind(Target, Distractor) ~ 
    EVT_GSV_z * (1 + ot1) + ot2 + ot3 + (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_ns %>% anti_join(no_vocab_pairs))
summary(m_ns_2a)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ EVT_GSV_z * (1 + ot1) + ot2 + ot3 +  
#>     (ot1 + ot2 + ot3 | ChildStudyID)
#>    Data: d_ns %>% anti_join(no_vocab_pairs)
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11227.4  11318.7  -5597.7  11195.4     2216 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.75440 -0.45644 -0.02429  0.46163  2.70958 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.3415   0.5844                    
#>               ot1         5.5813   2.3625    0.44            
#>               ot2         1.6299   1.2767    0.02  0.23      
#>               ot3         1.0829   1.0406   -0.18 -0.42 -0.30
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>               Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)   -1.00615    0.06940 -14.498  < 2e-16 ***
#> EVT_GSV_z     -0.32791    0.06892  -4.758 1.95e-06 ***
#> ot1           -1.85436    0.28294  -6.554 5.61e-11 ***
#> ot2            0.31633    0.15765   2.007   0.0448 *  
#> ot3            0.05420    0.13021   0.416   0.6772    
#> EVT_GSV_z:ot1 -0.46303    0.25986  -1.782   0.0748 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) EVT_GSV_z ot1    ot2    ot3   
#> EVT_GSV_z    0.005                               
#> ot1          0.438  0.005                        
#> ot2          0.023  0.001     0.234              
#> ot3         -0.169 -0.001    -0.379 -0.249       
#> EVT_GSV_z:1  0.005  0.416     0.008  0.005  0.000

m_ns_3a <- glmer(
  cbind(Target, Distractor) ~ 
    Group * (1 + ot1 + ot2 + ot3) + 
    EVT_GSV_z * (1 + ot1) + 
    (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_ns %>% anti_join(no_vocab_pairs))
summary(m_ns_3a)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + EVT_GSV_z *  
#>     (1 + ot1) + (ot1 + ot2 + ot3 | ChildStudyID)
#>    Data: d_ns %>% anti_join(no_vocab_pairs)
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11229.3  11343.5  -5594.6  11189.3     2212 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.74976 -0.45788 -0.02647  0.46007  2.70574 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.3203   0.5659                    
#>               ot1         5.5747   2.3611    0.45            
#>               ot2         1.5988   1.2644    0.05  0.24      
#>               ot3         1.0802   1.0393   -0.18 -0.41 -0.30
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>                        Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)            -0.85358    0.10018  -8.521  < 2e-16 ***
#> GroupNormalHearing     -0.30431    0.14792  -2.057 0.039660 *  
#> ot1                    -1.84677    0.41933  -4.404 1.06e-05 ***
#> ot2                     0.13780    0.22178   0.621 0.534375    
#> ot3                     0.10082    0.18408   0.548 0.583900    
#> EVT_GSV_z              -0.26232    0.07359  -3.565 0.000364 ***
#> GroupNormalHearing:ot1 -0.01545    0.61441  -0.025 0.979935    
#> GroupNormalHearing:ot2  0.35369    0.31171   1.135 0.256504    
#> GroupNormalHearing:ot3 -0.09483    0.25966  -0.365 0.714964    
#> ot1:EVT_GSV_z          -0.42565    0.28620  -1.487 0.136956    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) GrpNrH ot1    ot2    ot3    EVT_GS GrNH:1 GrNH:2 GrNH:3
#> GrpNrmlHrng -0.741                                                        
#> ot1          0.447 -0.328                                                 
#> ot2          0.057 -0.038  0.232                                          
#> ot3         -0.158  0.108 -0.355 -0.244                                   
#> EVT_GSV_z    0.312 -0.417  0.126  0.001 -0.001                            
#> GrpNrmlHr:1 -0.331  0.444 -0.738 -0.159  0.242 -0.168                     
#> GrpNrmlHr:2 -0.040  0.051 -0.165 -0.709  0.175 -0.002  0.220              
#> GrpNrmlHr:3  0.112 -0.153  0.252  0.175 -0.708 -0.002 -0.347 -0.251       
#> o1:EVT_GSV_  0.136 -0.179  0.294  0.008  0.001  0.423 -0.393 -0.007 -0.001

m_ns_3b <- glmer(
  cbind(Target, Distractor) ~ 
    Group * (1 + ot1 + ot2 + ot3) + 
    Group * EVT_GSV_z * (1 + ot1) + 
    (ot1 + ot2 + ot3 | ChildStudyID),
  family = binomial,
  control = glmer_controls,
  data = d_ns %>% anti_join(no_vocab_pairs))
summary(m_ns_3b)
#> Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#>  Family: binomial  ( logit )
#> Formula: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + Group *  
#>     EVT_GSV_z * (1 + ot1) + (ot1 + ot2 + ot3 | ChildStudyID)
#>    Data: d_ns %>% anti_join(no_vocab_pairs)
#> Control: glmer_controls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>  11231.3  11356.9  -5593.6  11187.3     2210 
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.74983 -0.45753 -0.02828  0.46153  2.75570 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev. Corr             
#>  ChildStudyID (Intercept) 0.3177   0.5637                    
#>               ot1         5.3901   2.3217    0.44            
#>               ot2         1.5982   1.2642    0.05  0.24      
#>               ot3         1.0808   1.0396   -0.17 -0.40 -0.30
#> Number of obs: 2232, groups:  ChildStudyID, 72
#> 
#> Fixed effects:
#>                                  Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)                      -0.86416    0.10106  -8.551  < 2e-16 ***
#> GroupNormalHearing               -0.33296    0.15322  -2.173 0.029769 *  
#> ot1                              -1.93199    0.41765  -4.626 3.73e-06 ***
#> ot2                               0.13760    0.22176   0.621 0.534925    
#> ot3                               0.10026    0.18414   0.544 0.586115    
#> EVT_GSV_z                        -0.28762    0.08236  -3.492 0.000479 ***
#> GroupNormalHearing:ot1           -0.24636    0.62593  -0.394 0.693876    
#> GroupNormalHearing:ot2            0.35362    0.31170   1.135 0.256580    
#> GroupNormalHearing:ot3           -0.09416    0.25977  -0.362 0.716983    
#> GroupNormalHearing:EVT_GSV_z      0.12031    0.17741   0.678 0.497670    
#> ot1:EVT_GSV_z                    -0.62835    0.31711  -1.982 0.047534 *  
#> GroupNormalHearing:ot1:EVT_GSV_z  0.96888    0.68391   1.417 0.156575    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) GrpNrH ot1    ot2    ot3    EVT_GS GrNH:1 GrNH:2 GrNH:3 GNH:EV o1:EVT
#> GrpNrmlHrng -0.661                                                                      
#> ot1          0.439 -0.290                                                               
#> ot2          0.055 -0.034  0.229                                                        
#> ot3         -0.151  0.101 -0.343 -0.244                                                 
#> EVT_GSV_z    0.346 -0.233  0.139  0.003  0.000                                          
#> GrpNrmlHr:1 -0.294  0.435 -0.668 -0.151  0.231 -0.095                                   
#> GrpNrmlHr:2 -0.039  0.047 -0.162 -0.709  0.175 -0.003  0.210                            
#> GrpNrmlHr:3  0.107 -0.142  0.244  0.175 -0.708 -0.002 -0.330 -0.252                     
#> GNH:EVT_GSV -0.157 -0.273 -0.064 -0.005 -0.003 -0.454 -0.105  0.003  0.001              
#> o1:EVT_GSV_  0.149 -0.100  0.329  0.011  0.004  0.418 -0.224 -0.008 -0.003 -0.190       
#> GNH:1:EVT_G -0.068 -0.112 -0.151 -0.008 -0.007 -0.189 -0.256  0.004  0.004  0.415 -0.454
```

The last two plots are from the Group + Vocab model.

<img src="03-models_files/figure-markdown_github-ascii_identifiers/ns-plots-1.png" width="80%" /><img src="03-models_files/figure-markdown_github-ascii_identifiers/ns-plots-2.png" width="80%" /><img src="03-models_files/figure-markdown_github-ascii_identifiers/ns-plots-3.png" width="80%" /><img src="03-models_files/figure-markdown_github-ascii_identifiers/ns-plots-4.png" width="80%" />

``` r
do.call(anova, unname(models))
#> Data: d_ns %>% anti_join(no_vocab_pairs)
#> Models:
#> MODEL1: cbind(Target, Distractor) ~ Group * (1 + ot1) + ot2 + ot3 + (ot1 + 
#> MODEL1:     ot2 + ot3 | ChildStudyID)
#> MODEL2: cbind(Target, Distractor) ~ EVT_GSV_z * (1 + ot1) + ot2 + ot3 + 
#> MODEL2:     (ot1 + ot2 + ot3 | ChildStudyID)
#> MODEL3: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + EVT_GSV_z * 
#> MODEL3:     (1 + ot1) + (ot1 + ot2 + ot3 | ChildStudyID)
#> MODEL4: cbind(Target, Distractor) ~ Group * (1 + ot1 + ot2 + ot3) + Group * 
#> MODEL4:     EVT_GSV_z * (1 + ot1) + (ot1 + ot2 + ot3 | ChildStudyID)
#>        Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#> MODEL1 16 11234 11326 -5601.1    11202                             
#> MODEL2 16 11227 11319 -5597.7    11195 6.9087      0     <2e-16 ***
#> MODEL3 20 11229 11344 -5594.6    11189 6.0965      4     0.1921    
#> MODEL4 22 11231 11357 -5593.6    11187 1.9973      2     0.3684    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

------------------------------------------------------------------------

``` r
d_ns %>% 
  anti_join(no_vocab_pairs) %>% 
  distinct(Group_Lab, ResearchID, ChildStudyID, EVT_GSV) %>% 
  ggplot() + 
    theme(axis.text.x = element_text(size = rel(1))) + 
    aes(x = Group_Lab, y = EVT_GSV) + 
    geom_boxplot(width = .4) +
    geom_point(position = position_jitter(.1), shape = 1) + 
    labs(x = NULL, y = "EVT-2 GSV") + 
    ggtitle("Children with CIs have more variable vocabularies") + 
    align_axis_right()
```

<img src="03-models_files/figure-markdown_github-ascii_identifiers/vocab-boxplot-1.png" width="80%" />
