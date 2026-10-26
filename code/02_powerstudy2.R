
## packages
library(here)
library(dplyr)
library(pwrss)

## load clean data from round 1
r1_df <- read.csv(here("data/analyticsample_round1.csv"))

## get correlations for power analysis 
## -0.02 correlation between barriers treatment and points system rank
## (0.02 for reverse coding)
cor(r1_df$is_any_barriers, r1_df$points_rank)

## model r2 - 0.0004
main_effect <- lm(points_rank ~ is_any_barriers,
         data = model_df)
summary(main_effect)

## model r2 for bjw - 0.05
summary(lm(points_rank ~ is_any_barriers*jw_combined,
           data = model_df))

## model r2 for political affil - 0.07
summary(lm(points_rank ~ is_any_barriers*relevel(factor(political_affil_buckets),
                                                 ref = "Democrat"),
           data = model_df))

## power just with main conditions - doing higher than just the barriers
## since we assume the points condition has stronger effect but lower
## sample size 1084
## than with demographics
pwrss.f.regression(
  r2 = 0.01,        # Expected R-squared
  k = 3,            # points, barriers, points x barriers 
  alpha = 0.05, 
  power = 0.80
)

## power with moderators - similarly use lower r^2 than bjw
## sample size 680 due to higher r^2
pwrss.f.regression(
  r2 = 0.02,        # Expected R-squared
  k = 6,            # points, barriers, bjw, bjw x points, bjw x barriers, bjw x points x barriers
  alpha = 0.05, 
  power = 0.80
)


