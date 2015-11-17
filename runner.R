setwd("F:\\Documents\\Data-Science-Capstone")
setwd("/Users/fredrikemilsson/Documents/Data-Science-Capstone")

rm(list=ls()) #will remove ALL objects 

library(jsonlite)
library(tm)
library(sqldf)
library(gdata)
library(aspace)
library(ggplot2)
library(Rmisc)

source("functions.R")

# create restaurants_join
data_path_all <- "data\\yelp_dataset_challenge_academic_dataset\\"
date()
create_restaurants_join(data_path_all)
date() # 35 min

date()
remove_duplicate()
date() # 30 min

# aggregate dependency data
date()
cat_and_attr_aggregator(1000)
date() # 1000 => 39 min, 10000 => 33 min, 100000 => 23 min


# remove some values  - 211 = attr + cat
date()
restaurants_join_rank <- readRDS(file="restaurants_join_rank.rds")
restaurants_join_rank <- restaurants_join_rank[restaurants_join_rank$r_date>="2014-01-01",] # 311905    1000 => 443, 10000 => 359, 100000 => 275 
#restaurants_join_rank <- restaurants_join_rank[restaurants_join_rank$b_review_count>=100,] # 599506    1000
#restaurants_join_rank <- restaurants_join_rank[restaurants_join_rank$u_review_count>=10,] # 749410    1000
saveRDS(restaurants_join_rank, file="restaurants_join_rank.rds") 
rm(restaurants_join_rank)
date()

# split data
date()
create_train_val_test()
date()

# estimate
date()
est <- create_lm_and_val(c("^rank_urm_b")) # 0.6247532 0.6630018 0.6247532 0.6626120
date()

train_data <- readRDS(file="train_data.rds")
lm_res <- readRDS(file="lm_res.rds")   
pred <- predict(lm_res)
round_pred <- round(pred)
diff <- round_pred-train_data$r_stars
plotter <- data.frame(diff)
ggplot(plotter, aes(diff)) + geom_histogram() + xlab("Differens") + ylab(label="Hits")

t_hits_percent <- sum(as.numeric(round_pred==train_data$r_stars))/length(pred) # 0.6621605

est <- create_lm_and_val(c("^rank_urm_b")) # r_date>="2014-01-01" -> 0.5457073 0.7328620 0.5457073 0.7331239 (1000) -> 0.5634632 0.7188246 0.5634632 0.7179269 (10000)
est <- create_lm_and_val(c("^rank_urm_b")) # r_date>="2014-01-01" -> 0.5456806 0.7327231 0.5456806 0.7334605 (1000 and remove_duplicate) 
#est <- create_lm_and_val(c("^rank_urm_b")) # b_review_count>=100 -> 0.6248515 0.6676434 0.6248515 0.6671004
#est <- create_lm_and_val(c("^rank_urm_b")) # u_review_count>=10 -> 0.6572222 0.6339387 0.6572222 0.6344057
est <- create_lm_and_val(c()) # r_date>="2014-01-01" -> 1.0385499 0.4060318 1.0385499 0.4067745

# execute the test

# final validation
test_data <- readRDS(file="test_data.rds")
lm_res <- readRDS(file="lm_res.rds")   
pred <- predict(lm_res, test_data)
round_pred <- round(pred)
v_sigma <- summary(lm_res)$sigma # 0.5456806
v_hits_percent <- sum(as.numeric(round_pred==test_data$r_stars))/length(pred) # 0.7314407
c(v_sigma, v_hits_percent)

# estimate restaurant
business <- readRDS(file="business_restaurants.rds")
mean_stars <- readRDS(file="mean_stars.rds")
business <- sqldf("select * from business, mean_stars where b_business_id=m_business_id")

user_rank_mean <- readRDS(file="user_rank_mean.rds") 
user <- readRDS(file="user_restaurants.rds") 
user_join <- sqldf("select * from user, user_rank_mean where u_user_id=urm_user_id")

lm_res <- readRDS(file="lm_res.rds")   

# las vegas
lat <- 36.114647
lng <- -115.172813
max_dist <- 500
#urm_user_id <- "--_L4WuJxAfQkRtC1e43hg"
urm_user_id <- "Kq8-FUG7d_MT2qRNiNBJnA"
user <- user_join[user_join$urm_user_id==urm_user_id,]
get_matching_companies(business, lat, lng, max_dist, user, lm_res)

# urm_user_id <- "--_L4WuJxAfQkRtC1e43hg"
#pred                     b_name b_stars
#118 4.574049 Off The Strip Bistro & Bar     4.5
#36  4.483504                  Guy Savoy     4.5
#18  4.473889                    Picasso     4.5
#117 4.458032               Border Grill     4.5
#12  4.381886                  Le Cirque     4.0

# urm_user_id <- "Kq8-FUG7d_MT2qRNiNBJnA"
#pred                     b_name b_stars
#118 4.905904 Off The Strip Bistro & Bar     4.5
#117 4.769857               Border Grill     4.5
#115 4.559424     Chipotle Mexican Grill     3.5
#73  4.474049               Secret Pizza     4.0
#89  4.406475   Jean Philippe Patisserie     4.0

names <- c("lm only u_average_stars and b_stars","lm only u_average_stars and m_stars","lm with aggregated data 1000 limit","lm with aggregated data 10000 limit","lm with aggregated data 100000 limit")
t_sigma <- c(1.0449858, 1.0385499, 0.5456806, 0.5643608, 0.6437713)
t_hits_percent
v_sigma
v_hits_percent
tt_sigma
tt_hits_percent


##b_stars
#1.0449858 0.4032692 1.0449858 0.4021737 1.044986 0.4058447
#with aggregated data (1000 limit and remove_duplicate)
0.5456806 0.7327231 0.5456806 0.7334605 0.5456806 0.7314407
#with aggregated data (10000 limit and remove_duplicate)
0.5643608 0.7178788 0.5643608 0.7173017 0.5643608 0.7163399
#with aggregated data (100000 limit and remove_duplicate)
0.6437713 0.6708934 0.6437713 0.6693064 0.6437713 0.6688254
#m_stars
1.0385499 0.4060318 1.0385499 0.4067745 1.0385499 0.4093073



n <- c(443-211,359-211,275-211,0,443-211,359-211,275-211,0,443-211,359-211,275-211,0)
sigma <- c(0.5456806, 0.5643608, 0.6437713, 1.0385499, 0.5456806, 0.5643608, 0.6437713, 1.0385499,0.5456806, 0.5643608, 0.6437713, 1.0385499)
hits_percent <- c(0.7327231, 0.7178788, 0.6708934, 0.4060318, 0.7334605, 0.7173017, 0.6693064, 0.4067745,0.7314407, 0.7163399, 0.6688254, 0.4093073) * 100
type <- c("train","train","train","train","val","val","val","val","test","test","test","test")
plotter <- data.frame(n, sigma, hits_percent, type)
p1 <- ggplot(data = plotter, aes(x=n, y = sigma, color = type)) + geom_line() + labs(color = "Sigma") + xlab("Number of types") + ylab(label="Sigma")
p2 <- ggplot(data = plotter, aes(x=n, y = hits_percent, color = type)) + geom_line() + labs(color = "Hits %") + xlab("Number of types") + ylab(label="Hits percent")
multiplot(p1, p2, cols=2)

