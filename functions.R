updateNames <- function(nameList, prefix) {
  nameList <- gsub(" ", "_", nameList)
  nameList <- gsub("-", "_", nameList)
  nameList <- gsub("/", "_", nameList)
  nameList <- gsub("&", "_", nameList)
  nameList <- gsub("\\.", "__", nameList)
  nameList <- gsub("'", "", nameList)
  nameList <- gsub("`", "", nameList)
  nameList <- gsub("\\(", "", nameList)
  nameList <- gsub("\\)", "", nameList)
  nameList <- gsub(",", "", nameList)
  nameList <- paste(prefix, "_", nameList, sep="") 
}

create_restaurants_join <- function(data_path_all) {
  # all data
  business_path <- paste(data_path_all, "yelp_academic_dataset_business.fix.json", sep="")
  checkin_path <- paste(data_path_all, "yelp_academic_dataset_checkin.json", sep="")
  review_path <- paste(data_path_all, "yelp_academic_dataset_review.json", sep="")
  tip_path <- paste(data_path_all, "yelp_academic_dataset_tip.json", sep="")
  user_path <- paste(data_path_all, "yelp_academic_dataset_user.json", sep="")
  
  business <- fromJSON(paste("[", paste(readLines(business_path), collapse=","), "]"), flatten = TRUE)
  names(business) <- updateNames(names(business), "b") 
  business$b_attributes__Good_For_Kids__true <- NULL
  business$b_attributes__Good_For_Kids__false <- NULL
  
  checkin <- fromJSON(paste("[", paste(readLines(checkin_path), collapse=","), "]"), flatten = TRUE)
  names(checkin) <- updateNames(names(checkin), "c") 
  
  review <- fromJSON(paste("[", paste(readLines(review_path), collapse=","), "]"), flatten = TRUE)
  names(review) <- updateNames(names(review), "r") 
  
  tip <- fromJSON(paste("[", paste(readLines(tip_path), collapse=","), "]"), flatten = TRUE)
  names(tip) <- updateNames(names(tip), "t") 
  
  user <- fromJSON(paste("[", paste(readLines(user_path), collapse=","), "]"), flatten = TRUE)
  names(user) <- updateNames(names(user), "u") 
  
  user_extra <- user
  user_extra$u_type <- NULL
  user_extra$u_name <- NULL
  user_extra$u_review_count <- NULL
  user_extra$u_average_stars <- NULL
  user_extra$u_votes__funny <- NULL
  user_extra$u_votes__useful <- NULL
  user_extra$u_votes__cool <- NULL
  user_extra$u_yelping_since <- NULL
  user_extra$u_compliments__profile <- NULL
  user_extra$u_compliments__cute <- NULL
  user_extra$u_compliments__funny <- NULL
  user_extra$u_compliments__plain <- NULL
  user_extra$u_compliments__writer <- NULL
  user_extra$u_compliments__note <- NULL
  user_extra$u_compliments__photos <- NULL
  user_extra$u_compliments__hot <- NULL 
  user_extra$u_compliments__cool <- NULL
  user_extra$u_compliments__more <- NULL
  user_extra$u_compliments__list <- NULL
  user_extra$u_fans <- NULL
  
  user$u_friends <- NULL
  user$u_elite <- NULL
  
  business_restaurants <- sqldf("select * from business where b_categories__Restaurants=1")
  review_restaurants <- sqldf("select distinct r.* from business_restaurants b, review r where r_business_id=b_business_id")
  user_restaurants <- sqldf("select distinct u.* from review_restaurants r, user u where r_user_id=u_user_id")
  checkin_restaurants <- sqldf("select distinct c.* from business_restaurants b, checkin c where c_business_id=b_business_id")
  tip_restaurants <- sqldf("select distinct t.* from business_restaurants b, tip t where t_business_id=b_business_id")
  mean_stars <- sqldf("select r_business_id,avg(r_stars) from review_restaurants group by r_business_id")  
  names(mean_stars) <- c("m_business_id","m_stars")
  saveRDS(business_restaurants, file="business_restaurants.rds")   
  saveRDS(mean_stars, file="mean_stars.rds")   
  saveRDS(user_restaurants, file="user_restaurants.rds")   
  
  restaurants_join <- sqldf("select * from review_restaurants r, user_restaurants u,
                          business_restaurants b, mean_stars m 
                          where r_user_id=u_user_id and r_business_id=b_business_id and 
                          r_business_id=m_business_id")
  
  # Remove columns that only contains one unique value
  remove <- c()
  for (i in 1:dim(restaurants_join)[2])
  {
    l <- length(unique(restaurants_join[,i]))
    if (l==1) {
      remove <- c(remove, i)    
    }
  }
  restaurants_join <- restaurants_join[-remove]
  
  write.table(names(restaurants_join), "names_restaurants_join.txt", sep="\t") 
  saveRDS(restaurants_join, file="restaurants_join.rds") 
}

remove_duplicate <- function() {
  restaurants_join <- readRDS(file="restaurants_join.rds")
  
  c_and_a <- c()
  for (i in 1:dim(restaurants_join)[2]) {
    if (startsWith(names(restaurants_join)[i], "b_categories__") || startsWith(names(restaurants_join)[i], "b_attributes__")) {
      c_and_a <- c(c_and_a, i)
    }
  }
  
  tmp <- restaurants_join[,c_and_a]
  tmp[is.na(tmp)] <- 0
  
  remove <- c()
  pos_i <- 0
  for (i in c_and_a) {
    pos_i <- pos_i + 1
    pos_j <- 0 
    for (j in c_and_a) {
      pos_j <- pos_j + 1
      if (i<j) {   
        c <- cor(tmp[pos_i], tmp[pos_j])
        if (c>0.85) {
          remove <- c(remove, j)   
        }
      }
    } 
  }
  
  restaurants_join <- restaurants_join[-remove]
  saveRDS(restaurants_join, file="restaurants_join.rds")   
}


meanOfNot0 <- function(s) {
  count = 0
  for (v in s) {
    if (v!=0)
      count=count+1
  }
  if (count==0) 
    0
  else
    sum(s)/count
}

add_rank <- function(restaurants_join_rank) {
  columns <- c(grep("^b_attributes__",names(restaurants_join_rank)), 
               grep("^b_categories__",names(restaurants_join_rank)))
  
  rankNames <- names(restaurants_join_rank)
  columnNames <- rankNames[columns]
  removeColumns <- c()
  for (name in columnNames) {
    urm <- paste("urm_", name, sep="")
    if (! urm %in% rankNames) {
      removeColumns <- c(removeColumns, grep(paste("^",name,"$", sep=""),rankNames))    
    }
  }
  restaurants_join_rank <- restaurants_join_rank[-removeColumns]
  rankNames <- names(restaurants_join_rank)

  #replace all NA int by 0
  for (i in 1:dim(restaurants_join_rank)[2])
  {
    if (sapply(restaurants_join_rank[i], class)=="integer")
      restaurants_join_rank[i][is.na(restaurants_join_rank[i])] <- 0      
  }  
  
  for (i in 1:dim(restaurants_join_rank)[2])
  {
    if (sapply(restaurants_join_rank[i], class)=="numeric")
      restaurants_join_rank[i][is.na(restaurants_join_rank[i])] <- 0      
  }  
  
  ## Remove 
  
  p1 <- c()
  p2 <- c()
  urmColumns <- grep("^urm_b_",rankNames) 
  for (pos in urmColumns) {
    urm_name <- rankNames[pos]
    pos2 <- grep(paste("^",gsub("urm_", "", urm_name),"$",sep=""), rankNames)
    if (length(pos2)>0) {
      p1 <- c(p1,pos)
      p2 <- c(p2,pos2)      
    }
  }
  
  tp1 <- restaurants_join_rank[,p1]
  tp2 <- restaurants_join_rank[,p2]

  restaurants_join_rank <- restaurants_join_rank[,-c(p1,p2)]
  saveRDS(restaurants_join_rank, file="add_rank_restaurants_join_rank.rds") 
  rm(restaurants_join_rank)
  
  tp <- tp1 * tp2
  names(tp) <- paste("rank_", names(tp), sep="") 
  rm(tp1)
  rm(tp2)
  

  saveRDS(tp, file="tp.rds") 
  restaurants_join_rank <- readRDS(file="add_rank_restaurants_join_rank.rds")
  restaurants_join_rank <- data.frame(restaurants_join_rank,tp)  

  restaurants_join_rank
} 

cat_and_attr_aggregator <- function(value_limit) {
  restaurants_join <- readRDS(file="restaurants_join.rds")
  
  categories <- c()
  for (i in 1:dim(restaurants_join)[2]) {
    if (startsWith(names(restaurants_join)[i], "b_categories__")) {
      categories <- c(categories, i)
    }
  }
  
  attributes <- c()
  for (i in 1:dim(restaurants_join)[2]) {
    if (startsWith(names(restaurants_join)[i], "b_attributes__")) {
      attributes <- c(attributes, i)
    }
  }
  
  ranking <- data.frame(restaurants_join$u_average_stars, restaurants_join$b_stars, 
                        restaurants_join$r_stars, restaurants_join$u_user_id, restaurants_join[categories], 
                        restaurants_join[attributes])
  
  
  # Replace NA with 0 for int attributes
  for (i in 1:dim(ranking)[2])
  {
    if (sapply(ranking[i], class)=="integer")
      ranking[i][is.na(ranking[i])] <- 0      
  }  
  
  ### Check if columns have more than 1000 values that is not 0. Remove them!
  ###
  # Remove categories with only a few that is not 0
  few_not_0 <- c()
  for (i in 1:dim(ranking)[2])  
  {
    if (sapply(ranking[i], class)=="numeric") {
      l <- sum(ranking[,i]>0)
      if (l<value_limit) {
        few_not_0 <- c(few_not_0, i)    
      }
    }
  }
  ranking <- ranking[-few_not_0]
  
  user_data <- (ranking$restaurants_join.r_stars-ranking$restaurants_join.b_stars)*ranking[,5:ncol(ranking)]
  
  df <- data.frame(ranking[,4],user_data)
  names(df)[1] <- "u_user_id"
  
  user_rank_mean <- aggregate(df[,2:ncol(df)], by=list(df$u_user_id), meanOfNot0)
  names(user_rank_mean) <- names(ranking)[4:ncol(ranking)]
  names(user_rank_mean) <- paste("urm_", names(user_rank_mean), sep="") 
  names(user_rank_mean)[1] <- "urm_user_id"
  
  saveRDS(user_rank_mean, file="user_rank_mean.rds") 
  
  restaurants_join_rank <- sqldf("select * from user_rank_mean, restaurants_join 
                                 where urm_user_id=u_user_id")
  rm(restaurants_join)
  rm(user_rank_mean)
  restaurants_join_rank <- add_rank(restaurants_join_rank)
  
  saveRDS(restaurants_join_rank, file="restaurants_join_rank.rds") 
  write.table(names(restaurants_join_rank), "names_restaurants_join_rank.txt", sep="\t") 
}

create_train_val_test <- function() {
  set.seed(42)
  restaurants_join_rank <- readRDS(file="restaurants_join_rank.rds")
  
  reviews <- dim(restaurants_join_rank)[1]
  train_ids <- ceiling(reviews*.60)
  val_ids <- ceiling(reviews*.80)
  smpl <- sample(reviews, replace = FALSE)
  train_data <- restaurants_join_rank[smpl[1:train_ids],]
  val_data <- restaurants_join_rank[smpl[(train_ids+1):val_ids],]
  test_data <- restaurants_join_rank[smpl[(val_ids+1):reviews],]
  
  saveRDS(train_data, file="train_data.rds") 
  saveRDS(val_data, file="val_data.rds") 
  saveRDS(test_data, file="test_data.rds") 
}

create_lm_and_val <- function(included) {
  train_data <- readRDS(file="train_data.rds")
  val_data <- readRDS(file="val_data.rds")
  train_names <- names(train_data)
  
  # only get interesting columns
  # r_stars, u_average_stars, m_stars, rank_urm_b*
  columns <- c(
    grep("^r_stars$",train_names),
    grep("^u_average_stars$",train_names),
    grep("^m_stars$",train_names)
    ) 
  for (inc in included) {
    columns <- c(columns, grep(inc,train_names))
  }
  
  interesting <- train_data[,columns]
  val <- val_data[,columns]
  
  repeat{
    l <- lm(r_stars ~ ., data = interesting)
    sm <- summary(l)
    remove <- which(as.logical(coef(sm)[, "Pr(>|t|)"] >= 0.001))
    remove <- remove[remove!=1]
    interesting <- interesting[,-remove]
    val <- val[,-remove]
    if(length(remove)==0){
      break
    }
  }  
  pred <- predict(l)
  round_pred <- round(pred)
  t_sigma <- summary(l)$sigma
  t_hits_percent <- sum(as.numeric(round_pred==train_data$r_stars))/length(pred) # 0.6621605
 
  pred <- predict(l, val_data)
  round_pred <- round(pred)
  v_sigma <- summary(l)$sigma
  v_hits_percent <- sum(as.numeric(round_pred==val_data$r_stars))/length(pred) # 0.6623243
  
  saveRDS(l, file="lm_res.rds")   
  c(t_sigma, t_hits_percent, v_sigma, v_hits_percent)
}

# Calculates distance in meter
distFrom <- function(lat1, lng1, lat2, lng2) {
  earthRadius <- 6371000
  dLat <- as_radians(lat2-lat1)
  dLng <- as_radians(lng2-lng1)
  a <- sin(dLat/2) * sin(dLat/2) +
    cos(as_radians(lat1)) * cos(as_radians(lat2)) *
    sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  earthRadius * c
}

get_matching_companies <- function(business, lat, lng, max_dist, user, lm_res) {
  
  hist <- business[distFrom(business$b_latitude, business$b_longitude, lat, lng)<max_dist,]
#  print(head(hist[c(1,6,7,10)]))
  
  rank <- sqldf("select * from user, hist")
  rank <- add_rank(rank)
  rank_name <- names(rank)
  
  # u_average_stars, m_stars, rank_urm_b*
  columns <- c(
    grep("^u_average_stars$",rank_name),
    grep("^m_stars$",rank_name),
    grep("^rank_urm_b",rank_name)
  ) 
  
  rank_use <- rank[,columns]
  
  pred <- predict(lm_res, rank_use)
  
  sort_data <- data.frame(pred, rank)  
  sorted <- sort_data[ order(-sort_data[,1]), ]
  names_sorted <- names(sorted)

  columns <- c(
    grep("^pred$",names_sorted),
    grep("^b_name$",names_sorted),
    grep("^b_stars$",names_sorted)
  ) 
  sorted[1:3,columns]
  #sorted[1:5,c(1,275,278)]
}