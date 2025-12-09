# Airbnb_Linear_Regression.R — Big Data Project
#JY Edits 11/25/25
#Only keeping 5 property types, removed beds from model
#Most code was kept so that anyone can follow the thought processes

#JY Edits 11/26
#Clustering Code 


install.packages("car")

library(tidyverse)
library(car)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caTools)


# Adding variables here to set working directly and filename)
filename <- "Airbnb_Data.csv"
wd <- "/Users/j_yin/Documents/Big Data Project"

setwd(wd)

Airbnb <- read_csv("Airbnb_Data.csv", show_col_types = FALSE)
glimpse(Airbnb)
summary(Airbnb$log_price)

# 0.1) Basic feature engineering (amenities & factors) -------------------
Airbnb2 <- Airbnb %>%
  mutate(
    amenities_clean = stringr::str_remove_all(amenities, "[\\{\\}\"]"),
    amenities_list  = stringr::str_split(amenities_clean, pattern = ",")
  ) %>%
  mutate(
    amenities_list = purrr::map(amenities_list, ~ stringr::str_trim(.x)),
    # simple binary indicators for a few key amenities
    has_wifi    = purrr::map_lgl(amenities_list, ~ "Wireless Internet" %in% .x),
    has_kitchen = purrr::map_lgl(amenities_list, ~ "Kitchen" %in% .x),
    has_aircon  = purrr::map_lgl(amenities_list, ~ "Air conditioning" %in% .x),
    has_tv      = purrr::map_lgl(amenities_list, ~ "TV" %in% .x),
    amenity_count = purrr::map_int(amenities_list, length)
  ) %>%
  mutate(
    has_wifi    = as.integer(has_wifi),
    has_kitchen = as.integer(has_kitchen),
    has_aircon  = as.integer(has_aircon),
    has_tv      = as.integer(has_tv),
    cleaning_fee  = as.integer(cleaning_fee),
    property_type = factor(property_type),
    room_type     = factor(room_type)
  )

#Easier to just pull the types we want. Think we should only keep the 5 types
keep_types <- c("Apartment", "House", "Condominium", "Townhouse", "Loft")

# We'll build models on a cleaner subset with complete cases:
ModelData <- Airbnb2 %>%
  select(log_price,
         accommodates, bathrooms, bedrooms, beds,
         number_of_reviews, review_scores_rating,
         amenity_count, has_wifi, has_kitchen, has_aircon, has_tv,
         property_type, room_type, cleaning_fee) %>%
  drop_na() %>% filter(property_type %in% keep_types)

glimpse(ModelData)
summary(ModelData$log_price)

###### CODE NOT USED #####

M2 <- lm(log_price ~ accommodates + bathrooms + bedrooms + beds +
           number_of_reviews + review_scores_rating +
           amenity_count + has_wifi + has_kitchen + has_aircon + has_tv +
           property_type + room_type + cleaning_fee,
         data = ModelData)

summary(M2)

###### END OF CODE NOT USED ######

# New model without variables that are not statistically significant or super unique properties

# Aggregating how many of each property type and room type exist...
property_counts <- Airbnb2 %>% count(property_type, sort=TRUE)
print(property_counts,n=35)

room_counts <- Airbnb2 %>% count(room_type, sort=TRUE)
print(room_counts)

####### NOT USED ########

# Vector of property types you want to remove
drop_types <- c("Cave", "Chalet", "Earth House", "Guest suite",
                "In-law", "Island", "Other", "Serviced apartment",
                "Train", "Treehouse", "Vacation home", "Villa", "Yurt", "Tipi", "Tent")

ModelData_M4 <- ModelData %>% filter(property_type %in% keep_types)
M4 <- lm(log_price ~ accommodates + bathrooms + bedrooms + beds +
           number_of_reviews + review_scores_rating +
           amenity_count + has_wifi + has_kitchen + has_aircon + has_tv +
           property_type + room_type,
         data = ModelData_M4)

summary(M4)

numeric_vars_M4 <- ModelData_M4 %>%
  select(accommodates, bathrooms, bedrooms, beds,
         number_of_reviews, review_scores_rating, amenity_count)

round(cor(numeric_vars_M4), 3)


###### END OF NOT USED ########

#Removing beds from the linear model, beds correlated with bedrooms
#An argument could also be made to only pick one of Accomodates and Bedrooms with a correlation of 0.729
M5 <- lm(log_price ~ accommodates + bathrooms + bedrooms +
           number_of_reviews + review_scores_rating +
           amenity_count + has_wifi + has_kitchen + has_aircon + has_tv +
           property_type + room_type,
         data = ModelData)

summary(M5)

numeric_vars_M5 <- ModelData_M4 %>%
  select(accommodates, bathrooms, bedrooms,
         number_of_reviews, review_scores_rating, amenity_count)

round(cor(numeric_vars_M5), 3)

cor_matrix <- round(cor(numeric_vars_M5), 3)

####### NOT USED STARTING HERE ##########

# Create a restricted modeling dataset without those property types
ModelData_M3 <- ModelData %>%
  filter(!(property_type %in% drop_types))

# New multivariable regression M3 (still without cleaning_fee)
M3 <- lm(log_price ~ accommodates + bathrooms + bedrooms + beds +
           number_of_reviews + review_scores_rating +
           amenity_count + has_wifi + has_kitchen + has_aircon + has_tv +
           property_type + room_type,
         data = ModelData_M3)

summary(M3)

#check for multicolinearity

numeric_vars_M3 <- ModelData_M3 %>%
  select(accommodates, bathrooms, bedrooms, beds,
         number_of_reviews, review_scores_rating, amenity_count)

round(cor(numeric_vars_M3), 3)

####### NOT USED ENDING HERE ####


#Training and Testing Split Data
#JY Modifying with new model, 80/20 train/test, dropping beds

n <- nrow(ModelData)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))

Train_M5 <- droplevels(ModelData[train_idx, ])
Test_M5  <- droplevels(ModelData[-train_idx, ])

# 3) Fit M5 on the training data ----------------------------------------
M5_train <- lm(log_price ~ accommodates + bathrooms + bedrooms +
                 number_of_reviews + review_scores_rating +
                 amenity_count + has_wifi + has_kitchen + has_aircon + has_tv +
                 property_type + room_type,
               data = Train_M5)

summary(M5_train)

# 4) Out-of-sample predictions & R^2 -------------------------------------
Pred_M5 <- predict(M5_train, newdata = Test_M5)   # log-price predictions

SSE_M5 <- sum((Test_M5$log_price - Pred_M5)^2)
SST_M5 <- sum((Test_M5$log_price - mean(Train_M5$log_price))^2)
OOS_R2_M5 <- 1 - SSE_M5 / SST_M5
OOS_R2_M5

###### START OF JY CLUSTERING WORK - K MEANS CLUSTERING ########

#One Hot Encoding for property Type start
ModelData$property_type <- factor(ModelData$property_type, levels = c("Apartment", "House", "Condominium", "Townhouse", "Loft"))
prop_ohe <- model.matrix(~property_type -1, data = ModelData)

colnames(prop_ohe) <- sub("^property_type", "prop_", colnames(prop_ohe))

clusterdata <- cbind(ModelData, prop_ohe)
clusterdata$property_type <- NULL

#End One Hot Encoding

#define which columns to run clusters on
feature_cols <- c("log_price", "accommodates", "bathrooms", 
                  "number_of_reviews", "review_scores_rating", 
                  "prop_Apartment", "prop_House", "prop_Condominium", 
                  "prop_Townhouse", "prop_Loft",
                  "amenity_count", "cleaning_fee")

#defining numeric columns for scaling
clusterdata_filtered <- clusterdata[feature_cols]
num_cols <- c("log_price", "accommodates", "bathrooms", "number_of_reviews",
              "review_scores_rating", "amenity_count")

#scaling numeric columns
clusterdata_scaled <- clusterdata_filtered
clusterdata_scaled[num_cols] <- scale(clusterdata_filtered[num_cols])

######ELBOW METHOD########
ks <- 2:15
wss <- sapply(ks, function(k) {
  kmeans(clusterdata_scaled, centers = k, nstart = 20)$tot.withinss
})
plot(ks, wss, type = "b", xlab = "k", ylab = "Total within-cluster SS")
#######END ELBOW METHOD########


#clustering
set.seed(5)
k <- 4
km <- kmeans(clusterdata_scaled, centers = k, nstart = 25)

#summarizing
clusterdata$cluster <- km$cluster
centroids <- km$centers

table(clusterdata$cluster)

cluster_summary <- clusterdata %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    avg_price        = mean(log_price, na.rm = TRUE),
    avg_accommodates = mean(accommodates, na.rm = TRUE),
    avg_bathrooms    = mean(bathrooms, na.rm = TRUE),
    avg_reviews      = mean(number_of_reviews, na.rm = TRUE),
    avg_rating       = mean(review_scores_rating, na.rm = TRUE),
    avg_amenities    = mean(amenity_count, na.rm = TRUE),
    avg_cleaning_fee = mean(cleaning_fee, na.rm = TRUE),
    # property-type mix (proportions)
    prop_apartment   = mean(prop_Apartment, na.rm = TRUE),
    prop_house       = mean(prop_House, na.rm = TRUE),
    prop_condo       = mean(prop_Condominium, na.rm = TRUE),
    prop_townhouse   = mean(prop_Townhouse, na.rm = TRUE),
    prop_loft        = mean(prop_Loft, na.rm = TRUE)
  ) %>%
  arrange(cluster)

print(cluster_summary)


#### CART ######

decision_tree <- rpart(log_price ~ accommodates + bathrooms + bedrooms +
                         amenity_count + property_type + room_type, data = ModelData_M4, method = "anova")
cart_test <- predict(decision_tree, Test_M5)

prp(decision_tree)


#CLUSTERING#

# Data for clustering: size, quality, amenities (no log_price!)
ClusterData <- ModelData_M4 %>%
  select(accommodates, bathrooms, bedrooms,
         number_of_reviews, review_scores_rating,
         amenity_count, has_wifi, has_kitchen, has_aircon, has_tv) %>%
  drop_na()

# Scale numeric features so they are on comparable scales
ClusterMatrix <- scale(ClusterData)

#Distance matrix + hierarchical clustering (Ward’s method)
dist_mat <- dist(ClusterMatrix)
hc <- hclust(dist_mat, method = "ward.D2")

# Dendrogram
plot(hc, labels = FALSE, main = "Airbnb Listings — Hierarchical Clustering")

# Choose number of clusters K by eye (start with 4 as a candidate)
rect.hclust(hc, k = 4, border = "red")

# Add cluster labels from hierarchical clustering
K <- 4   # change if you decide on a different K after looking at dendrogram
ModelData_M3$cluster_hc <- factor(cutree(hc, k = K))

# How many listings per cluster?
count(ModelData_M3, cluster_hc)

# Partial "centroid" info: average features by cluster
ModelData_M3 %>%
  group_by(cluster_hc) %>%
  summarise(
    n_listings         = n(),
    mean_accommodates  = mean(accommodates),
    mean_bedrooms      = mean(bedrooms),
    mean_bathrooms     = mean(bathrooms),
    mean_amenities     = mean(amenity_count),
    wifi_share         = mean(has_wifi),
    kitchen_share      = mean(has_kitchen),
    aircon_share       = mean(has_aircon),
    tv_share           = mean(has_tv),
    mean_reviews       = mean(number_of_reviews),
    mean_score         = mean(review_scores_rating, na.rm = TRUE)
  )