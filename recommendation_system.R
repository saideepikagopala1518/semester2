setwd("./Desktop/gdm_project")
options(stringsAsFactors = FALSE)
library(jsonlite)
library(plyr)
library(dplyr)
library(igraph)

### Author : Sai Deepika Gopala
### Data Reading ###

business <- readLines("dataset/business.json")
business <- ldply(lapply(business, function(x) t(unlist(fromJSON(x)[c("business_id", "name", "categories")]))))
business$categories_all <- do.call(paste, c(business[, 3:ncol(business)], sep=";"))
business[, 3:(ncol(business)-1)] <- NULL

#user <- readLines("dataset/user.json")
#user <- ldply(lapply(user, function(x) t(unlist(fromJSON(x)[c("user_id", "name")]))))

review <- readLines("dataset/review.json", n = 1000000)
review <- ldply(lapply(review, function(x) t(unlist(fromJSON(x)[c("user_id", "business_id", "stars")]))))
review$stars <- as.numeric(review$stars)

### Data Preprocessing ###

#include only restaurants for analysis
review <- left_join(review, business, by = "business_id")
colnames(review)[4:5] <- c("business_name", "business_categories")
review <- review[grepl("Restaurants", review$business_categories), ]

review_count_by_user <- review %>% group_by(user_id) %>% summarise(user_count = n())
review_count_by_business <- review %>% group_by(business_id) %>% summarise(business_count = n())
#remove business nodes with low indegree
review <- left_join(review, review_count_by_business, by = "business_id")
review <- review %>% filter(business_count > 2)
#remove user nodes with low outdegree
review <- left_join(review, review_count_by_user, by = "user_id")
review <- review %>% filter(user_count > 2)

#include edges with high rankings (means user actually likes business)
review <- review %>% filter(stars >= 4)

#find biggest weakly connected component of the graph
g <- graph_from_data_frame(review[, 1:2], directed = FALSE)
g_components <- components(g)
biggest_component <- subgraph(g, which(g_components$membership == which.max(g_components$csize)))

#filter out users and vertices not in biggest weakly connected component
biggest_component_vertices <- V(biggest_component)$name
review <- review %>% filter(user_id %in% biggest_component_vertices | business_id %in% biggest_component_vertices)
review <- review[, 1:4]
