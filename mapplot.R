library(tidyverse)
library(ggplot2)
library(sf)
library(lme4)

# read Boston data
listings_boston <- read.csv("listings 2.csv")
neighbourhood_boston <- st_read("neighbourhoods.geojson")

# add a column of city
listings_boston$city <- "Boston"

# combine the three data
col_need <- c("host_id", "city", "neighbourhood_cleansed", "latitude", 
              "longitude", "room_type", "accommodates",
              "bathrooms_text", "bedrooms", "beds", "price")
data_bind <- bind_rows(listings_boston)[,col_need]

# deal with bedrooms and bedrooms_text
data_clean <- data_bind %>% separate(
  col = `bathrooms_text`,
  into = c("bathroom_numbers", "bathroom_type"),
  sep = " ",
  fill = "right"
)

# clean the data
# deal with half-bathroom
data_clean[which(data_clean$bathroom_type == "baths"), "bathroom_type"] <- "bath"
row_halfbath <- which(data_clean$bathroom_type == "half-bath")
data_clean[row_halfbath,"bathroom_type"] <- data_clean[row_halfbath,"bathroom_numbers"]
data_clean[row_halfbath,"bathroom_numbers"] <- 0.5
data_clean$bathroom_type <- sapply(data_clean$bathroom_type,tolower)

row_halfbath2 <- which(data_clean$bathroom_numbers == "Half-bath")
data_clean[row_halfbath2,"bathroom_type"] <- data_clean[row_halfbath2,"bathroom_numbers"]
data_clean[row_halfbath2,"bathroom_numbers"] <- 0.5
data_clean$bathroom_numbers <- sapply(data_clean$bathroom_numbers, as.numeric)

# deal with other numeric variables
data_clean$accommodates <- sapply(data_clean$accommodates, as.numeric)
data_clean$price <- gsub("\\$", "", data_clean$price)
data_clean[,10:12] <- sapply(data_clean[,10:12], as.numeric)
names(data_clean)[3]<-paste("neighbourhood")

# data cleaning finished

# pool with neighbourhoods
col2 <- c("city", "neighbourhood", "latitude", "longitude","price")
data_boston <- data_clean[which(data_clean$city == "Boston"),col2]

ggplot()+
  geom_sf(data=neighbourhood_boston,colour='black',fill=NA)+
  geom_point(mapping = aes(x = longitude, y = latitude, 
                            color = price, size = price),data = data_boston)+
  scale_colour_gradientn(colors = c(rev(RColorBrewer::brewer.pal(10, "RdBu")))) + 
  scale_size_continuous(range=c(0.2,2))+
  labs(title = "Boston ")
  theme_bw()
