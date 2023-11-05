

rm(list=ls())


library(readr)
library(rvest)
library(dplyr)
library(tmaptools)
library(ggmap)  
library(leaflet)
library(jsonlite)
library(sf)

### LOAD DATASET ###
denver <- read_csv("/Users/jackconnors/Downloads/Denver_Loc.csv", skip=1)

register_google(key = 'AIzaSyDW8jvKkjRs5ef2MMiB5Es5pLk23TPpj7U')

geocoded_denver <- geocode(denver$Address, output = "latlona", source = "google", 
                                key = "AIzaSyDW8jvKkjRs5ef2MMiB5Es5pLk23TPpj7U")

### BIND GEO-CODES W DENVER DF ###
denver_geo <- denver %>% 
  cbind(geocoded_denver)

#geocoded_denver <- geocode(denver_na$Address, output = "latlona", source = "google", 
#key = "AIzaSyDW8jvKkjRs5ef2MMiB5Es5pLk23TPpj7U")

### CHECK NAs IN GEO-CODED DATA ###
unlist(colSums(is.na(denver_geo)))

### CLUSTER LOCATIONS ###
###### K MEANS CLUSTERING ########
# Standardize latitude and longitude
scaled_data <- scale(denver_geo[, c("lat", "lon")])

sum(is.na(scaled_data))

# Define the number of clusters (you can adjust this)
num_clusters <- 5

# Perform K-Means clustering
kmeans_result <- kmeans(scaled_data, centers = num_clusters)

# Add cluster assignments to the master dataset
denver_geo$cluster <- kmeans_result$cluster


### ONLY INCLUDE ADDRESSES WHERE WE HAVE QUERIES ###
#denver_filtered <- denver_geo %>% 
  #filter(Address %in% geocoded_data$address)

### MASTER DATA FILE GOOGLE SHHET + LONGITUDE + LATITUDE ###
#combined_data <- cbind(geocoded_data, denver_filtered)

## cleaned_denver <- cleaned_data[-19, ] check address = NA

# Rename the "Address" column to "query" to match the geocoded data
#colnames(combined_data)[colnames(combined_data) == "Address"] <- "query"

library(htmltools)

# Create the leaflet map with circle markers using the cleaned data
# Define a color palette based on "Lead" levels
lead_colors <- colorFactor(
  palette = "Set1",  # You can choose different built-in palettes
  domain = denver_geo$Lead)  # Your variable of interest (Lead)


unique(denver_geo$Status)  
# Create the Leaflet map
# Define a color palette for different types
# Define a function to group similar types
# Define a color palette for each category
color_palette <- c(
  "lost" = "red",
  "C1" = "darkblue",
  "partner" = "purple",
  "L2" = "blue",
  "L1" = "orange",
  "Stalled" = "brown",
  "Lost" = "red",
  "L0" = "black",
  "stalled" = "brown")

# Create a new column with corresponding colors
denver_geo$status_Color <- color_palette[denver_geo$Status]
# Check the unique values in the new "Type_Color" column


# Create the leaflet map
# Create the leaflet map
mymap <- leaflet() %>%
  setView(lng = -104.9847, lat = 39.7392, zoom = 11) %>%  # Set the initial map view
  addTiles() %>%
  addCircleMarkers(
    data = denver_geo,
    lat = ~lat,
    lng = ~lon,
    radius = 5,
    color = ~status_Color,  # Use the Type_Color column for coloring
    fillOpacity = 0.7,
    popup = ~paste("<strong>Name:</strong>", Name, "<br><strong>Status:</strong>", Status, "<br><strong>Shop Category:</strong>", POC)
  )

write_csv(denver_geo, "denver_geo.csv")

unique(denver_geo$Lead)

#### A1 ####
a1 <- denver_geo %>% 
  filter(Lead==1, `Type(A,B,C)`=="A") %>% 
  select(Name, status, `Decision Maker Email`, POC, Notes, cluster,`Last PoC Date`) %>% 
  arrange(`Last PoC Date`)

### ALL OTHER LEADS ###
leads <- denver_geo %>% 
  filter(Lead %in% c(2,3), `Type(A,B,C)`%in% c("B", "C")) %>% 
  select(Name, status, `Decision Maker Email`, POC, Notes, cluster,`Last PoC Date`) %>% 
  arrange(`Last PoC Date`)

### PARTNERS ###
partners <- denver_geo %>% 
  filter(status=="partner") %>% 
  select(Name, status, `Decision Maker Email`, POC, Notes, cluster,`Last PoC Date`) %>% 
  arrange(`Last PoC Date`)

### break down of yes and nos 
status_by_type <- denver_geo %>% 
  group_by(status, Type) %>% 
  summarize(count=n())

denver_geo %>% 
  ggplot() +
  geom_col(aes(Type, status, fill=status)) +
  theme_bw()

status_count <- denver_geo %>% 
  group_by(status, Type) %>% 
  count() %>% 
  arrange(desc(n))

### TOP TYPES OF RESTAURANTS ###
head(status_count)

# Create a bar plot
ggplot(status_count, aes(x = status, y = n, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Status Count by Type",
       x = "Status",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
