################################################################################
# Analysis of the Chicago Crimes over the last 6 years                         #
# Practical Assignment                                                         #
# DM2+TACD                                                                     #
#                                                                              #
# Author:                                                                     #
# Margarida Cardeano Pinheiro    (up201805012@edu.fc.up.pt)                    #
#                                                                              #
################################################################################

library(tidyverse)
library(lubridate)
library(dplyr)
library(leaflet)
library(sf)
library(gplots)
library(ggplot2)
library(scales)
library(viridis)
library(RColorBrewer)
library(arules)
library(arulesViz)
library(igraph)
library(gridExtra)
library(recommenderlab)
library(dplyr)

################################################################################
# 0. DATA PREPARATION  #########################################################
################################################################################

# Read the dataset
crimes <- read_csv("C:/Users/Diogo Lobo/Desktop/FCUP/Mestrado/MestradoCC/Data Mining II/Pratical_Assignment/Crimes___2017_to_Present/Crimes___2017_to_Present.csv")
head(crimes)

# Remove spaces from column names
colnames(crimes) <- gsub(" ","_", colnames(crimes))
colnames(crimes) <- tolower(colnames(crimes))

# Remove columns 'fbi_code' and 'updated_on'
crimes <- subset(crimes, select = -c(fbi_code, updated_on))

# Remove rows with NA or empty values
rowsbef <- nrow(crimes) # number of examples before removing NA
crimes <- crimes %>% drop_na()
rowsaft <- nrow(crimes) # number of examples after removing NA

# Verify if there are duplicated examples of 'id' and 'case_number'
sum(duplicated(crimes$id))
sum(duplicated(crimes$case_number))

# Remove rows with duplicated 'case_number'
crimes <- crimes[!duplicated(crimes$case_number), ]

# Convert 'date' format and create new columns: 'time_of_day', 'quarter', and 'week_day'
crimes$date <- ymd_hms(crimes$date)

crimes$time_of_day <- ifelse(hour(crimes$date)>=6 & hour(crimes$date)<=11 & minute(crimes$date)>=0 & seconds(crimes$date)>=0, "MORNING",
                             ifelse(hour(crimes$date)>=12 & hour(crimes$date)<=16 & minute(crimes$date)>=0 & seconds(crimes$date)>=0, "AFTERNOON",
                                    ifelse(hour(crimes$date)>=17 & hour(crimes$date)<=19 & minute(crimes$date)>=0 & seconds(crimes$date)>=0, "EVENING", "NIGHT")))

crimes$quarter <- ifelse(month(crimes$date)>=1 & month(crimes$date)<=3, 1,
                         ifelse(month(crimes$date)>=4 & month(crimes$date)<=6, 2,
                                ifelse(month(crimes$date)>=7 & month(crimes$date)<=9, 3, 4
                                )))

crimes$week_day <- (weekdays(crimes$date))

# Subset the data frame to exclude crimes that occurred in April 2023
crimes <- subset(crimes, !(format(crimes$date, '%m')=='04' & format(crimes$date, '%Y')=='2023'))

# Final number of rows
rowsfinal <- nrow(crimes)

# Organize categories of 'primary_type' and 'location_description'
crimes <- subset(crimes, primary_type!='NON-CRIMINAL')
crimes <- subset(crimes, primary_type!='NON-CRIMINAL (SUBJECT SPECIFIED)')

crimes$primary_type[crimes$primary_type == 'CRIM SEXUAL ASSAULT'] <- 'CRIMINAL SEXUAL ASSAULT'
crimes[str_detect(crimes$primary_type, '^OTHER'),]$primary_type <- 'OTHER'

crimes[str_detect(crimes$location_description, '^AIRPORT'),]$location_description <- 'AIRPORT'
crimes[str_detect(crimes$location_description, '^AUTO'),]$location_description <- 'AUTO DEALERSHIP'
crimes[str_detect(crimes$location_description, '^BARBER'),]$location_description <- 'BARBERSHOP/SALON'
crimes[str_detect(crimes$location_description, '^BOAT'),]$location_description <- 'BOAT'
crimes[str_detect(crimes$location_description, '^CHA '),]$location_description <- 'CHA'
crimes[str_detect(crimes$location_description, '^CHURCH'),]$location_description <- 'PLACE OF WORSHIP'
crimes[str_detect(crimes$location_description, 'TAVERN'),]$location_description <- 'TAVERN'
crimes[str_detect(crimes$location_description, 'STORE') | str_detect(crimes$location_description, ' SHOP'),]$location_description <- 'STORE'
crimes[str_detect(crimes$location_description, '^POLICE FACILITY'),]$location_description <- 'POLICE FACILITY'
crimes[str_detect(crimes$location_description, 'RESIDEN') | str_detect(crimes$location_description, 'HOUSE'),]$location_description <- 'RESIDENCE'
crimes[str_detect(crimes$location_description, 'SCHOOL'),]$location_description <- 'SCHOOL'
crimes[str_detect(crimes$location_description, '^PARKING LOT'),]$location_description <- 'PARKING LOT'
crimes[str_detect(crimes$location_description, 'STADIUM'),]$location_description <- 'STADIUM'
crimes[str_detect(crimes$location_description, '^VACANT LOT'),]$location_description <- 'VACANT LOT'
crimes[str_detect(crimes$location_description, '^COLLEGE'),]$location_description <- 'COLLEGE'
crimes[str_detect(crimes$location_description, '^CTA'),]$location_description <- 'CTA'
crimes[str_detect(crimes$location_description, 'RIDE') | str_detect(crimes$location_description, 'TAXI'),]$location_description <- 'TAXI/RIDE SERVICE'
crimes[str_detect(crimes$location_description, 'VEHICLE') & str_detect(crimes$location_description, 'COMMERCIAL'),]$location_description <- 'COMMERCIAL VEHICLE'
crimes[str_detect(crimes$location_description, '$THEATER'),]$location_description <- 'THEATER'
crimes[str_detect(crimes$location_description, '^NURSING'),]$location_description <- 'NURSING HOME'
crimes[str_detect(crimes$location_description, '^MEDICAL'),]$location_description <- 'MEDICAL/DENTAL OFFICE'
crimes[str_detect(crimes$location_description, '^LAKE'),]$location_description <- 'LAKE/RIVER/SEA'
crimes[str_detect(crimes$location_description, 'HOTEL') | str_detect(crimes$location_description, 'MOTEL'),]$location_description <- 'HOTEL/MOTEL'
crimes[str_detect(crimes$location_description, '^HIGHWAY'),]$location_description <- 'HIGHWAY'
crimes[str_detect(crimes$location_description, '^GOVERNMENT'),]$location_description <- 'GOVERNMENT BUILDING'
crimes[str_detect(crimes$location_description, '^GAS'),]$location_description <- 'GAS STATION'
crimes[str_detect(crimes$location_description, '^GARAGE'),]$location_description <- 'GARAGE'
crimes[str_detect(crimes$location_description, '^FACTORY'),]$location_description <- 'FACTORY'
crimes[str_detect(crimes$location_description, 'TRUCK'),]$location_description <- 'TRUCK'
crimes[str_detect(crimes$location_description, 'BUSINESS'),]$location_description <- 'OFFICE'
crimes[str_detect(crimes$location_description, 'HOSPITAL'),]$location_description <- 'HOSPITAL'
crimes[str_detect(crimes$location_description, 'OTHER'),]$location_description <- 'OTHER'


# Identify the categories to change
loc_to_change <- crimes$location_description %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  names() %>%
  tail(-20)

desc_to_change <- crimes$description %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  names() %>%
  tail(-20)

# Update the dataframe with 'OTHER' category
crimes$location_description[crimes$location_description %in% loc_to_change] <- 'OTHER'
crimes$description[crimes$description %in% desc_to_change] <- 'OTHER'


# Final categories
crimes %>% group_by(primary_type) %>% summarise(count=n()) %>% print(n=100)
crimes %>% group_by(location_description) %>% summarise(count=n()) %>% print(n=100)

# Save the hidden set to a CSV file
hidden <- subset(crimes, year==2023)
write_csv(hidden, "chicago_crimes_hidden.csv")

# Save the train/test set to a CSV file
traintest <- subset(crimes, year!=2023)
write_csv(traintest, "chicago_crimes_traintest.csv")

# Save the complete clean dataset to a CSV file
write_csv(crimes, "chicago_crimes_clean.csv")

################################################################################
# 1. Exploratory Data Analysis  ################################################
################################################################################

plot_directory <- "C:/Users/maria/Documents/2º SEMESTRE/TACD/trabalho/plots"

# Function to save a plot
save_plot <- function(plot, filename) {
  jpeg(file.path(plot_directory, filename))
  print(plot)
  dev.off()
}


# Visualization 1 - Crime and Arrest evolution
#---------------------------------------------

# Data preparation for arrests
chicago_arrests_monthly <- traintest %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate(YearMonth = floor_date(date, "month")) %>%
  filter(arrest == TRUE) %>%
  count(YearMonth)

# Data preparation for crimes
chicago_crimes_monthly <- traintest %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate(YearMonth = floor_date(date, "month")) %>%
  count(YearMonth) # this creates the n for the plot

# Create a sequence of months
start_date <- min(chicago_crimes_monthly$YearMonth)
end_date <- max(chicago_crimes_monthly$YearMonth)
month_labels <- seq(from = start_date, to = end_date, by = "1 month")

# Create the plot
ggplot() +
  geom_line(data = chicago_arrests_monthly, aes(x = YearMonth, y = n, color = "Arrests")) +
  geom_line(data = chicago_crimes_monthly, aes(x = YearMonth, y = n, color = "Crimes")) +
  labs(title = "Monthly evolution of Crimes and Arrests",
       x = "Month",
       y = "Count",
       color = "Event") +
  scale_x_date(breaks = month_labels, labels = scales::date_format("%Y-%m")) +
  scale_color_manual(values = c("Arrests" = "coral", "Crimes" = "slateblue1")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Visualization 2 - Theft crimes per ward (map)
#----------------------------------------------

# Considering that 'crimes' is a data frame with latitude and longitude columns
# Show on a map where homicides took place

filtered_crimes <- traintest %>% filter(`primary_type` == "THEFT")

# Create leaflet map
map <- leaflet(filtered_crimes) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions(),
             popup = ~paste("Primary Type: ", `primary_type`, "<br>",
                            "Date: ", format(as.Date(date), "%Y-%m-%d"), "<br>",
                            "Description: ", description, "<br>",
                            "Location Description: ", `location_description`, "<br>")
  )

# Load the shapefile data for Chicago wards
wards <- st_read("C:/Users/Diogo Lobo/Desktop/FCUP/Mestrado/MestradoCC/Data Mining II/Pratical_Assignment/wards.geojson")

# Add the wards layer to the map
map <- map %>%
  addPolygons(data = wards, fillColor = "#774983", fillOpacity = 0.4, color = "#4E3056", weight = 1,
              label = paste("WARD:", as.character(wards$ward)))  # Add the ward number as a label

# Show map
map



# Visualization 3 - Battery crimes per ward (map)
#------------------------------------------------

filtered_crimes <- traintest %>% filter(`primary_type` == "BATTERY")

# create leaflet map
map <- leaflet(filtered_crimes) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions(),
             popup = ~paste("Primary Type: ", `primary_type`, "<br>",
                            "Date: ", format(as.Date(date), "%Y-%m-%d"), "<br>",
                            "Description: ", description, "<br>",
                            "Location Description: ", `location_description`, "<br>")
  )

# Add the wards layer to the map
map <- map %>%
  addPolygons(data = wards, fillColor = "#774983", fillOpacity = 0.4, color = "#4E3056", weight = 1,
              label = paste("WARD:", as.character(wards$ward)))  # Add the ward number as a label


# Display the map
map



# Visualization 4 - Homicides per ward (map)
#-------------------------------------------

filtered_crimes <- traintest %>% filter(`primary_type` == "HOMICIDE")

# create leaflet map
map <- leaflet(filtered_crimes) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions(),
             popup = ~paste("Primary Type: ", `primary_type`, "<br>",
                            "Date: ", format(as.Date(date), "%Y-%m-%d"), "<br>",
                            "Description: ", description, "<br>",
                            "Location Description: ", `location_description`, "<br>")
  )

# Add the wards layer to the map
map <- map %>%
  addPolygons(data = wards, fillColor = "#774983", fillOpacity = 0.4, color = "#4E3056", weight = 1,
              label = paste("WARD:", as.character(wards$ward)))  # Add the ward number as a label

# Display the map
map



# Visualization 5 - Correlation Heatmap of numerical variables
#-------------------------------------------------------------

save_plot <- function(plot, filename) {
  jpeg(file.path(plot_directory, filename))
  print(plot)
  dev.off()
}


heatMap <- function(traintest) {
  # Filter numeric columns
  numeric_cols <- sapply(traintest, is.numeric)
  df_numeric <- traintest[, numeric_cols]
  
  # Create correlation matrix
  corr <- cor(df_numeric)
  
  # Plot heatmap with correlation values
  heatmap.2(corr, col = colorRampPalette(c("slateblue2", "lightyellow", "#FF9966"))(100),
            main = "Correlation Heatmap",
            xlab = "Variables",
            ylab = "Variables",
            cex.axis = 0.8,
            cex.lab = 0.8,
            key = TRUE,
            keysize = 1.5,
            density.info = "none",
            trace = "none",
            dendrogram = "none")
}

# Call the heatMap function with your data frame
heatMap(traintest)



# Visualization 6 - Number of crimes per primary type
#----------------------------------------------------

# Count number of crimes per type
crime_counts <- count(traintest, primary_type)

# Create the plot
ggplot(crime_counts, aes(x = reorder(primary_type, n), y = n, fill = n)) +
  geom_col() +
  scale_fill_gradient(low = "slateblue1", high = "#FF9966") +
  labs(title = "Number of Crimes by Type",
       x = "Crime Type",
       y = "Number of Crimes") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  theme(legend.text = element_text(angle = 0, hjust = 1))



# Visualization 7 - Comparative number of occurrences of the top 5 crime types
#----------------------------------------------------------------------------

# Calculate the total number of occurrences of each primary type over the years
crimes_by_type <- traintest %>%
  group_by(primary_type) %>%
  summarize(total = n()) %>%
  arrange(desc(total))

# Select the top N primary types with the highest number of occurrences
N <- 5
top_types <- head(crimes_by_type$primary_type, N)

# Filter the original data frame to keep only the rows corresponding to the selected primary types
new_filtered_crimes <- traintest %>%
  filter(primary_type %in% top_types) %>%
  filter(ward == 42)

# Group the filtered data frame by primary type and year, and count the number of occurrences
crimes_by_year <- new_filtered_crimes %>%
  mutate(year = year(date)) %>%
  group_by(primary_type, year) %>%
  summarize(count = n())

custom_colors <- c("#4E3056", "slateblue2", "#FFCDE1", "#FF8484", "#FF9966")

# Plot the results using ggplot
ggplot(crimes_by_year, aes(x = year, y = count, color = primary_type)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = custom_colors) +
  labs(title = "Top 5 Crime Types by Occurrences",
       x = "Year",
       y = "Number of Occurrences",
       color = "Crime Type") +
  theme_minimal()



# Visualization 8 - Number of crimes (>15000) per location
#---------------------------------------------------------

# Plot the number of crimes by location
crimes_by_location <- table(traintest$location_description)

# Convert the table to a data frame
crimes_data <- data.frame(Location = names(crimes_by_location), Count = as.numeric(crimes_by_location))

# Sort the data frame by count in descending order
crimes_data <- crimes_data %>%
  arrange(desc(Count))

crimes_data <- crimes_data %>%
  filter(Count > 30000)

# Generate a color palette with the number of unique locations
colors <- magma(nrow(crimes_data))

# Plot the data
ggplot(crimes_data, aes(x = Count, y = fct_reorder(Location, Count), fill = Location)) +
  geom_bar(stat = "identity", color = "slateblue1") +
  scale_fill_manual(values = rep("slateblue1", nrow(crimes_data)), guide = FALSE) +
  labs(title = "Number of Crimes by Location",
       x = "Number of Crimes",
       y = "Crime Location") +
  scale_x_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, hjust = 0.5))



# Visualization 9 - Number of crimes by ward and year
#----------------------------------------------------

# Subset the data to include only the relevant columns
crimes_subset <- traintest %>% 
  select(date, ward)

# Convert date to year
crimes_subset$year <- year(ymd_hms(crimes_subset$date))

# Create a contingency table of the data

crime_table <- table(crimes_subset$ward, crimes_subset$year)

# Convert the table to a data frame
crime_data <- data.frame(expand.grid(Ward = row.names(crime_table), Year = colnames(crime_table)), Count = as.vector(crime_table))

# Plot the data as a heatmap
ggplot(crime_data, aes(x = Year, y = Ward)) +
  geom_tile(aes(fill = Count)) +
  scale_fill_gradient(low = "white", high = "slateblue2") +
  labs(title = "Number of Crimes by Ward and Year",
       x = "Year",
       y = "Ward") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# Visualization 10 - Number of crimes by days of the week
#--------------------------------------------------------

# Convert date column to date index
traintest$index <- as.Date(traintest$date)
traintest <- traintest[order(traintest$index),]

# Define colors and days of the week
colors <- c('slateblue2')

counts <- table(crimes$week_day)
plot10 <- barplot(counts, horiz = TRUE, col = colors, xlab = "Number of crimes", ylab = "Days of the week",
        main = "Number of crimes by day of the week")



# Visualization 11 - Number of Crimes by Month of the Year
#--------------------------------------------------------

# Count number of crimes per month
crime_counts <- data.frame(table(month(traintest$date)))

# Create the plot
ggplot(crime_counts, aes(x = Var1, y = Freq, fill = Freq)) +
  geom_col() +
  scale_fill_gradient(low = "#D8D1FF", high = "slateblue2") +
  labs(title = "Number of Crimes by Month of the Year",
       x = "Months of the Year",
       y = "Number of Crimes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  coord_flip() +
  scale_y_continuous(labels = comma)



# Visualization 12 - Arrests per location
#----------------------------------------

arrests_by_location <- traintest %>%
  filter(arrest == TRUE) %>%
  filter(year(date) < 2020) %>%
  group_by(location_description) %>%
  summarize(arrest_count = sum(arrest)) %>%
  filter(arrest_count > 5000) %>%
  arrange(desc(arrest_count))

ggplot(arrests_by_location, aes(x = reorder(location_description, -arrest_count), y = arrest_count)) +
  geom_bar(stat = "identity", fill = "slateblue1") +
  labs(x = "Location Description", y = "Arrest Count", title = "Arrests by Location Description") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Visualization 13 - Arrests per ward (>5000 arrests)
#------------------------------------

arrests_by_location <- traintest %>%
  filter(arrest == TRUE) %>%
  group_by(ward) %>%
  summarize(arrest_count = sum(arrest)) %>%
  filter(arrest_count > 5000) %>%
  arrange(desc(arrest_count))

ggplot(arrests_by_location, aes(x = reorder(ward, -arrest_count), y = arrest_count)) +
  geom_bar(stat = "identity", fill = "slateblue1") +
  labs(x = "Location Description", y = "Arrest Count", title = "Arrests by Ward") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Visualization 14 - Arrests per time of day
#-------------------------------------------

arrests_by_time <- traintest %>%
  filter(arrest == TRUE) %>%
  group_by(time_of_day) %>%
  summarize(arrest_count = sum(arrest))

ggplot(arrests_by_time, aes(x = time_of_day, y = arrest_count)) +
  geom_bar(stat = "identity", fill = "slateblue1") +
  labs(x = "Time of Day", y = "Arrest Count", title = "Arrests by Time of Day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Visualization 15 - Arrests per quarter
#---------------------------------------

arrests_by_quarter <- traintest %>%
  filter(arrest == TRUE) %>%
  group_by(quarter) %>%
  summarize(arrest_count = sum(arrest))

ggplot(arrests_by_quarter, aes(x = quarter, y = arrest_count)) +
  geom_bar(stat = "identity", fill = "slateblue1") +
  labs(x = "Quarter", y = "Arrest Count", title = "Arrests by Quarter") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Visualization 16 - Arrests per year
#------------------------------------

arrests_by_year <- traintest %>%
  filter(arrest == TRUE) %>%
group_by(year) %>%
  summarize(arrest_count = sum(arrest)) %>%
  complete(year = min(year):max(year), fill = list(arrest_count = 0))

ggplot(arrests_by_year, aes(x = year, y = arrest_count)) +
  geom_bar(stat = "identity", fill = "slateblue1") +
  labs(x = "Year", y = "Arrest Count", title = "Arrests by Year") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(arrests_by_year$year))



# Visualization 17 - Arrests per month
#-------------------------------------

arrests_by_month <- traintest %>%
  filter(arrest == TRUE) %>%
  mutate(month = lubridate::month(date, label = TRUE)) %>%
  group_by(month) %>%
  summarize(arrest_count = n())

ggplot(arrests_by_month, aes(x = month, y = arrest_count)) +
  geom_bar(stat = "identity", fill = "slateblue1") +
  labs(x = "Month", y = "Arrest Count", title = "Arrests by Month") +
  scale_x_discrete(labels = month.abb) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Visualization 18 - Arrest vs Non-Arrest by Crime Type
#------------------------------------------------------

crime_counts <- traintest %>%
  group_by(primary_type) %>%
  summarize(total_count = n()) %>%
  filter(total_count > 70000) %>%
  inner_join(crimes, by = "primary_type")

arrest_counts <- crime_counts %>%
  group_by(primary_type, arrest) %>%
  summarize(count = n())

ggplot(arrest_counts, aes(x = primary_type, y = count, fill = arrest)) +
  geom_bar(stat = "identity") +
  labs(x = "Crime Type", y = "Arrest Count", title = "Arrest vs Non-Arrest by Crime Type", fill = "Outcome") +
  scale_fill_manual(values = c("TRUE" = "slateblue", "FALSE" = "#D8D1FF"), labels = c("Arrest", "Non-Arrest")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))




################################################################################
# 2. ASSOCIATION RULES  ########################################################
################################################################################

# 1st OPTION FOR ASSOCIATION RULES:
chicago_crimes_clean <- traintest %>% select(primary_type, location_description, time_of_day, arrest)


# Convert Primary Type, Location Description and Arrest columns to factors
chicago_crimes_clean$primary_type <- as.factor(chicago_crimes_clean$primary_type)
chicago_crimes_clean$location_description <- as.factor(chicago_crimes_clean$location_description)
chicago_crimes_clean$arrest <- as.factor(chicago_crimes_clean$arrest)
chicago_crimes_clean$time_of_day <- as.factor(chicago_crimes_clean$time_of_day)

#TransList <- split(chicago_crimes_clean$primary_type, chicago_crimes_clean$location_description)
#TransMat <- as(TransList, "transactions")
#summary(TransMat)

# Generate transaction dataset
trans <- as(chicago_crimes_clean, "transactions")
itemFrequencyPlot(trans,topN=15,col=brewer.pal(8,'Pastel2'),cex.names=0.7,main='Relative Item Frequency Plot',type="relative",ylab="Item Frequency (Relative)")

# Generate association rules with a focus on Arrest
rules <- apriori(trans, parameter = list(supp = 0.03, conf = 0.2))
#appearance = list(default="lhs", rhs="arrest"),
#control = list(verbose=F))

# Inspect the rules
inspect(rules)
plot(rules)
plot(rules, method="grouped")

# Generate recommendations for a specific item (Two examples)
item <- "primary_type=NARCOTICS"
item_rules <- subset(rules, subset = lhs %in% item)
recs <- item_rules@rhs
# Print the recommendations
inspect(recs)

item <- "location_description=SIDEWALK"
item_rules <- subset(rules, subset = lhs %in% item)
recs <- item_rules@rhs
# Print the recommendations
inspect(recs)


# Visualize the rules as a graph
#graph <- plot(rules, method = "graph")

## graphs work better with very few rules
#subrules2 <- sample(rules, 20)
plot(rules, method="graph")
plot(rules, method="graph", engine = "htmlwidget")



################################################################################
# 3. LINK ANALYSIS  ############################################################
################################################################################

# The edges represent the connections between different wards 

# based on the occurrence of crimes of a specific type within a given time interval.
# Set the desired time interval
#start_date <- as.Date("2022-01-01")
#end_date <- as.Date("2022-12-31")

# Select the columns of interest
selected_columns <- c("primary_type", "ward")

# Filter the dataset and selected columns
filtered_crimes <- traintest %>%
  #filter(date >= start_date & date <= end_date) %>%
  select(selected_columns)

# Create an edge list based on the filtered dataset
edge_list <- filtered_crimes %>%
  distinct() %>%
  group_by(ward) %>%
  summarise(count = n())

# Create a graph from the edge list
graph <- graph_from_data_frame(edge_list)

# Apply a layout algorithm to distribute the vertices
layout <- layout_with_fr(graph)

# Plot the graph with the layout
plot(graph, layout = layout, vertex.label = V(graph)$name)

degree(graph)
closeness(graph)
betweenness(graph)

# Perform community structure analysis
communities <- cluster_walktrap(graph)
membership <- communities$membership

# Visualize the communities
plot(graph, vertex.color = membership, vertex.label = V(graph)$name)

# Interpret the communities
community_labels <- unique(membership)
for (i in community_labels) {
  community_wards <- which(membership == i)
  print(paste("Community", i, ":"))
  print(community_wards)
}

# Filter the crime dataset for each community
community_1_crimes <- traintest %>% filter(ward %in% c(1, 4, 5, 11, 15, 16, 18, 20, 24, 26, 28, 30, 35, 37, 39, 41, 45, 46))
community_2_crimes <- traintest %>% filter(ward %in% c(13, 25, 44))
community_3_crimes <- traintest %>% filter(ward %in% c(2, 3, 6, 8, 9, 12, 17, 21, 29, 34, 36, 38, 42, 43, 49))
community_4_crimes <- traintest %>% filter(ward %in% c(7, 10, 14, 19, 22, 23, 27, 31, 32, 33, 40, 47, 48, 50))


# Analyze crime characteristics for each community and plot the results
analyze_community <- function(community_crimes, community_label, community_color) {
  cat("Community", community_label, ":\n")
  
  # Crime type distribution
  primary_type_counts <- table(community_crimes$primary_type)
  cat("Primary Crime Types:\n")
  print(primary_type_counts)
  
  # Plot crime type distribution
  plot_primary_type <- ggplot(data = community_crimes, aes(x = primary_type)) +
    geom_bar(fill = community_color) +
    labs(title = paste("Crime Type Distribution - Community", community_label),
         x = "Primary Crime Type",
         y = "Count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(plot_primary_type)
}

# Analyze crime patterns for each community and create the plots
plot_community_1 <- analyze_community(community_1_crimes, "1", "orange")
plot_community_2 <- analyze_community(community_2_crimes, "2", "darkgreen")
plot_community_3 <- analyze_community(community_3_crimes, "3", "yellow")
plot_community_4 <- analyze_community(community_4_crimes, "4", "skyblue")


# Arrange the plots in a grid
grid.arrange(plot_community_1, plot_community_2, plot_community_3, plot_community_4, ncol = 4)


################################################################################
# 4. Recommendations  ############################################################
################################################################################


# Subset the dataset with relevant columns (ward and primary type)
subset_data <- traintest[, c("ward", "primary_type")]

# Convert the dataset to a user-item matrix
user_item_matrix <- subset_data %>%
  dcast(ward ~ `primary_type`, value.var = "primary_type", fun.aggregate = length, fill = 0)

# Convert the user-item matrix to a recommender object
recommender_data <- as(user_item_matrix, "binaryRatingMatrix")

# Generate the similarity-based recommendations using recommenderlab
recommender_model <- Recommender(recommender_data, method = "UBCF")
recommendations <- predict(recommender_model, recommender_data, n = 7, type = "topNList")
getList(recommendations)

###
# Example
# Generate the recommendations for a specific user (ward)
target_user <- 42  

user_recommendations <- recommendations@items[[target_user]]

# Print the recommended items
print("Recommended items:")
print(user_recommendations)
###

# Create the Visualization for the Recommendations
# Create a data frame for storing the recommendations
recommendations_df <- data.frame(Ward = integer(), Recommendations = character(), stringsAsFactors = FALSE)

# Loop through each ward
for (ward_id in unique(subset_data$ward)) {
  # Generate recommendations for the ward
  ward_recommendations <- as.character(recommendations@items[[ward_id]])
  
  # Add the recommendations to the data frame
  recommendations_df <- rbind(recommendations_df, data.frame(Ward = ward_id, Recommendations = ward_recommendations))
}


# Create a mapping of primary types to numeric values
primary_type_mapping <- unique(traintest$primary_type)

# Convert numeric recommendations to primary types
recommendations_df$Recommendations <- factor(recommendations_df$Recommendations,
                                             levels = 1:length(primary_type_mapping),
                                             labels = primary_type_mapping)

# Plot the recommendations
ggplot(recommendations_df, aes(x = factor(Ward), fill = Recommendations)) +
  geom_bar() +
  labs(title = "Recommendations for Wards", x = "Ward", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")
