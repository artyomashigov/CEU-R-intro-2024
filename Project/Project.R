# import packages
library(data.table)
library(ggplot2)



# import data, we use fread to get a data.table ----
locations = fread('https://raw.githubusercontent.com/artyomashigov/CEU-R-intro-2024/main/Project/Data/locations.csv')
locations
str(locations)

# import make details ----
make_details = fread('https://raw.githubusercontent.com/artyomashigov/CEU-R-intro-2024/main/Project/Data/make_details.csv')
make_details
str(make_details)

# import stolen vehicles ----
stolen_vehicles = fread('https://raw.githubusercontent.com/artyomashigov/CEU-R-intro-2024/main/Project/Data/stolen_vehicles.csv')
stolen_vehicles
str(stolen_vehicles)

# Convert 'population' from character to integer ----
locations$population <- as.integer(gsub(",", "", locations$population))

# Convert 'date_stolen' from character to Date in the format 'dd-mm-yyyy' ----
stolen_vehicles$date_stolen <- as.Date(stolen_vehicles$date_stolen, format="%m/%d/%y")
stolen_vehicles$date_stolen <- format(stolen_vehicles$date_stolen, "%d-%m-%Y")

# Column-wise count of missing values ----
# In 'locations'
sapply(locations, function(x) sum(is.na(x)))

# In 'make_details'
sapply(make_details, function(x) sum(is.na(x)))

# In 'stolen_vehicles'
sapply(stolen_vehicles, function(x) sum(is.na(x)))

# Merge datasets ----
df <- merge(stolen_vehicles, make_details, by = "make_id", all.x = TRUE)
df <- merge(df, locations, by = "location_id", all.x = TRUE)

# check missing values ----
sapply(df, function(x) sum(is.na(x)))

# drop missing values ----
df <- na.omit(df)
df <- df[vehicle_type != ""]
# use summary
summary(df)

# get weekday names ----
df$week_day <- weekdays(as.Date(df$date_stolen))

# Calculate the number of thefts per day of the week
theft_counts <- df[, .(Count = .N), by = .(week_day)]

# Order the days of the week
week_days_ordered <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
theft_counts$week_day <- factor(theft_counts$week_day, levels = week_days_ordered)

# Create a bar plot with ColorBrewer palette
ggplot(theft_counts, aes(x = week_day, y = Count, fill = week_day)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "BuGn") +  # Apply ColorBrewer palette
  geom_text(aes(label = Count), vjust = -0.3, color = "black") +  # Adding data labels
  labs(title = "Vehicle Thefts by Day of the Week", x = "", y = "") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))  # Center the title


# Aggregate data: count of stolen vehicles by vehicle type ----
vehicle_type_counts <- df[, .(Count = .N), by = .(vehicle_type)]

# Order the vehicle types by count
vehicle_type_counts <- vehicle_type_counts[order(-Count)]

# Create a bar chart with data labels
ggplot(vehicle_type_counts, aes(x = vehicle_type, y = Count, fill = vehicle_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, size = 2.5) +  # Adding data labels
  labs(title = "Count of Thefts by Vehicle Type",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Calculate the count of luxury vs. non-luxury vehicles ----
luxury_counts <- df[, .(Count = .N), by = .(make_type)]

# Calculate the percentages
luxury_counts[, Percentage := Count / sum(Count) * 100]

# Create a pie chart
ggplot(luxury_counts, aes(x = 2, y = Percentage, fill = make_type)) +  # Set x to a constant value
  geom_bar(stat = "identity", width = 0.8) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Stolen Luxury Cars", x = "", y = "") +
  theme_void() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +  
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5))  # Add labels

# Calculate the age of the vehicles ----
df[, vehicle_age := 2024 - model_year]

# Create a box plot of vehicle age by vehicle type
ggplot(df, aes(x = vehicle_type, y = vehicle_age, fill = vehicle_type)) +
  geom_boxplot() +
  labs(title = "Average Age of Stolen Vehicles by Vehicle Type",
       x = "",
       y = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))  # Center the title


# Aggregate data: count of stolen vehicles by region ----
theft_counts_by_region <- df[, .(Count = .N), by = .(region)]

# Order regions by count
theft_counts_by_region <- theft_counts_by_region[order(-Count)]

# Create a bar chart of theft counts by region with data labels
ggplot(theft_counts_by_region, aes(x = reorder(region, Count), y = Count, fill = region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, color = "black", size = 3) +  # Adding data labels
  labs(title = "Number of Vehicle Thefts by Region",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none", 
        plot.title = element_text(hjust = 0.5))  # Center the title

# Aggregate data: count of stolen vehicles by color ----
theft_counts_by_color <- df[, .(Count = .N), by = .(color)]

# Order colors by count
theft_counts_by_color <- theft_counts_by_color[order(-Count)]

# Create a bar chart of theft counts by color
ggplot(theft_counts_by_color, aes(x = reorder(color, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +  
  geom_text(aes(label = Count), vjust = -0.3, color = "black", size = 3) +  
  labs(title = "Vehicle Theft Counts by Color",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position = "none",  #
        plot.title = element_text(hjust = 0.5))  # Center the title

# Aggregate data: count of stolen vehicles by region's population density ----
theft_counts_by_density <- df[, .(Count = .N), by = .(density)]

# Create a scatter plot with a linear regression trend line
ggplot(theft_counts_by_density, aes(x = density, y = Count)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear regression trend line
  labs(title = "Vehicle Theft Counts by Population Density with Trend Line",
       x = "Population Density",
       y = "Count of Thefts") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 




