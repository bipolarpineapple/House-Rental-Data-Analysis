#Lim Wye Yee
#TP059371

install.packages("stringr")
install.packages("factoextra")
install.packages("ggcorrplot")
install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")
install.packages("ggpubr")
install.packages("treemapify")

#Import library
library(dplyr)
library(stringr)
library(ggplot2)
library(factoextra)
library(ggcorrplot)
library(tidyr)
library(ggmap)
library(maps)
library(mapdata)
library(ggpubr)
library(treemapify)

#Section 1: Data Import
#Import the data set
house_data = read.csv("C:\\Users\\chery\\Desktop\\Semester 1\\PDFA\\House_Rent_Dataset.csv", header=TRUE)
#View data in table form
View(house_data)

#Section 2: Overall Data Exploration
#Get Variable Names
names(house_data)

#Renaming the columns of the data set
names(house_data) = c("Posting Date", "BHK In Unit", "Rent Price", "Unit Size", "Floor Number", "Type of Area", "Locality",
                      "City Located", "Furnishings", "Preferred Tenants", "Number of Bathroom", "Contact Person")
house_data

#Calculate basic descriptive statistics
summary(house_data)

#Calculate the number of rows
nrow(house_data)

#Calculate the number of columns
ncol(house_data)

#Looking into the first 10 rows of the data set
head(house_data, 10)

#Looking into the last 10 rows of the data set
tail(house_data, 10)

#View 10 random rows of the data set
sample_n(house_data, 10)

#Categorise floor number data
factor(house_data$`Floor Number`)

#Categorise type of area data
factor(house_data$`Type of Area`)

#Categorise locality data
factor(house_data$Locality)

#Categorise city located data
factor(house_data$`City Located`)

#Categorise furnishing data
factor(house_data$Furnishings)

#Categorise preferred tenants
factor(house_data$`Preferred Tenants`)

#Categorise Contact Person
factor(house_data$`Contact Person`)

#Count of rows according to variable
house_data %>% count(`Type of Area`)
house_data %>% count(`BHK In Unit`)
house_data %>% count(`City Located`)
house_data %>% count(Furnishings)
house_data %>% count(`Preferred Tenants`)
house_data %>% count(`Number of Bathroom`)

#Section 3: Data Cleaning and Pre-processing
#List structure of data (including the data types)
str(house_data)

#Change data type of posting date to date
house_data$`Posting Date` <- as.Date(house_data$`Posting Date`, format="%m/%d/%Y")
str(house_data)

#Number of missing values
colSums(is.na(house_data))

#Standardise the format of locality
house_data$Locality <- tolower(house_data$Locality)

#Maintain the consistency of data
house_data$`Floor Number` <- tolower(house_data$`Floor Number`)
house_data$`Type of Area`<- tolower(house_data$`Type of Area`)
house_data$`City Located` <- tolower(house_data$`City Located`)
house_data$Furnishings <- tolower(house_data$Furnishings)
house_data$`Preferred Tenants`<- tolower(house_data$`Preferred Tenants`)
house_data$`Contact Person`<- tolower(house_data$`Contact Person`)

#Strip leading and trailing space
house_data$`Floor Number` <- trimws(house_data$`Floor Number`, which = c("both"))
house_data$`Type of Area` <- trimws(house_data$`Type of Area`, which = c("both"))
house_data$Locality <- trimws(house_data$Locality, which = c("both"))
house_data$`City Located` <- trimws(house_data$`City Located`, which = c("both"))
house_data$Furnishings <- trimws(house_data$Furnishings, which = c("both"))
house_data$`Preferred Tenants` <- trimws(house_data$`Preferred Tenants`, which = c("both"))
house_data$`Contact Person` <- trimws(house_data$`Contact Person`, which = c("both"))

#Looking into unique values
unique(house_data$Furnishings)
unique(house_data$`Preferred Tenants`)
unique(house_data$`City Located`)
unique(house_data$`Type of Area`)
unique(house_data$`Contact Person`)

#Detect outliers
plot(house_data$`Rent Price`)
lower_bound <- quantile(house_data$`Rent Price`, 0.1)
upper_bound <- quantile(house_data$`Rent Price`, 0.99)
lower_bound
upper_bound
  
outlier_ind <- which(house_data$`Rent Price` < lower_bound | house_data$`Rent Price` > upper_bound)
outlier_ind
length(outlier_ind)

#Replace outliers
house_data$`Rent Price`[house_data$`Rent Price` %in% outlier_ind] <- median(house_data$`Rent Price`)
house_data[house_data$`Rent Price` == 3500000, ]$`Rent Price` <- median(house_data[house_data$`City Located` == "bangalore", ]$`Rent Price`)

#Section 4: Data Transformation
#Price of each unit area
house_data$"Price Per Square Feet" <- round(house_data$`Rent Price`/ house_data$`Unit Size`, digits=2)

#Standardise the values of floor number
house_data["Floor"][house_data$Floor == "ground",] <- "0"
house_data["Floor"][house_data$Floor == "upper basement",] <- "-1"
house_data["Floor"][house_data$Floor == "lower basement",] <- "-2"
unique(house_data$Floor)

#Replace empty values in total number of floors
unique(house_data$`Total Number of Floors`)
house_data$`Total Number of Floors` <- as.numeric(house_data$`Total Number of Floors`)
total_floors <- house_data[!is.na(house_data$`Total Number of Floors`), ]
replace_total_floor <- median(total_floors$`Total Number of Floors`)
house_data["Total Number of Floors"][is.na(house_data$`Total Number of Floors`),] <- replace_total_floor

#Section 5: Questions
#Question 1: What is the preference of a bachelor in choosing a house?

#Analysis 1-1: Determine the number of bachelors
#Number of Bachelors
number_of_bachelors = nrow(house_data[(house_data$`Preferred Tenants` == "bachelors")
                                      |(house_data$`Preferred Tenants` == "bachelors/family"),])
number_of_bachelors

#Total number of Bachelors: 4274

#Analysis 1-2: Determine the preference of BHK by most bachelors?
#Distribution of overall bhk unit
house_data %>% count(house_data$`BHK In Unit`)

# For bachelors
calculate_bhk <- function(bhk)
{
  nrow(house_data[(house_data$`BHK In Unit` == bhk) & 
                    ((house_data$`Preferred Tenants` == "bachelors") | 
                       (house_data$`Preferred Tenants` == "bachelors/family")),])
}

one_bhk <- calculate_bhk(1)
two_bhk <- calculate_bhk(2)
three_bhk <- calculate_bhk(3)
four_bhk <- calculate_bhk(4)
five_bhk <- calculate_bhk(5)
six_bhk <- calculate_bhk(6)

#Plot barplot to determine the most BHK preferred
bhk_number <- c(one_bhk,two_bhk,three_bhk,four_bhk,five_bhk,six_bhk)
bhk_label <- c("1","2","3","4","5","6")
bachelor_bhk <- barplot(bhk_number, main = "Distribution of BHK In Unit for Bachelors", xlab = "Number of BHK",
        ylab = "Number of Bachelors", names.arg = bhk_label, ylim = c(0,3000), col = c("darkturquoise", "deeppink", "darkorchid",
                                                                     "deepskyblue", "cyan4", "blueviolet"))
text(bachelor_bhk, 0, bhk_number, cex = 1, pos = 3)

#Bachelors prefer to rent 2 BHK houses

#Analysis 1-3: Determine the preference of the number of bathrooms of the most preferred BHK
#Distribution of the number of bathrooms
house_data %>% count(`Number of Bathroom`)

calculate_bathroom <- function(bathroom)
{
  nrow(house_data[(house_data$`Number of Bathroom` == bathroom) & 
                    (house_data$`BHK In Unit` == 2),])
}

one_bathroom <- calculate_bathroom(1)
two_bathroom <- calculate_bathroom(2)
three_bathroom <- calculate_bathroom(3)
four_bathroom <- calculate_bathroom(4)
five_bathroom <- calculate_bathroom(5)
six_bathroom <- calculate_bathroom(6)
seven_bathroom <- calculate_bathroom(7)
ten_bathroom <- calculate_bathroom(10)

#Plot barplot for bathrooms of 2 BHK units
bathroom_number <- c(one_bathroom, two_bathroom, three_bathroom, four_bathroom,
                     five_bathroom, six_bathroom, seven_bathroom, ten_bathroom)
bathroom_label <- c("1", "2", "3", "4", "5", "6", "7", "10")
bathroom <- barplot(bathroom_number, main = "Distribution of The Number of Bathrooms for 2 BHK",
        xlab = "Number of Bathroom", ylab = "Number of Bachelors", ylim = c(0, 2000), names.arg = bathroom_label,
        col = c("darkturquoise", "deeppink", "darkorchid","deepskyblue", "cyan4", "blueviolet", "blue", "aquamarine"))
text(bathroom, 0, bathroom_number, cex = 1, pos = 3)

#The most preferred BHK usually has 2 bathrooms

#Analysis 1-4: Determine the average rent price preferred by bachelors

bachelor_rent_price <- mean(house_data[(house_data$`Preferred Tenants`== "bachelors"|
                                         house_data$`Preferred Tenants`== "bachelors/family"),
                                       'Rent Price'])
bachelor_rent_price

#The average rent price preferred by bachelors is $33333.95

#Analysis 1-5: Determine the average unit size preferred by bachelors

bachelor_unit_size <- mean(house_data[(house_data$`Preferred Tenants` == "bachelors" |
                                         house_data$`Preferred Tenants` == "bachelors/family"),
                                      'Unit Size'])

bachelor_unit_size

#Range of unit size
ggplot(house_data, aes(y=`Unit Size`, x=`Preferred Tenants`, color=`Preferred Tenants`)) +
  geom_boxplot() +
  labs(title="Range of Unit Size According to Tenants")
summary(house_data$`Unit Size`)

#The average unit size preferred by bachelors are 946.75 square feet

#Analysis 1-6: Determine the preference of bachelors on lower or upper floors

#Explore floor data
house_data %>% count(house_data$Floor)

floor_integer <- as.integer(house_data$Floor)
summary(floor_integer)
individual_floor <- house_data %>% distinct(floor_integer, keep_all = TRUE)
#Median = 24.5
summary(individual_floor)

#Groups the floors into lower floors and upper floors range
#Median is rounded up to 25 as floors cannot be counted in decimals
house_data <- house_data %>%
  mutate("Floor Range"= if_else(house_data$Floor <= 25, 'lower floors',
                                           'upper floors'))

#Pie chart for floor range
lower_floor = nrow(house_data[(house_data$`Floor Range` == "lower floors") & 
                                ((house_data$`Preferred Tenants` == "bachelors") 
                                 | (house_data$`Preferred Tenants` == "bachelors/family")),])
upper_floor = nrow(house_data[house_data$`Floor Range` == "upper floors" & 
                                ((house_data$`Preferred Tenants` == "bachelors") 
                                 | (house_data$`Preferred Tenants` == "bachelors/family")),])

floor = c(lower_floor, upper_floor)
label = c(lower_floor, upper_floor)

pie(floor, label, radius = 1, main="Distribution of Preferred Floor Range for Bachelors", col = c("cyan", "lightblue"))
legend("topright", c("Lower Floor", "Upper Floor"), cex = 0.7, fill = c("cyan", "lightblue"))

#Most of the bachelors tend to rent houses on lower floors

#Analysis 1-7: Determine the preference of cities for bachelors

#Calculate the number of rows
calculate_city <- function(city)
{
  nrow(house_data[(house_data$`City Located` == city) &
                    ((house_data$`Preferred Tenants` == "bachelors") |
                       (house_data$`Preferred Tenants` == "bachelors/family")),])
}

bachelor_kolkata <- calculate_city("kolkata")
bachelor_mumbai <- calculate_city("mumbai")
bachelor_bangalore <- calculate_city("bangalore")
bachelor_delhi <- calculate_city("delhi")
bachelor_chennai <- calculate_city("chennai")
bachelor_hyderabad <- calculate_city("hyderabad")

#Plot pie chart
bachelor_cities = c(bachelor_kolkata, bachelor_mumbai, bachelor_bangalore,
                    bachelor_delhi, bachelor_chennai, bachelor_hyderabad)
city_label = c("Kolkata", "Mumbai", "Bangalore", "Delhi", "Chennai", "Hyderabad")
city_number = c(bachelor_kolkata, bachelor_mumbai, bachelor_bangalore,
                bachelor_delhi, bachelor_chennai, bachelor_hyderabad)
color = c("red", "orange", "yellow", "green", "blue", "purple")

pie(bachelor_cities, city_number, radius = 1, main = "Distribution of Cities for Bachelors", col=color)
legend("topleft", city_label, cex = 0.7, fill=color)

#Bachelors prefer to stay at Bangalore

#Analysis 1-8: Determine the preference of the average total number of floors and the category of floors

floor_total <- as.integer(house_data$`Total Number of Floors`)
floor_mean <- ceiling(mean(house_data[(house_data$`Preferred Tenants`== "bachelors"|
                                              house_data$`Preferred Tenants`== "bachelors/family"),
                                           'Total Number of Floors']))
floor_mean

#The average total number of floors bachelors prefer to rent is 7 floors

floor_median <- ceiling(median(house_data[(house_data$`Preferred Tenants`== "bachelors"|
                                         house_data$`Preferred Tenants`== "bachelors/family"),
                                      'Total Number of Floors']))
house_data <- house_data %>%
  mutate("Building Category"= if_else(house_data$`Total Number of Floors`<= floor_median, 'short building',
                                      'tall building'))

#Pie chart for total floor range
short_building = nrow(house_data[(house_data$`Building Category` == "short building") &
                                   (house_data$`Preferred Tenants` == "bachelors" | 
                                      house_data$`Preferred Tenants` == "bachelors/family"),])
tall_building = nrow(house_data[(house_data$`Building Category` == "tall building") &
                                   (house_data$`Preferred Tenants` == "bachelors" | 
                                      house_data$`Preferred Tenants` == "bachelors/family"),])

building = c(short_building, tall_building)
building_label = c("Short Building", "Tall Building")
building_numbers = c(short_building, tall_building)

pie(building, building_numbers, radius = 1, main = "Distribution of Building Height For Bachelors",
    col = c("cyan", "lightgreen"))
legend("topright", building_label, cex = 0.7, fill = c("cyan", "lightgreen"))

#Most bachelors prefer to rent shorter buildings

#Analysis 1-9: Determine the preference of area type for bachelors

bachelor_area <- function(area)
{
  nrow(house_data[(house_data$`Type of Area` == area) &
                    (house_data$`Preferred Tenants` == "bachelors" | 
                       house_data$`Preferred Tenants` == "bachelors/family"),])
}

super_area = bachelor_area("super")
carpet_area = bachelor_area("carpet")
built_area = bachelor_area("built")

#Plot bar chart for type of area
area_type <- c(super_area,carpet_area,built_area)
area_label <- c("Super Area", "Carpet Area", "Built Area")

area <- barplot(area_type, main = "Distribution of The Area Types for Bachelors",
        xlab = "Type of Area", ylab = "Number of Bachelors", names.arg = area_label,
        col = c("darkturquoise", "deeppink", "darkorchid"), ylim = c(0,3000))
text(area, 0, area_type, cex = 1, pos = 3)

#Bachelors prefer super areas

#Analysis 1-10: Determine the preference of the furnishing type for bachelors

bachelor_furnishing <- function(furnishing)
{
  nrow(house_data[(house_data$Furnishings == furnishing) & 
                    (house_data$`Preferred Tenants`== "bachelors" | 
                       house_data$`Preferred Tenants` == "bachelors/family"),])
}

unfurnished = bachelor_furnishing("unfurnished")
semi_furnished = bachelor_furnishing("semi-furnished")
furnished = bachelor_furnishing("furnished")

#Plot bar chart for furnishings
furnishing_type <- c(unfurnished, semi_furnished, furnished)
furnishing_label <- c("Unfurnished", "Semi-furnished", "Furnished")

furnishing <- barplot(furnishing_type, main = "Distribution of Furnishings for Bachelors",
                      xlab = "Type of Furnishing", ylab = "Number of Bachelors", names.arg = furnishing_label,
                      col = c("purple", "turquoise", "yellow"), ylim = c(0,3000))
text(furnishing, 0, furnishing_type, cex = 1, pos = 3)

#Bachelors prefer semi-furnished units

#Analysis 1-11: Determine the preference of bachelors in terms of contact person

bachelor_contact <- function(person)
{
  nrow(house_data[(house_data$`Contact Person` == person) 
                  & (house_data$`Preferred Tenants`== "bachelors" | 
                       house_data$`Preferred Tenants` == "bachelors/family"),])
}

owner = bachelor_contact("owner")
agent = bachelor_contact("agent")
builder = bachelor_contact("builder")

#Plot bar chart for contact person
contact_person <- c(owner, agent, builder)
contact_label <- c("Owner", "Agent", "Builder")

contact <- barplot(contact_person, main = "Distribution of Contact Person for Bachelors",
                   xlab = "Person of Contact", ylab = "Number of Bachelors", names.arg = contact_label,
                   col = c("purple3", "cyan", "blue"), ylim = c(0,3000))
text(contact, 0, contact_person, cex = 1, pos = 3)

#Bachelors prefer to contact the owner straight away

#Question 2: What is the pricing of the rent affected by?
#Analysis 2-1: Relationship between unit size and rent.

#Plot scatterplot with linear line
ggplot(house_data, aes(x=`Unit Size`, y=`Rent Price`)) + 
  geom_point(shape = 15, aes(colour = `Rent Price`)) + geom_smooth(method = lm) +
  scale_colour_gradient(low = "green", high = "yellow") +
  labs(title="Relationship Between Unit Size and Rent Price")

#As the unit size increases, the rent increases. Hence, the unit size affects the price of the rent.

#Analysis 2-2 : Relationship between BHK and rent price

#Aggregate values according to BHK In Unit
bhk_price <- house_data %>%
  group_by(`BHK In Unit`) %>%
  summarise(bathroom_mean=round(mean(`Rent Price`)), 2)

#Plot bar plot
ggplot(bhk_price, aes(x=`BHK In Unit`, y= bathroom_mean)) +
  geom_bar(aes(fill=`BHK In Unit`), stat = "identity") +
  geom_text(aes(label=bathroom_mean)) +
  labs(title = "BHK In Unit vs Rent Price of Houses for Rental", x="BHK In Unit", y="Price")

#Analysis 2-3: Mean of the rental price for each type of areas

mean_area <- function(area)
{
  round(mean(house_data[house_data$`Type of Area` == area, 'Rent Price']), 2)
}

mean_super = mean_area("super")
mean_carpet = mean_area("carpet")
mean_built = mean_area("built")

#Construct bar chart to compare mean for each area
mean_area <- c(mean_super, mean_carpet, mean_built)
mean_area_label <- c("Super Area", "Carpet Area", "Built Area")


mean_area_plot <- barplot(mean_area, main = "Distribution of Mean for Types of Area", xlab = "Type of Area",
                          ylab = "Mean Value", names.arg = mean_area_label, col = mean_area, ylim = c(0,70000))
text(mean_area_plot,0, mean_area, cex = 1, pos = 3)

#The carpet area has the highest rent whereas the built area has the lowest rent

#Analysis 2-4: Mean of the rental price for each type of furnishings.

mean_furnishings <- function(furnishings)
{
  round(mean(house_data[house_data$Furnishings == furnishings, 'Rent Price']), 2)
}

mean_unfurnished = mean_furnishings("unfurnished")
mean_semi_furnished = mean_furnishings("semi-furnished")
mean_furnished = mean_furnishings("furnished")

#Construct bar chart to compare mean of each furnishings
mean_furnishing <- c(mean_unfurnished, mean_semi_furnished, mean_furnished)
mean_furnishing_label <- c("Unfurnished", "Semi-furnished", "Furnished")

mean_furnishing_plot <- barplot(mean_furnishing, main = "Distribution of Mean Rent Price for Types of Furnishing", xlab = "Types of Furnishing"
                                , ylab= "Mean Value", names.arg = mean_furnishing_label, col = mean_furnishing, ylim = c(1, 60000))
text(mean_furnishing_plot,0, mean_furnishing, cex = 1, pos = 3)

#Furnished units tend to have the highest rent whereas unfurnished units have the lowest rent

#Analysis 2-5: Relationship between the number of bathrooms and the rent price.

#Aggregate the mean of the rent price according to the number of bathroom
bathroom_price <- house_data %>%
  group_by(`Number of Bathroom`) %>%
  summarise(bathroom_mean=round(mean(`Rent Price`)), 2)

#Construct bar plot
ggplot(bathroom_price, aes(x=`Number of Bathroom`, y= bathroom_mean)) +
  geom_bar(stat = "identity", aes(fill=`Number of Bathroom`)) +
  geom_text(aes(label=bathroom_mean), position = position_stack(vjust = 0.5)) +
  labs(title = "Number of Bathrooms vs Rent Price of Houses for Rental", x="Number of Bathroom", y="Price")

#Analysis 2-6: Relationship between the floor number and the rent.

#Calculate bin mean
floor_bin_one <- mean(binned_data[binned_data$`Floor Bin` == 1, 'Rent Price'])
floor_bin_two <-mean(binned_data[binned_data$`Floor Bin` == 2, 'Rent Price'])
floor_bin_three <-mean(binned_data[binned_data$`Floor Bin` == 3, 'Rent Price'])
floor_bin_four <-mean(binned_data[binned_data$`Floor Bin` == 4, 'Rent Price'])
floor_bin_five <-mean(binned_data[binned_data$`Floor Bin` == 5, 'Rent Price'])

floor_bin_rent <- c(floor_bin_one, floor_bin_two, floor_bin_three, floor_bin_four, floor_bin_five)
floor_bin <- c(1,2,3,4,5)

floor_bin_data <- data.frame(floor_bin,floor_bin_rent)

#Plot line chart
ggplot(floor_bin_data, aes(x=floor_bin, y= floor_bin_rent, color=floor_bin_rent)) +
  scale_colour_gradient2(low = "red", mid = "yellow" , high = "seagreen", midpoint=median(floor_bin_rent)) +
  geom_line() + geom_point() +
  labs(title = "Floor vs Rent Price", x="Floor Range", y="Rent Price")

#The rent price is higher when the unit is at higher floors
#The rent price then gradually decreases at middle floors

#Analysis 2-7: Relationship between the total number of floors and the rent price

#Plot scatterplot with linear line
ggplot(house_data, aes(x=`Total Number of Floors`, y=`Rent Price`)) + 
  geom_point(shape = 15, aes(colour = `Rent Price`)) +
  geom_smooth(method=lm, color = "purple") +
  scale_colour_gradient(low = "pink", high = "blue") +
  labs(title= "Total Number of Floors vs Rent Price")

#As the total number of floors increases, the rent price increases (without taking into account other factors)

#Analysis 2-8: Mean of the rental price for each city. 

ggplot(house_data, aes(y=log(`Rent Price`), x=`City Located`, fill=`City Located`)) +
  geom_boxplot() +
  labs(title="Range of Rental Price According to City")

#Mumbai has the highest range of rent and start value

#Analysis 2-9: Clustering of the pricing of the property
library(factoextra)

#Make data frame
cluster_column <- data.frame(as.numeric(house_data$`Total Number of Floors`),
                             house_data$`Rent Price`)
cluster_column <- scale(cluster_column)
set.seed(123)

#K-means model
model <- kmeans(cluster_column, 5, nstart = 50)
print(model)

#Visualize cluster
fviz_cluster(model, data = cluster_column)

#Analysis 2-10: Distribution of budget and expensive properties according to Analysis 2-9

#Add the clusters as a column
house_data <- cbind(house_data, 'Price Category' = model$cluster)
head(house_data)

#Check the count for each cluster
model$size

#Check cluster means
model$centers

#Change the cluster number to meaningful labels
house_data["Price Category"][house_data$`Price Category` == 1,] <- "Slightly Budget"
house_data["Price Category"][house_data$`Price Category` == 2,] <- "Moderate"
house_data["Price Category"][house_data$`Price Category` == 3,] <- "Slightly Expensive"
house_data["Price Category"][house_data$`Price Category` == 4,] <- "Expensive"
house_data["Price Category"][house_data$`Price Category` == 5,] <- "Budget"

#Plot bar chart
ggplot(house_data, aes(x =`Price Category`)) +
  geom_bar(aes(fill=`Price Category`)) +
  labs(title = "Distributions of Price Categories")

#Most of the units fall under the budget price range

#Question 3: Which cities have the most demand on renting property? And why?

#Analysis 3-1: Distribution of properties within cities

#Calculation of the number of rows of each city
city_distribution <- function(city_name)
{
  nrow(house_data[house_data$`City Located` == city_name ,])
}

property_kolkata = city_distribution("kolkata")
property_mumbai = city_distribution("mumbai")
property_bangalore = city_distribution("bangalore")
property_delhi = city_distribution("delhi")
property_chennai = city_distribution("chennai")
property_hyderabad = city_distribution("hyderabad")

#Plot barplot
city_property <- c(property_kolkata, property_mumbai, property_bangalore, property_delhi,
                   property_chennai, property_hyderabad)
city_property_label <- c("Kolkata", "Mumbai", "Bangalore", "Delhi", "Chennai", "Hyderabad")

city_property_plot <- barplot(city_property, main = "Distribution of Properties Within Cities", xlab = "City"
                                , ylab= "Number of Properties", names.arg = city_property_label, 
                              col = city_property, ylim = c(0, 1000))
text(city_property_plot,0, city_property, cex = 1, pos = 3)

#Mumbai has the most demand in properties

#Analysis 3-2: Distributions of BHK according to cities

#Plot barplot according to BHK In Unit
ggplot(house_data, aes(x = `BHK In Unit`)) +
  geom_bar(aes(fill=factor(`BHK In Unit`))) +
  facet_wrap(~`City Located`) +
  labs(title = "Distributions of BHK According to Cities")

#Seems like the BHK available did not affect the preference of people in choosing a city as all cities has 2 BHK
#as the top BHK in demand

#Analysis 3-3: Distributions of areas according to cities

#Plot barplot according to Type of Area
ggplot(house_data, aes(x = `Type of Area`)) +
  geom_bar(aes(fill=factor(`Type of Area`))) +
  facet_wrap(~`City Located`) +
  labs(title = "Distributions of The Type of Areas According to Cities")

#According to the preference of people, they tend to be more fond of super areas, which is present the most
#at Hyderabad
  
#Analysis 3-4: Distributions of unit sizes according to cities

  #Classify unit sizes to categories
  unit_median <- median(house_data$`Unit Size`)
  house_data <- house_data %>%
    mutate("Unit Size Category"= if_else(house_data$`Unit Size`<= unit_median, 'small unit',
                                        'big unit'))
  
  #Plot barplot
  ggplot(house_data, aes(x = `Unit Size Category`)) +
    geom_bar(aes(fill=factor(`Unit Size Category`))) +
    facet_wrap(~`City Located`) +
    labs(title = "Distributions of Unit Sizes According to Cities")
  
#Mumbai has most of the smaller units whereas Hyderabad has the most bigger units
  
#Analysis 3-5: Distributions of furnishings according to cities

#Plot barplot
ggplot(house_data, aes(x = Furnishings)) +
  geom_bar(aes(fill=Furnishings)) +
  facet_wrap(~`City Located`) +
  labs(title = "Distributions of The Type of Furnishings According to Cities")

#Bangalore has the most semi-furnished units, Mumbai has the most furnished units and Chennai has the most unfurnished units

#Analysis 3-6: Distributions of the number of bathrooms according to cities

#Plot barplot
ggplot(house_data, aes(x =as.character(`Number of Bathroom`))) +
  geom_bar(aes(fill=`Number of Bathroom`)) +
  facet_wrap(~`City Located`) +
  labs(title = "Distributions of The Number of Bathrooms According to Cities")

#Cities like Chennai, Hyderabad and Mumbai has a higher number of 2 bathroom units, which people usually prefer

#Analysis 3-7: The number of properties and the average rent for each city

#Calculate the number of rows and rent price for each city
property_count <- function(city)
{
  nrow(house_data[house_data$`City Located` == city,])
}

average_city_rent <- function(city_name)
{
  mean(house_data[house_data$`City Located` == city_name, 'Rent Price'])
}

kolkata = property_count("kolkata")
bangalore = property_count("bangalore")
chennai = property_count("chennai")
delhi = property_count("delhi")
hyderabad = property_count("hyderabad")
mumbai = property_count("mumbai")

kolkata_rent = average_city_rent("kolkata")
bangalore_rent = average_city_rent("bangalore")
chennai_rent = average_city_rent("chennai")
delhi_rent = average_city_rent("delhi")
hyderabad_rent = average_city_rent("hyderabad")
mumbai_rent = average_city_rent("mumbai")

property_amount <- c(kolkata, bangalore, chennai, delhi, hyderabad, mumbai)
city_name <- c("Kolkata", "Bangalore", "Chennai", "Delhi", "Hyderabad", "Mumbai")
rent_city <- c(kolkata_rent, bangalore_rent, chennai_rent, delhi_rent, hyderabad_rent, mumbai_rent)

property_frame <- data.frame(city_name,property_amount,rent_city)

#Plot scatterplot with loess line
ggplot(property_frame, aes(x=property_amount, y= rent_city, label=city_name)) +
  geom_point() +
  geom_smooth(method = "loess", color="purple") +
  geom_text(hjust = 0.5, vjust=1.5) +
  labs(title="Relationship Between the Number of Properties and The Average Rent")

#The amount of rent gradually increases when there are a huge number of properties

#Question 4: What is the preference of a family in choosing a house?

#Analysis 4-1: Determine the number of Families
number_of_families = nrow(house_data[(house_data$`Preferred Tenants` == "family")
                                      |(house_data$`Preferred Tenants` == "bachelors/family"),])
number_of_families

#Total number of Bachelors: 3916

#Analysis 4-2: Determine the preferred BHK by most families

#Calculate the number of rows of each BHK for families
family_bhk <- function(bhk)
{
  nrow(house_data[(house_data$`BHK In Unit` == bhk) & 
                    ((house_data$`Preferred Tenants` == "family") | 
                       (house_data$`Preferred Tenants` == "bachelors/family")),])
}

one_bhk_fam <- family_bhk(1)
two_bhk_fam <- family_bhk(2)
three_bhk_fam <- family_bhk(3)
four_bhk_fam <- family_bhk(4)
five_bhk_fam <- family_bhk(5)
six_bhk_fam <- family_bhk(6)

#Plot barplot to determine the most BHK preferred
bhk_number <- c(one_bhk_fam,two_bhk_fam,three_bhk_fam,four_bhk_fam,five_bhk_fam,six_bhk_fam)
bhk_label <- c("1","2","3","4","5","6")
bhk_family <- barplot(bhk_number, main = "Distribution of BHK In Unit for Families", xlab = "Number of BHK",
              ylab = "Number of Families", names.arg = bhk_label, col = bhk_number, ylim= c(0,2500))
text(bhk_family, 0, bhk_number, cex = 1, pos = 3)

#Families prefer to rent 2 BHK houses

#Analysis 4-3: Determine the number of bathrooms families prefer

#Calculate the number of rows of each number of bathroom
family_bathroom <- function(bathroom)
{
  nrow(house_data[(house_data$`Number of Bathroom` == bathroom) & 
                    ((house_data$`Preferred Tenants` == "family") | 
                       (house_data$`Preferred Tenants` == "bachelors/family")),])
}

one_bathroom_fam <- family_bathroom(1)
two_bathroom_fam <- family_bathroom(2)
three_bathroom_fam <- family_bathroom(3)
four_bathroom_fam <- family_bathroom(4)
five_bathroom_fam <- family_bathroom(5)
six_bathroom_fam <- family_bathroom(6)
seven_bathroom_fam <- family_bathroom(7)
ten_bathroom_fam <- family_bathroom(10)

#Plot radial plot for bathroom
bathroom_number_fam <- c(one_bathroom_fam, two_bathroom_fam, three_bathroom_fam, four_bathroom_fam,
                     five_bathroom_fam, six_bathroom_fam, seven_bathroom_fam, ten_bathroom_fam)
bathroom_label_fam <- c("1", "2", "3", "4", "5", "6", "7", "10")
bathroom_fam_tab <- data.frame(bathroom_label_fam, bathroom_number_fam)

ggplot(bathroom_fam_tab) +
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(0:1) * 1000),
    color= "lightgrey") +
  geom_col(aes(
    x = reorder(str_wrap(bathroom_label_fam, 8), bathroom_number_fam),
    y = bathroom_number_fam,
    fill = bathroom_label_fam),
  position = "dodge2",
  show.legend = TRUE, 
  alpha= .9) +
  geom_segment(aes(
    x = reorder(str_wrap(bathroom_label_fam, 8), bathroom_number_fam),
    y = 0,
    xend = reorder(str_wrap(bathroom_label_fam, 8), bathroom_number_fam),
    yend = 2000),
  linetype = "dashed",
  color = "gray12") +
  coord_polar() +
  labs(title = "Number of Bathrooms Preferred By Families", ylab = "Count",
       xlab="Number of Bathrooms")

#Families prefer their houses to have 2 bathrooms

#Analysis 4-4: Determine the average rent price preferred by families

#Calculate mean rent price
family_rent_price <- mean(house_data[(house_data$`Preferred Tenants`== "family"|
                                          house_data$`Preferred Tenants`== "bachelors/family"),
                                       'Rent Price'])

family_rent_price

#The average rent price preferred by families is $33480.94

#Analysis 4-5: Determine the average unit size preferred by families

#Calculate mean unit size
family_unit_size <- mean(house_data[(house_data$`Preferred Tenants` == "family" |
                                         house_data$`Preferred Tenants` == "bachelors/family"),
                                      'Unit Size'])

family_unit_size

#The average unit size preferred by families are 957.3979 square feet

#Analysis 4-6: Determine the preference of families in staying at lower or upper floors

#Calculate the number of floors
lower_floor_fam = nrow(house_data[(house_data$`Floor Range` == "lower floors") & 
                                ((house_data$`Preferred Tenants` == "family") 
                                 | (house_data$`Preferred Tenants` == "bachelors/family")),])
upper_floor_fam = nrow(house_data[house_data$`Floor Range` == "upper floors" & 
                                ((house_data$`Preferred Tenants` == "family") 
                                 | (house_data$`Preferred Tenants` == "bachelors/family")),])

floor_fam = c(lower_floor_fam, upper_floor_fam)
label_fam = c("Lower Floor", "Upper Floor")

fam_floor <- data.frame(label_fam, floor_fam)

# Compute percentages
fam_floor$fraction = fam_floor$floor_fam / sum(fam_floor$floor_fam)

# Compute the cumulative percentages (top of each rectangle)
fam_floor$ymax <- cumsum(fam_floor$fraction)

# Compute the bottom of each rectangle
fam_floor$ymin <- c(0, head(fam_floor$ymax, n=-1))

# Compute label position
fam_floor$labelPosition <- (fam_floor$ymax + fam_floor$ymin) / 2

# Compute a good label
fam_floor$label <- paste0(fam_floor$label_fam, "\n value:", fam_floor$floor_fam)

#Construct Donut Plot
ggplot(fam_floor, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=label_fam)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Distribution of the Preference of Floors Among Families")

#Most of the families tend to rent houses on lower floors

#Analysis 4-7: Determine the cities that families prefer to stay at

#Calculate the number of rows of each city for families
family_city <- function(city)
{
  nrow(house_data[(house_data$`City Located` == city) &
                    ((house_data$`Preferred Tenants` == "family") |
                       (house_data$`Preferred Tenants` == "bachelors/family")),])
}

family_kolkata <- family_city("kolkata")
family_mumbai <- family_city("mumbai")
family_bangalore <- family_city("bangalore")
family_delhi <- family_city("delhi")
family_chennai <- family_city("chennai")
family_hyderabad <- family_city("hyderabad")

#Plot pie chart
family_cities = c(family_kolkata, family_mumbai, family_bangalore,
                    family_delhi, family_chennai, family_hyderabad)
city_label = c("Kolkata", "Mumbai", "Bangalore", "Delhi", "Chennai", "Hyderabad")
city_number_fam = c(family_kolkata, family_mumbai, family_bangalore,
                family_delhi, family_chennai, family_hyderabad)

pie(family_cities, city_number_fam, radius = 1, main = "Distribution of Cities for Families",
    col=family_cities)
legend("topright", city_label, cex = 0.7, fill=family_cities)

#Families prefer to stay at Mumbai

#Analysis 4-8: Determine the preference of the average total number of floors and the category of floors for families

floor_mean_fam <- ceiling(mean(house_data[(house_data$`Preferred Tenants`== "family"|
                                         house_data$`Preferred Tenants`== "bachelors/family"),
                                      'Total Number of Floors']))
floor_mean_fam

#The average total number of floors families prefer to rent is 7 floors

#Stacked bar chart for total floor range
short_building_fam = nrow(house_data[(house_data$`Building Category` == "short building") &
                                   (house_data$`Preferred Tenants` == "family" | 
                                      house_data$`Preferred Tenants` == "bachelors/family"),])
tall_building_fam = nrow(house_data[(house_data$`Building Category` == "tall building") &
                                  (house_data$`Preferred Tenants` == "family" | 
                                     house_data$`Preferred Tenants` == "bachelors/family"),])

building_fam = c(short_building_fam, tall_building_fam)
building_label = c("Short Building", "Tall Building")
building_numbers_fam = data.frame(building_fam, building_label)

ggplot(building_numbers_fam, aes(x="", y=building_fam, fill=building_label, label=building_fam)) +
  geom_col(position = position_stack()) +
  guides(fill=guide_legend(reverse=TRUE)) +
  geom_text(hjust = 1.5, vjust=0, reverse=TRUE) +
  coord_flip() +
  ggtitle("Distribution of Building Height For Families") +
  xlab("Building Category") +
  ylab("Number of Families") +
  scale_fill_brewer(palette = 8)

#Most families prefer to rent shorter buildings

#Analysis 4-9: Determine the preference of the type of area for families

#Calculate the number of rows according to the type of area
family_area <- function(area)
{
  nrow(house_data[(house_data$`Type of Area` == area) &
                    (house_data$`Preferred Tenants` == "family" | 
                       house_data$`Preferred Tenants` == "bachelors/family"),])
}

super_area_fam = family_area("super")
carpet_area_fam = family_area("carpet")
built_area_fam = family_area("built")

#Plot dot chart for type of area
area_type_fam <- c(super_area_fam,carpet_area_fam,built_area_fam)
area_label <- c("Super Area", "Carpet Area", "Built Area")
fam_area <- data.frame(area_label, area_type_fam)

ggdotchart(fam_area, x = "area_label", y = "area_type_fam",
           color="area_label",
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
           sorting = "descending",                       
           rotate = TRUE,                                                              
           y.text.col = TRUE,
           dot.size = 15,                                 
           label = area_type_fam,                       
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),  
           ggtheme = theme_pubr()                       
  )+
  theme_cleveland() +
  ggtitle("Distribution of The Area Types for Families") +
  ylab("Number of Families")

#Families prefer super areas

#Analysis 4-10: Determine the preference of the type of furnishings for families

#Calculate the number of rows for furnishings
family_furnishing <- function(furnishing)
{
  nrow(house_data[(house_data$Furnishings == furnishing) & 
                    (house_data$`Preferred Tenants`== "family" | 
                       house_data$`Preferred Tenants` == "bachelors/family"),])
}

unfurnished_fam = family_furnishing("unfurnished")
semi_furnished_fam = family_furnishing("semi-furnished")
furnished_fam = family_furnishing("furnished")

#Plot Lollipop chart for furnishings
furnishing_type_fam <- c(unfurnished_fam, semi_furnished_fam, furnished_fam)
furnishing_label <- c("Unfurnished", "Semi-furnished", "Furnished")
furnish_fam_tab <- data.frame(furnishing_label, furnishing_type_fam)

ggdotchart(furnish_fam_tab, x = "furnishing_label", y = "furnishing_type_fam",
           color = "furnishing_label",                              
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
           sorting = "descending",                       
           add = "segments",                             
           rotate = TRUE,                                
           group = "furnishing_label",                              
           dot.size = 10,                                 
           label = round(furnish_fam_tab$furnishing_type_fam),                
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               
           ggtheme = theme_pubr()                        
) + ggtitle("Distribution of Furnishing Types for Families") +
  xlab("Number of Family") +
  ylab("Furnishing Type")

#Families prefer semi-furnished units

#Analysis 4-11: Determine the preference of contact person for families

#Calculate the number of rows for each contact person
family_contact <- function(person)
{
  nrow(house_data[(house_data$`Contact Person` == person) 
                  & (house_data$`Preferred Tenants`== "family" | 
                       house_data$`Preferred Tenants` == "bachelors/family"),])
}

owner_fam = family_contact("owner")
agent_fam = family_contact("agent")
builder_fam = family_contact("builder")

#Plot bar chart for contact person
contact_person_fam <- c(owner_fam, agent_fam, builder_fam)
contact_label <- c("Owner", "Agent", "Builder")

contact_fam <- barplot(contact_person_fam, main = "Distribution of Contact Person for Families",
                   xlab = "Person of Contact", ylab = "Number of Families", names.arg = contact_label,
                   col = contact_person_fam, ylim = c(0,3000))
text(contact_fam, 0, contact_person_fam, cex = 1, pos = 3)

#Families prefer to contact the owner for rental inquiries

#Question 5: How the locality and city affects the number of floors of houses?
#Parse the locality to two columns
house_data[c('Area', 'Area 2')] <- str_split_fixed(house_data$Locality,', ', 2)
house_data$`Area 2`=NULL
unique(house_data$Area)

#Analysis 5-1: Distribution of the height of a building according to city

#Plot bar chart
ggplot(house_data, aes(x =`Building Category`, fill=`Building Category`)) +
  geom_bar() +
  facet_wrap(~`City Located`) +
  labs(title = "Distributions of Building Categories According to Cities")

#Analysis 5-2: Mean of the total height of buildings in each city

#Aggregate mean total number of floors grouped by city located
floor_means <- house_data %>%
  group_by(`City Located`) %>%
  summarise(mean_floor=ceiling(mean(`Total Number of Floors`)))
floor_means

#Create a new data frame with new aggregated columns
city_floor <- floor_means %>%
              arrange(mean_floor) %>%
              mutate(Avg = mean(mean_floor, na.rm = TRUE),
                     Above = ifelse(mean_floor - Avg > 0, TRUE, FALSE),
                     city = factor(`City Located`, levels = .$`City Located`))
city_floor

#Create lollipop chart
ggplot(city_floor, aes(y =`City Located`, x=mean_floor, color=Above, label=mean_floor)) +
  geom_segment(aes(x=Avg, y=`City Located`, xend = mean_floor, yend =`City Located`), color = "grey50") +
  geom_point() +
  geom_text(nudge_x = 1.5) +
  labs(title = "Mean of Total Height of Buildings In Each City") +
  xlab("Mean of Total Floors")

#Analysis 5-3: Determine the top localities

#Extract the top 10 locality
locality_freq <- data.frame(house_data %>% count(Area, sort=TRUE))
top_ten <- head(locality_freq, 10)

#Create bar plot
ggbarplot(top_ten, x = "Area", y="n",
          fill = "Area",
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palette
          sort.val = "desc",          # Sort the value in descending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90,
          label = TRUE) +
  ggtitle("Distibution of Properties in Top 10 Localities") +
  ylim(0,40)

#Analysis 5-4: The mean total number of floors according to the top 5 localities

#Aggregate the mean of the total number of floors based on locality
locality_floor_mean <- house_data %>%
  group_by(Locality) %>%
  summarise(locality_mean=ceiling(mean(`Total Number of Floors`)))
locality_floor_mean

localities <- c('bandra west', 'gachibowli', 'electronic city', 'miyapur, nh 9', 'velachery')

top_five_mean <- locality_floor_mean[locality_floor_mean$Locality %in% localities,]

#Create a data frame with aggregated columns
locality_floor <- top_five_mean %>%
  arrange(locality_mean) %>%
  mutate(average = mean(locality_mean, na.rm = TRUE),
         above = ifelse(locality_mean - average > 0, TRUE, FALSE),
         locality = factor(Locality, levels = .$Locality))
locality_floor

#Create lollipop chart
ggplot(locality_floor, aes(y =Locality, x=locality_mean, color=above, label=locality_mean)) +
  geom_segment(aes(x=average, y=Locality, xend = locality_mean, yend =Locality), color = "grey50") +
  geom_point() +
  geom_text(nudge_x = 1.5) +
  labs(title = "Mean of Total Height of Buildings In The Top 5 Localities To Stay At") +
  xlab("Mean of Total Number of Floors")

#Analysis 5-5: Plot a map according to cities to determine its geographical location

#Insert Coordinates

house_data <- house_data %>%
  mutate('Latitude' = case_when(
    `City Located`== 'kolkata' ~ 22.5726,
    `City Located`== 'mumbai' ~ 19.0760,
    `City Located`== 'bangalore' ~ 12.9716,
    `City Located`== 'delhi' ~ 28.6448,
    `City Located`== 'chennai' ~ 13.0827,
    `City Located`== 'hyderabad' ~ 17.3850
  ))

house_data <- house_data %>%
  mutate('Longitude' = case_when(
    `City Located`== 'kolkata' ~ 88.3639,
    `City Located`== 'mumbai' ~ 72.8777,
    `City Located`== 'bangalore' ~ 77.5946,
    `City Located`== 'delhi' ~ 77.2167,
    `City Located`== 'chennai' ~ 80.2707,
    `City Located`== 'hyderabad' ~ 78.4867
  ))

#Insert India map coordinates and plot map
india <- map_data("world", region = "india")
india_floors
india_plot <- ggplot() +
  geom_polygon(data=india, aes(x=long, y=lat, group=group), fill="#00AF92") +
  coord_fixed(1.3)
india_plot

#Keep distinct values
cords <- house_data %>% distinct(`City Located`, .keep_all = TRUE)
cords <- select(cords,`City Located`, Longitude, Latitude, Floor)
names(cords) <- c('City', 'Longitude', 'Latitude', 'Floor')
cords

#Plot the points of the cities
india_plot +
  geom_point(data=cords, aes(x=Longitude, y=Latitude, color=City), size = 4) +
  geom_text(data=cords, aes(x=Longitude, y=Latitude, label=paste(" ", 
                                                                 as.character(City),
                                                                 sep = "")),
            hjust = 0.5, vjust=1.5) +
  labs(title="Geographical Locations of Cities")

#Analysis 5-6: Distribution of the geographical structure of each city

#Reference to India's Map at analysis 5-5 - Coastal, Inland

#Categorise the city location
house_data <- house_data %>%
  mutate('Geographical Location' = case_when(
    `City Located`== 'kolkata' ~ 'coastal',
    `City Located`== 'mumbai' ~ 'coastal',
    `City Located`== 'bangalore' ~ 'inland',
    `City Located`== 'delhi' ~ 'inland',
    `City Located`== 'chennai' ~ 'coastal',
    `City Located`== 'hyderabad' ~ 'inland'
  ))

#Count the geographical location
geographical_data <- house_data %>% count(`Geographical Location`)

#Plot pie chart
ggplot(geographical_data, aes(x="", y=n,
                       fill = `Geographical Location`)) +
  geom_col() +
  geom_text(aes(label=n),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = 9) +
  labs(title="Distribution of Properties According to Geographical Location")

#Analysis 5-7: Distribution of the average total number of floors according to geographical characteristics

#Aggregate mean of total number of floors grouped by geographical location
geographical_floor <- house_data %>%
  group_by(`Geographical Location`) %>%
  summarise(geo_mean=ceiling(mean(`Total Number of Floors`)))
geographical_floor

#Plot bar plot
ggplot(geographical_floor, aes(x=`Geographical Location`, y=geo_mean, 
                               fill=`Geographical Location`, label=geo_mean))+
  geom_bar(stat="identity") +
  geom_text() +
  scale_fill_manual(values = c("#FF007F", "#7F00FF")) +
  labs(title="Average Total Number of Floors According to Geographical Characteristics")

#Question 6: When and where do people usually start looking for houses?
#Parse dates to day, month and year column

house_data[c('Year', 'Month', 'Day')] <- str_split_fixed(house_data$`Posting Date`,'-', 3)
unique(house_data$Month)
unique(house_data$Year)

#Analysis 6-1: Demand of houses over time

#Count the houses available according to the posting date
demand_houses <- data.frame(house_data %>% count(house_data$`Posting Date`))
demand_houses

#Plot time series plot
ggplot(demand_houses, aes(x=house_data..Posting.Date., y=n, color=n)) +
  geom_line() +
  geom_smooth(method=lm) +
  scale_y_continuous(
    labels = function(x)
      format(x, big.mark = " ", scientific = FALSE)
  ) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  labs(title="Demand of Houses Over Time") +
  xlab("Posting Date") +
  ylab("Number of Houses")

#Analysis 6-2: Rental Price of houses over time

#Aggregate the mean of the rent price according to the posting date
time_rent_mean <- house_data %>%
  filter(`Posting Date` > '2022-04-13') %>%
  group_by(`Posting Date`) %>%
  summarise(rent_mean=round(mean(`Rent Price`), 2))
time_rent_mean

#Plot time series plot
ggplot(time_rent_mean, aes(x=`Posting Date`, y=rent_mean, color=rent_mean)) +
  geom_line() +
  geom_smooth(method = lm, color = 'orange') +
  scale_y_continuous(
    labels = function(x)
      format(x, big.mark = " ", scientific = FALSE)
  ) +
  scale_colour_gradient(low = "blue", high = "orange") +
  theme_light() +
  theme(panel.grid = element_blank()) +
  labs(title="Mean Rental Prices of Houses Over Time")

#Analysis 6-3: Determine the month where bachelors usually start finding houses

#Calculate the number of rows for each month
bachelor_time <- function(month)
{
  nrow(house_data[(house_data$`Preferred Tenants` == "bachelors" | 
                house_data$`Preferred Tenants`== "bachelors/family")
             & house_data$Month == month,])
}

bachelor_april <- bachelor_time('04')
bachelor_may <- bachelor_time('05')
bachelor_june <- bachelor_time('06')
bachelor_july <- bachelor_time('07')

bachelor_time_demand <- c(bachelor_april, bachelor_may, bachelor_june, bachelor_july)
month <- c('April','May', 'June', 'July')

bachelor_month <- data.frame(month, bachelor_time_demand)

#Plot pie chart
ggplot(bachelor_month, aes(x="", y=bachelor_time_demand, fill=month)) +
  geom_col() +
  geom_text(aes(label=bachelor_time_demand),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title="Distribution of Bachelors Demand According to Month")

#Analysis 6-4 : Determine the month where families usually start finding houses

#Calculate the number of rows for each month
family_time <- function(month)
{
  nrow(house_data[(house_data$`Preferred Tenants` == "family" | 
                     house_data$`Preferred Tenants`== "bachelors/family")
                  & house_data$Month == month,])
}

family_april <- family_time('04')
family_may <- family_time('05')
family_june <- family_time('06')
family_july <- family_time('07')

family_time_demand <- c(family_april, family_may, family_june, family_july)
month <- c('April','May', 'June', 'July')

family_month <- data.frame(month, family_time_demand)

#Plot radial chart
ggplot(family_month) +
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(0:1) * 1000),
    color= "lightgrey") +
  geom_col(aes(
    x = reorder(str_wrap(month, 4), month),
    y = family_time_demand,
    fill = month),
    position = "dodge2",
    show.legend = TRUE, 
    alpha= .9) +
  geom_segment(aes(
    x = reorder(str_wrap(month, 4), month),
    y = 0,
    xend = reorder(str_wrap(month, 4), month),
    yend = 2000),
    linetype = "dashed",
    color = "gray12") +
  coord_polar() +
  labs(title = "Distribution of Families Demand According to Month") +
  ylab("Number of Families") +
  xlab("Month")

#Analysis 6-5: Determine the city that has the most demand during April

#Transform to percentage
city_apr_dist <- house_data %>%
  filter(Month == "04") %>%
  group_by(`City Located`) %>%
  count() %>%
  ungroup() %>% 
  mutate(city_apr = `n` / sum(`n`)) %>% 
  arrange(city_apr) %>%
  mutate(labels = scales::percent(city_apr))

city_apr_dist

#Plot pie chart
ggplot(city_apr_dist, aes(x="", y = city_apr, fill = `City Located`)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title="Demand of Properties for Each City In April 2022")

#Analysis 6-6: Determine the city that has the most demand during May

#Transform to percentage
city_may_dist <- house_data %>%
  filter(Month == "05") %>%
  group_by(`City Located`) %>%
  count() %>%
  ungroup() %>% 
  mutate(city_may = `n` / sum(`n`)) %>% 
  arrange(city_may) %>%
  mutate(labels = scales::percent(city_may))

#Plot pie chart
ggplot(city_may_dist, aes(x="", y = city_may, fill = `City Located`)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title="Demand of Properties for Each City In May 2022")

#Analysis 6-7: Determine the city that has the most demand during June

#Transform to percentage
city_june_dist <- house_data %>%
  filter(Month == "06") %>%
  group_by(`City Located`) %>%
  count() %>%
  ungroup() %>% 
  mutate(city_june = `n` / sum(`n`)) %>% 
  arrange(city_june) %>%
  mutate(labels = scales::percent(city_june))

#Plot tree map
ggplot(city_june_dist, aes(area = n, fill = n, label = paste(`City Located`, 
                                                            labels, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre", 
                    size = 15) +
  scale_fill_gradient(low = "orange", high = "purple") +
  ggtitle("Demand of Properties for Each City In June 2022") +
  labs(fill = "Total Properties")

#Analysis 6-8: Determine the city that has the most demand during July

#Transform to percentage
city_july_dist <- house_data %>%
  filter(Month == "07") %>%
  group_by(`City Located`) %>%
  count() %>%
  ungroup() %>% 
  mutate(city_jul = `n` / sum(`n`)) %>% 
  arrange(city_jul) %>%
  mutate(labels = scales::percent(city_jul))

#Plot tree map
ggplot(city_july_dist, aes(area = n, fill = n, label = paste(`City Located`, 
                                                             labels, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre", 
                    size = 15) +
  scale_fill_gradient(low = "turquoise", high = "gold") +
  ggtitle("Demand of Properties for Each City In July 2022") +
  labs(fill = "Total Properties")

#Analysis 6-9: Determine the price category of houses according to month

#Aggregate the mean rent price according to the city located and month
month_houses <- house_data %>%
  group_by(`City Located`, Month) %>%
  dplyr::summarise(city_month_mean=round(mean(`Rent Price`)), .groups = 'drop') 
month_houses

#Plot stacked bar
ggplot(month_houses, aes(x=Month, fill=`City Located`)) +
  geom_bar(aes(weight=city_month_mean), position="stack") +
  geom_text(position = "stack", aes(Month,city_month_mean,label=city_month_mean), size = 5) +
  labs(title="Distribution of The Mean of House Rent for Each City Within Four Months", y="Mean Rent")

#Extra Feature 1: par()

#View distribution of all numerical columns (Data Exploration)

temp_house_data <- data.frame(house_data$`BHK In Unit`,house_data$`Unit Size`,
                              house_data$`Rent Price`, house_data$`Number of Bathroom`)
head(temp_house_data)

par(mfrow = c(2,2))
loop_vector <- 1:4

for (i in loop_vector)
{
  x <- temp_house_data[,i]
  
  hist(x, main = paste("Distribution of column", i),
       xlab = names(temp_house_data[ ,i]))
}

#par() is used to format the histograms to be displayed in the 2 x 2 manner

#Extra Feature 2: duplicated()

#Finding duplicate values
duplicated(house_data)

#duplicated() is used to find duplicate values

#Extra Feature 3: gsub()

#Remove unnecessary keywords such as "area" and "contact"
house_data$`Type of Area` <- gsub(' area','',as.character(house_data$`Type of Area`))
house_data$`Contact Person` <- gsub('contact ','',as.character(house_data$`Contact Person`))

#gsub() is used to replace values of the stated string to a new string

#Extra Feature 4: log()

#Transform rent price to view distribution
logged_price <- log(house_data$`Rent Price`)
boxplot(logged_price ~ house_data$`City Located`)

#log() is to calculate the logged values of a specific vectors

#Extra Feature 5: ggcorrplot()

#Looking into data correlation
house_corr <- select(house_data,`BHK In Unit`,`Rent Price`,`Unit Size`,`Number of Bathroom`,
                     `Price Per Square Feet`,`Total Number of Floors`)
corr <- round(cor(house_corr),1)
corrp.mat <- cor_pmat(house_corr)
ggcorrplot(corr, method ="square", hc.order = TRUE, lab=TRUE) +
  labs(title="Correlation Plot for House Data")

#ggcorrplot() is used to plot the correlation matrix according to the data given

#Extra Feature 6: ntile()

#Bin floor data
binned_data <- house_data %>% mutate("Floor Bin" = ntile(house_data$Floor, n = 5))
binned_data$`Floor Bin`

#ntile() is used to separate the values into several bins

#Extra Feature 7: kmeans() and fviz_cluster()

model <- kmeans(cluster_column, 5, nstart = 50)
print(model)
fviz_cluster(model, data = cluster_column)

#Check the count for each cluster
model$size

#Check cluster means
model$centers

#kmeans() implements the k-means clustering algorithm to the data passed
#fviz_cluster() visualizes the clusters in the form of cluster plots

#Extra Feature 8: str_split_fixed() (Data Transformation)

#Parse the floor number and the total floor number
house_data[c('Floor', 'Total Number of Floors')] <- str_split_fixed(house_data$`Floor Number`,' out of ', 2)
unique(house_data$Floor)
unique(house_data$`Total Number of Floors`)

#str_split_fixed() is used to separate the values in one column into several separate columns

#Extra Feature 9: fviz_nbclust() (WSS Method)

fviz_nbclust(cluster_column, kmeans, method = "wss")

#fviz_nbclust() is used to determine the optimal number of clusters

#Extra Feature 10: map_data()

india <- map_data("world", region = "india")

#map_data() provides the coordinates of a country's map so that it can be plotted

#Extra Feature 11: ggdotchart()

ggdotchart(fam_area, x = "area_label", y = "area_type_fam",
           color="area_label",
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
           sorting = "descending",                       
           rotate = TRUE,                                                              
           y.text.col = TRUE,
           dot.size = 15,                                 
           label = area_type_fam,                       
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),  
           ggtheme = theme_pubr()                       
)+
  theme_cleveland() +
  ggtitle("Distribution of The Area Types for Families") +
  ylab("Number of Families")

#ggdotchart() is used to plot the Cleveland's dot plot

#Extra Feature 12: Donut Plot

# Compute percentages
fam_floor$fraction = fam_floor$floor_fam / sum(fam_floor$floor_fam)

# Compute the cumulative percentages (top of each rectangle)
fam_floor$ymax <- cumsum(fam_floor$fraction)

# Compute the bottom of each rectangle
fam_floor$ymin <- c(0, head(fam_floor$ymax, n=-1))

# Compute label position
fam_floor$labelPosition <- (fam_floor$ymax + fam_floor$ymin) / 2

# Compute a good label
fam_floor$label <- paste0(fam_floor$label_fam, "\n value:", fam_floor$floor_fam)

ggplot(fam_floor, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=label_fam)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Distribution of the Preference of Floors Among Families")

#Extra Feature 13: Radial Plot

ggplot(bathroom_fam_tab) +
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(0:1) * 1000),
    color= "lightgrey") +
  geom_col(aes(
    x = reorder(str_wrap(bathroom_label_fam, 8), bathroom_number_fam),
    y = bathroom_number_fam,
    fill = bathroom_label_fam),
    position = "dodge2",
    show.legend = TRUE, 
    alpha= .9) +
  geom_segment(aes(
    x = reorder(str_wrap(bathroom_label_fam, 8), bathroom_number_fam),
    y = 0,
    xend = reorder(str_wrap(bathroom_label_fam, 8), bathroom_number_fam),
    yend = 2000),
    linetype = "dashed",
    color = "gray12") +
  coord_polar() +
  labs(title = "Number of Bathrooms Preferred By Families", ylab = "Count",
       xlab="Number of Bathrooms")

#Extra Feature 14: Tree Map

ggplot(city_june_dist, aes(area = n, fill = n, label = paste(`City Located`, 
                                                             labels, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre", 
                    size = 15) +
  scale_fill_gradient(low = "orange", high = "purple") +
  ggtitle("Demand of Properties for Each City In June 2022") +
  labs(fill = "Total Properties")
