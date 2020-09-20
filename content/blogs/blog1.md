---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: Predicting the cost of a four night stay in Cape Town
draft: false
image: cape town.jpg
keywords: ""
slug: ipsum
title: Airbnb Regression Analysis
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r, include=FALSE}
library(vroom)
library(tidyverse)
library(mosaic)
library(janitor)
library(skimr)
library(broom)
library(lubridate)
library(GGally)
library(leaflet)
library(ggfortify)
library(huxtable)
library(car)
library(kableExtra)
library(data.table)
library(modelr)
```


# Executive Summary 

In this exercise, we analyse listings from Airbnb bookings (via insideairbnb.com) to better understand price patterns within a particular city. The city we examined was Cape Town and we tried to predict the total cost of four nights for two people
To kick things off, we performed EDA to examine the data in more detail. Next, we subset the data and performed feature engineering to create new features which may suitable for further modeling.
From this exercise, we move to regression analysis where we iteratively test out a few indicators to see if they are significant in explaining the variability within the dataset.
We build up on this to create a final model of 7 covariates (property type, room type, review ratings scores,  cancellation policy,  how many people the property accommodates, which neighbourhood the property is in and if the property has air conditioning installed. This model achieves a **R squared** of **0.512**. Finally, we use this model to make a prediction on unseen problem.


# Exploratory Data Analysis 


```{r, cache=TRUE, echo=FALSE}
data <- vroom::vroom("http://data.insideairbnb.com/south-africa/wc/cape-town/2020-06-21/data/listings.csv.gz", na=c("", "N/A")) %>%
    clean_names()
```

## Data background 

This data set contains information on publicly available Airbnb listings in the city of Cape Town, South Africa.
It was retreived from insideairbnb.com
Inside Airbnb is an independent, non-commercial website that offers a set of tools and data that allows you to explore how Airbnb is being used in cities around the world.
The original data was scraped from the the Airbnb website (www.airbnb.com).
The  raw scraping data was then analyzed, cleaned and aggregated where appropriate to faciliate the public discussion and made available here:
http://data.insideairbnb.com/south-africa/wc/cape-town/2020-06-21/data/listings.csv.gz

## Looking at the raw values 

First, we will take a look at our data. 
```{r}
glimpse(data)
```

```{r,include=FALSE}
skim(data)%>%
  kable()%>%
  kable_styling()

```

In the original dataset, there are 24062 observations and 106 variables. There are 41 numerical variables and 43 categorical/factor variables as seen in the table above. 



## Variable Selection 
In order to fit our model, we will remove columns that are redundant, of poor data quality, or that cannot be used to explore our dependent variable, price for four nights for 2 people.

```{r selecting data}

#Vector of all columns we want to exclude
columns_deselect <- c(3:19, 20:22, 24:25, 27:28, 30:36, 38:39, 41:48, 58, 60, 62:63, 69:82, 84, 94:95, 96, 98, 100:105)

#Deselecting all columns we dont need
listings <- data %>%
  select(-columns_deselect)

```


In the table below, we outline our reasoning for removing some variables. 

```{r, echo=FALSE}
#load file with excluded variables
deselected_variables <- read_csv(here::here("variables.csv"))
    glimpse(deselected_variables)
    
```

```{r}
kable(deselected_variables[1:2]) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, font_size = 14, position = "center") %>%
  scroll_box(width = "910px", height = "400px")

```


## Data Wrangling 


In order to run models with our data, we need to modify some variables and create some new variables that we want to examine. 

First, we must change the price variables (price, cleaning_fee, extra_people, and security_deposit) to numeric variables, as they are now characters. 

```{r,warning=FALSE}
# Changing price variables to numeric 
listings <- listings %>% 
  mutate(price = parse_number(price),
    cleaning_fee = parse_number(cleaning_fee),
    extra_people = parse_number(extra_people),
    security_deposit = parse_number(security_deposit))

# Check if these variables are numeric, whether we have any zero prices for `price` or there are any NAs
listings %>% 
  select(price, cleaning_fee, extra_people) %>% 
  skim()%>%
  kable()%>%
  kable_styling()
```

We will also change host_response_time to a factor variable, that is ordered based on how long a host takes to respond. That way we can determine whether there's a relationship between response time length and price. 
```{r}

# Changing host_response_time to a factor variable 

listings <- listings%>% 
  mutate(host_response_time = factor(host_response_time, 
  order = TRUE, levels = c("within an hour", "within a few hours", "within a day",
                           "a few days or more")))
```

We will also turn the listing ID's into character strings rather than numeric values. 
```{r}

#Turn IDs into string

listings <- listings %>% 
  mutate(id = as.character(id),
         host_id = as.character(id))

```


We create a new variable called host_experience_y which tells us the number of years a host is active on Airbnb. We want to examine  how long a host has been active impacts how much they might charge for a listing. 

```{r}

#max_date in host_since = 2020-06-17

# Creating a new variable call host_experience_y which tells us about the no of years the host is active on Airbnb

listings <- listings %>% 
  mutate(host_experience_y = as.numeric(ymd("2020-06-17") - host_since)/365)


```

For security_deposit, since there are 8559 missing values, we will treat the missing values as having no security deposit.  We will also convert to a dummy variable where if the listing has a security deposit the variable will be TRUE, an if not, FALSE. We do this because there is a massive range of security deposits and the distribution is very skewed. This will help us determine whether having a security deposit influences the price. 

```{r}
listings%>%
  skim(security_deposit)%>%
  kable()%>%
  kable_styling()

kable(favstats(~security_deposit, data=listings))%>%
  kable_styling()


```

```{r}
#turning security_deposit into a dummy variable 
listings <- listings %>%
  mutate(security_deposit = case_when(
    is.na(security_deposit) ~ 0,
    TRUE ~ security_deposit
  )) %>%
  mutate(security_deposit=case_when(
    security_deposit==0~FALSE,
    TRUE~TRUE
  ))
```


Next, we look at cancellation_policy. It looks like there are several types of cancellation policies that only have one or 2 listings with that category based on the count below. Additionally, to simplify, we can group all of the policies into three main categories: flexible, moderate, and strict. 

```{r}
listings %>%
  count(cancellation_policy)%>%
   kable()%>%
  kable_styling()

#We first convert cancellation policy into a 3-category variable where cancellation policy can be flexible, moderate, and strict. 

listings<- listings %>%
  mutate(
    cancellation_policy = case_when (cancellation_policy %in% c("flexible") ~"flexible", cancellation_policy %in% c("luxury_moderate", "moderate") ~"moderate", 
cancellation_policy %in%c("luxury_super_strict_125","luxury_super_strict_95",
                          "strict","strict_14_with_grace_period","super_strict_30",
                          "super_strict_60")~"strict"),
  )

```


We want to examine whether having air conditioning affects the listing price, as we noticed that only around 30% of properties have air conditioning installed. We introduce a new variable that evaluates to TRUE if string 'Air conditioning' is included in amenities, and FALSE otherwise.

```{r}
#creating the A/C variable
listings <- listings %>% 
  mutate(ac=str_detect(amenities,'Air conditioning'))

proportions<-listings %>% 
  group_by(ac) %>% 
  summarise(count=n()) %>% 
  mutate(proportion=count/sum(count))
proportions%>%
    kable()%>%
  kable_styling()
```
It seems that having Air conditioning in Cape Town is some sort of a luxury. Furthermore, according to website Statista, only 6% of households in South Africa have air conditioning (Global AC penetration rate by country 2016 | Statista, 2020).


We will also convert the amenities variable to be a count of the number of amenities in a certain property, rather than comma separated values. 
```{r}
listings<- listings %>% 
  mutate(total_amenities=str_count(amenities, ',')+1)
```

Additionally, we simplify the neighbourhood_cleansed by condensing them into the new variable neighborhood_simplified. Cape Town is separated into over 100 Wards, which would be difficult to include in our model. Through our research, we found that Cape Town is also segmented into 24 sub-councils. We assigned each ward to one of the 24 sub-councils following official data from capetown.gov.za. Then, based on an interview with a Cape Town local, we assigned each sub-council a grade corresponding to cost of living and quality of life, with the levels: best, better, good, and standard. 

```{r}

# Cape Town is made up of 100+ diffeerent Wards. 
# The column neighborhood_cleansed contains information on which Ward the listing is located in
# We first assign each Ward to 1 of the 24 sub-councils that make up Cape Town, following official data from the Cape Town website
# (http://resource.capetown.gov.za/documentcentre/Documents/Maps%20and%20statistics/Population_and_Households_by_Ward_2001_and_2011.pdf)



#Vector for each sub district

SC01 <-c("Ward 23",	"Ward 29",	"Ward 32",	"Ward 104")
SC02 <-c("Ward 6",	"Ward 7",	"Ward 8",	"Ward 101",	"Ward 102",	"Ward 111")
SC03 <-c("Ward 1",	"Ward 4",	"Ward 5",	"Ward 70",	"Ward 107",	"Ward 113")
SC04 <-c("Ward 25",	"Ward 26",	"Ward 27",	"Ward 28",	"Ward 30")
SC05 <-c("Ward 13",	"Ward 20",	"Ward 24",	"Ward 31",	"Ward 50",	"Ward 106")
SC06 <-c("Ward 2",	"Ward 3",	"Ward 9",	"Ward 10",	"Ward 12",	"Ward 22")
SC07 <-c("Ward 21",	"Ward 103",	"Ward 105",	"Ward 112")
SC08 <-c("Ward 83",	"Ward 85",	"Ward 86",	"Ward 100")
SC09 <-c("Ward 18",	"Ward 87",	"Ward 89",	"Ward 90",	"Ward 91",	"Ward 116")
SC10 <-c("Ward 92",	"Ward 93",	"Ward 94",	"Ward 97",	"Ward 98",	"Ward 99")
SC11 <-c("Ward 40",	"Ward 44",	"Ward 46",	"Ward 47")
SC12 <-c("Ward 78",	"Ward 79",	"Ward 81",	"Ward 82")
SC13 <-c("Ward 34",	"Ward 35",	"Ward 36",	"Ward 80",	"Ward 88")
SC14 <-c("Ward 37",	"Ward 38",	"Ward 39",	"Ward 41",	"Ward 42",	"Ward 45")
SC15 <-c("Ward 51",	"Ward 52",	"Ward 53",	"Ward 55",	"Ward 56")
SC16 <-c("Ward 54",	"Ward 57",	"Ward 74",	"Ward 77",	"Ward 115")
SC17 <-c("Ward 48",	"Ward 49",	"Ward 60")
SC18 <-c("Ward 63",	"Ward 65",	"Ward 66",	"Ward 67",	"Ward 68",	"Ward 110")
SC19 <-c("Ward 61",	"Ward 64",	"Ward 69")
SC20 <-c("Ward 58",	"Ward 59",	"Ward 62",	"Ward 71",	"Ward 72",	"Ward 73")
SC21 <-c("Ward 11",	"Ward 19",	"Ward 108")
SC22 <-c("Ward 14",	"Ward 16",	"Ward 17",	"Ward 114")
SC23 <-c("Ward 33",	"Ward 43",	"Ward 75",	"Ward 76")
SC24 <-c("Ward 15",	"Ward 84",	"Ward 95",	"Ward 96",	"Ward 109")
```

```{r}


# Based on an interview with a local, we then assigned the 24 sub-council a grade corresponding to quality of life 

# vector for living quality

Best <-c(SC16, SC19, SC20)
Better <-c(SC01, SC08, SC15, SC17)
Good <-c(SC02, SC03, SC04, SC05, SC06, SC07, SC08, SC15, SC21, SC22, SC23, SC24)
Standard <-c(SC09, SC10, SC11, SC12, SC13, SC14, SC17, SC18)

```

```{r}
# Next, we create two new variables: "sub_council", containing the sub council for each listing and "neighborhood_simplified", classing the location of the listing to either standard, good, better or best.
#assigning sub council

listings <- listings %>% 
  mutate(sub_council = case_when(neighbourhood_cleansed %in% SC01 ~ "SC01",
                                 neighbourhood_cleansed %in% SC02 ~ "SC02",
                                 neighbourhood_cleansed %in% SC03 ~ "SC03",
                                 neighbourhood_cleansed %in% SC04 ~ "SC04",
                                 neighbourhood_cleansed %in% SC05 ~ "SC05",
                                 neighbourhood_cleansed %in% SC06 ~ "SC06",
                                 neighbourhood_cleansed %in% SC07 ~ "SC07",
                                 neighbourhood_cleansed %in% SC08 ~ "SC08",
                                 neighbourhood_cleansed %in% SC09 ~ "SC09",
                                 neighbourhood_cleansed %in% SC10 ~ "SC10",
                                 neighbourhood_cleansed %in% SC11 ~ "SC11",
                                 neighbourhood_cleansed %in% SC12 ~ "SC12",
                                 neighbourhood_cleansed %in% SC13 ~ "SC13",
                                 neighbourhood_cleansed %in% SC14 ~ "SC14",
                                 neighbourhood_cleansed %in% SC15 ~ "SC15",
                                 neighbourhood_cleansed %in% SC16 ~ "SC16",
                                 neighbourhood_cleansed %in% SC17 ~ "SC17",
                                 neighbourhood_cleansed %in% SC18 ~ "SC18",
                                 neighbourhood_cleansed %in% SC19 ~ "SC19",
                                 neighbourhood_cleansed %in% SC20 ~ "SC20",
                                 neighbourhood_cleansed %in% SC21 ~ "SC21",
                                 neighbourhood_cleansed %in% SC22 ~ "SC22",
                                 neighbourhood_cleansed %in% SC23 ~ "SC23",
                                 neighbourhood_cleansed %in% SC24 ~ "SC24"
                                 ))
```

```{r}
# Assigning neighbourhood simplified

listings <- listings %>% 
  mutate(neighbourhood_simplified = case_when(neighbourhood_cleansed %in% Best ~ "best",
                                              neighbourhood_cleansed %in% Better ~ "better",
                                              neighbourhood_cleansed %in% Good ~ "good",
                                              neighbourhood_cleansed %in% Standard ~ "standard"))


```


Now we will take a look at the minimum_nights variable and filter only to include listings where the minimum number of nights is less than or equal to 4. 
```{r}
#To check the 5 most common values for the variable `minimum_nights`
listings%>%
  count(minimum_nights)%>% 
  arrange(desc(n))%>%
  kable()%>%
  kable_styling()


#To filter the variable `minimum_nights`
listings <- listings %>%
  filter(minimum_nights<= 4)

```

The top 5 most common values are for minimum nights are: 2, 1, 3, 7 and 5. The top values seem to be a week or less, which means most listings are probably used for tourism. However, some of the less common values also offer minimum stays of 30 or 365 days which demonstrates that some listings can be monthly or yearly rentals. 


Finally we will inspect the accommodates variable. We want to filter out the 737 listings that can only accommodate one person, since we are interested in places where two people could rent. 

```{r}

#To check the 5 most common values for the variable accommodates
listings %>%
  count(accommodates) %>%
  arrange(desc(accommodates))%>%
  kable()%>%
  kable_styling()

#as we can see, the data set includes 737 listings for only 1 person
#Because we are only interested in offerings that can accommodate 2 people, we filter

#To filter the variable `accommodates`
listings <- listings %>%
  filter(accommodates >= 2)

```



## Handling NAs 

Some of our variables have missing values. Cleaning fee, for example, which we will use to calculate price for four nights, has 7049 missing values, as seen below. This is most likely because these listings do not charge a cleaning fee, so we will treat N/A values as 0.  

```{r}
listings%>%
skim(cleaning_fee)%>%
  kable()%>%
  kable_styling()

listings <- listings %>%
  mutate(cleaning_fee = case_when(
    is.na(cleaning_fee) ~ 0, 
    TRUE ~ cleaning_fee
  ))


```

## Creating our dependent variable: price_4_nights 

```{r}
listings <- listings%>%
  filter(guests_included<=2)%>% 
  mutate(
    extra_charge_pp= case_when(guests_included==2 ~ 0, guests_included ==1 ~ extra_people),
    price_4_nights= price*4 + extra_charge_pp*4 + cleaning_fee
  )
  
```

## Computing summary statistics and visualizing the distribution of the variables of interest 

Now, we will look at some of the variables that we want to examine when we eventually build our model. We want to look at how the variables are distributed, and whether there is any correlation between explanatory variables. 

First, we will examine the host_experience_y variable that we created, which demonstrates how long a host has been listed on Airbnb. 

```{r}

#Descriptive Analysis

kable(favstats(~host_experience_y, data=listings))%>%
  kable_styling()

skim(listings$host_experience_y)%>%
  kable()%>%
  kable_styling()

ggplot(listings, aes(x=host_experience_y)) +
  geom_density(fill = "lavender") + 
  labs(x = "Host Experience", y="Density", title= "Host Experience Density Plot") +
  theme_minimal()


```
Based on the density plot, it looks like host_experience_y is a multi-modal distribution which is slightly right skewed. The mean is 4.13 years and the median is 4.27. 

Next, we will look at host_is_superhost. We might want to use this in our model to examine whether superhosts tend to charge a premium.  
```{r}
skim(listings$host_is_superhost)%>%
  kable()%>%
  kable_styling()

listings %>%
  count(host_is_superhost)%>%
  kable()%>%
  kable_styling()

ggplot(listings, aes(x = host_is_superhost)) +
  geom_bar(width = 0.5, color = "blue", fill = "#77a5f0") +
  labs(x = "Super Host", y="Density", title= "Distribution of Superhosts") +
  theme_minimal() 


```

Based on the data above, roughly 1/4 of all hosts in Cape Town are super hosts.


Now, we look at number_of_reviews, which represents the total number of reviews on a particular listing. 
```{r}
listings%>%
  skim(number_of_reviews)%>%
  kable()%>%
  kable_styling()

kable(favstats(~number_of_reviews, data=listings))%>%
  kable_styling()
```

```{r}
listings%>%
  ggplot(aes(x=number_of_reviews))+
  geom_histogram(aes(x = number_of_reviews), color = "blue", fill = "#77a5f0") +
  labs(x = "Number of Reviews", y="Count", title= "Total Number of Reviews") +
  ggtitle("Total Number of Reviews") +
  coord_cartesian(xlim = c(0, 220)) +
  theme_minimal()
```

Based on the histogram, number of reviews is very skewed. It seems like the majority of listings do not have many reviews. The median is 3, yet the mean is 14.42, indicating that the variable is right-skewed. 



Now we'll look at the review_scores_rating variable, as we want to examine whether listings with higher reviews charge a premium. 

```{r}
  kable(favstats(~review_scores_rating, data=listings))%>%
  kable_styling()

```
There are 7314 missing values for review_scores_rating, which most likely means that those listings did not receive a review. The median rating is 97 and the mean rating is 94.58. The standard deviation is 8.87.

Now let's visualize the distribution: 
```{r, warning=FALSE}
listings%>%
  ggplot(aes(x=review_scores_rating))+
  geom_density(fill="lavender") +
  labs(x = "Review Score Rating") +
  ggtitle("Distribution of Review Score Rating.") +
  theme_minimal()
```
The distribution is left-skewed with most of the ratings in the 90's. There is a small second peak at 80. 

The following variables are reviews for individual categories. They are all discrete values from 0-10 and exhibit similar distributions. We will keep them all to explore whether some variables impact price more than others. 

```{r}
accuracy <- listings%>%
  ggplot(aes(x=review_scores_accuracy)) +
  geom_bar(width = 0.5, color = "blue", fill="#77a5f0") +
  labs(x = "Review Score Accuracy") +
  ggtitle("Accuracy") +
  theme_minimal()
```

```{r}
cleanliness <- listings%>%
  ggplot(aes(x=review_scores_cleanliness))+
  geom_bar(width = 0.5, color = "blue", fill="#77a5f0") +
  labs(x = "Review Score Cleanliness") +
  ggtitle("Cleanliness") +
  theme_minimal()
```

```{r}
communication <- listings%>%
  ggplot(aes(x=review_scores_communication))+
  geom_bar(width = 0.5, color = "blue", fill="#77a5f0") +
  labs(x = "Review Score Communication") +
  ggtitle("Communication") +
  theme_minimal()
```

```{r}
check_in <- listings%>%
  ggplot(aes(x=review_scores_checkin))+
  geom_bar(width = 0.5, color = "blue", fill="#77a5f0") +
  labs(x = "Review Score Check-in") +
  ggtitle("Check-in") +
  theme_minimal()
```

```{r}
location <- ggplot(listings, aes(x = review_scores_location)) +
  geom_bar(width = 0.5, color = "blue", fill = "#77a5f0") +
  labs(x = "Review Score Location") +
  ggtitle("Location") +
  theme_minimal() +
  NULL
```

```{r}
value <- ggplot(listings, aes(x = review_scores_value)) +
  geom_bar(width = 0.5, color = "blue", fill = "#77a5f0") +
  labs(x = "Review Score Value") +
  ggtitle("Value") +
  theme_minimal() +
  NULL
```

```{r, warning=FALSE}
library(gridExtra)
grid.arrange(accuracy, cleanliness, check_in, communication, location, value, nrow = 3, ncol = 3)

```


We also want to look at whether if a property has an instant booking option it impacts price. It looks like, based on the table below, properties are pretty evenly split between the two options. 

```{r}

listings %>%
skim(instant_bookable)%>%
  kable()%>%
  kable_styling()
```

```{r}
ggplot(listings, aes(x = instant_bookable)) +
  geom_bar(width = 0.5, color = "blue", fill = "#77a5f0") +
  labs(x = "Instant Booking") +
  ggtitle("Count of listings with or without Instant Booking") +
  theme_minimal() +
  NULL
```



We also want to explore the property_type variable. 

```{r}
#To check the top 4 categories of property type in Cape Town
listings %>%
count(property_type) %>%
arrange(desc(n))%>%
  kable()%>%
  kable_styling()
```

As we can see, the four most common listings (Apartment, House, Guest Suite, and Villa) make up ~80% of all listings. Thus, to simplify, we will create a new variable with five categories: apartment, house, guest suite, villa, and other. 

```{r}
#To simplify the `property_type` variable into 5 categories, Mutate others to gather the other property type with small values
listings <- listings %>%
  mutate(prop_type_simplified = case_when(
    property_type %in% c("Apartment","House","Guest suite","Villa","Guest House","Bed and breakfast") ~ property_type, 
    TRUE ~ "Other"
  ))

listings %>% 
  ggplot(aes(x=prop_type_simplified)) +
  labs(x = "Property Type") +
  ggtitle("Simplified Property Type") +
  geom_bar(width = 0.5, color = "blue", fill = "#6dccc1") +
  theme_minimal()
```



Finally, we examine the distribution of listings based on quality of life in the area that they are located. 

```{r}

#count by neighborhood
listings %>%
  count(neighbourhood_simplified) %>%
  arrange(desc(n))%>%
  kable()%>%
  kable_styling()

ggplot(listings, aes(x = neighbourhood_simplified)) +
  geom_bar(width = 0.5, color = "blue", fill = "#77a5f0") +
  labs(x = "Neighborhood", y="Count", title="Distribution of listings based on cost of living") +
  theme_minimal()

```

The majority of the Airbnb listings seem to be located in Wards that are classified as "best" in regards to location. These are likely to be popular destinations for tourists. 



## Examining correlations between variables 

Now we will take a look at some of the relationships between the explanatory variables that we might want to include in our model, to see if anything stands out. 
```{r, warning=FALSE}

# To see the correlations between variables
listings%>%
  select(bedrooms, bathrooms, accommodates, review_scores_rating, host_experience_y, price_4_nights) %>% 
  ggpairs(alpha=0.4) +
  theme_minimal()

```
Based on the correlation matrix above, it looks like the variables bathroom,bedrooms, and accommodates are highly correlated. We most likely will not want to include all of them in our model.

Now, we will look at the relationship between property type and price. 
```{r, warning=FALSE}

listings %>%
  ggplot(aes(x=prop_type_simplified, y= price_4_nights)) +
           geom_boxplot(aes(fill=prop_type_simplified), show.legend=FALSE)+
           theme_minimal()+
          labs(x="Property Type", title="Property Type and Price for Four Nights")+
          scale_y_continuous(name="Price for 4 nights", limits = c(0,60000))
```

Based on the plot, it seems like Villa and House might be indicators of a higher price. We will test them when we build our model. 

We will also examine neighborhood in the plot below. 
```{r, warning=FALSE}

listings %>%
  ggplot(aes(x=neighbourhood_simplified, y= price_4_nights)) +
           geom_boxplot(aes(fill=neighbourhood_simplified), show.legend=FALSE)+
           theme_minimal()+
          labs(x="Neighbourood", title="Neighbourhood and Price for Four Nights")+
          scale_y_continuous(name="Price for 4 nights", limits = c(0,60000))
```
As expected, it seems like listings in the "best" neighbourhoods are most expensive, while those in "standard" are cheapest. We will test whether this effect is significant when we build our model. 

We will also look at host_is_superhost. 
```{r, warning=FALSE}

listings %>%
  ggplot(aes(x=host_is_superhost, y= price_4_nights)) +
           geom_boxplot(aes(fill=host_is_superhost, show.legend=FALSE))+
           theme_minimal()+
          labs(x="Superhost", title="Superhost and Price for Four Nights")+
          scale_y_continuous(name="Price for 4 nights", limits = c(0,60000))
```
There does not seem to be a large distinction between whether a host is a superhost and the price of the listing, but we will confirm when we test our model. 

We will also take a look at cancellation_policy. 
```{r, warning=FALSE}

listings %>%
  ggplot(aes(x=cancellation_policy, y= price_4_nights)) +
           geom_boxplot(aes(fill=cancellation_policy, show.legend=FALSE))+
           theme_minimal()+
          labs(x="Cancellation Policy", title="Cancellation Policy and Price for Four Nights")+
          scale_y_continuous(name="Price for 4 nights", limits = c(0,60000))
```
It seems like listings with stricter policies are slightly more expensive, which could be because they are more desirable listings. We will test the significance in our model fitting. 

Finally, we will examine the air conditioning variable that we created. 
```{r, warning=FALSE}

listings %>%
  ggplot(aes(x=ac, y= price_4_nights)) +
           geom_boxplot(aes(fill=ac, show.legend=FALSE))+
           theme_minimal()+
          labs(x="Air conditioning", title="Air conditioning and Price for Four Nights")+
          scale_y_continuous(name="Price for 4 nights", limits = c(0,60000))
       
```
As expected, it seems like listings that have air conditioning are more expensive. We will examine the effect as we construct our model. 


# Mapping 

Visualizing the map of listings in Cape Town 

```{r, out.width = '100%'}
#creating the map
leaflet(data = filter(listings, minimum_nights <= 4,accommodates>=2)) %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircleMarkers(lng = ~longitude, 
                   lat = ~latitude, 
                   radius = 1, 
                   fillColor = "blue", 
                   fillOpacity = 0.4, 
                   popup = ~listing_url,
                   label = ~property_type)
```

# Regression Analysis 


## Visualizing the distribution of price_4_nights
```{r}
#density plot price_4_nights
ggplot(data=listings, aes(x=price_4_nights)) +
  geom_density(fill = "lavender") +
  labs(x="log of Price for Four Nights") +
  theme_minimal()

#density plot log
ggplot(data=listings, aes(x=price_4_nights)) + 
  scale_x_log10()+geom_density(fill = "lavender") + 
  labs(x="log of Price for Four Nights") +
  theme_minimal()
```
Price_4_nights is very heavily positively skewed. Log (price_4_nights), on the other hand, is approximately normally distributed. Hence, log (price_4_nights) is a better candidate for further modeling.


### Model 1: prop_type_simplified, number_of_reviews, review_scores_rating 

Next, we fit a regression model with 3 explanatory variables: prop_type_simplified, number_of_reviews, and review_scores_rating. 
```{r}
# ***** Fit linear regression models: First, just the mean
model0 <- lm(log (price_4_nights) ~ 1, data= listings)

model0 %>% 
  broom::tidy(conf.int=TRUE)

model0 %>% 
  broom::glance()

# ***** Fit linear regression models: criminals on 3 explanatory variables.
model1 <- lm(log (price_4_nights) ~ prop_type_simplified + number_of_reviews + review_scores_rating, data= listings)

model1 %>% 
  broom::tidy(conf.int=TRUE)%>%
  kable()%>%
  kable_styling()

model1 %>% 
  broom::glance()%>%
 kable()%>%
  kable_styling()
```

Below, we interpret the coefficients of our model: 
review_scores_rating
For every 1 unit increase in review_scores_rating, the price_4_nights increases by 0.62% .
As (exp(0.006188)-1)*100 = 0.62%
prop_type_simplified
Price for 4 nights at a **Bed and Breakfast** is on average **10.2% lower** than the price for an apartment.
(Equivalent to (exp(-0.107467) - 1)*100) = (-10.2%)
Price for 4 nights at a **Guest Suite** is on average ** **34.4% lower** than the price for an apartment.
(Equivalent to (exp(-0.421721) - 1)*100) = -34.4%)
Price for 4 nights at a **House** is on average **13.8% higher** than the price for an apartment.
(Equivalent to (exp(0.129616) - 1)*100) = 13.8%)
Price for 4 nights at a **Villa** is on average **243.3% higher** than the price for an apartment.
(Equivalent to (exp(1.233319) - 1)*100)= 243.3%)


###  Model 2: adding room_type 

We want to determine if room_type is a significant predictor of the cost for 4 nights, given everything else in the model. 
```{r}

# ***** Fit linear regression models: criminals on 4 explanatory variables.
model2 <- lm(log (price_4_nights) ~ prop_type_simplified + number_of_reviews + review_scores_rating + room_type, data= listings)

model2 %>% 
  broom::tidy(conf.int=TRUE)%>%
   kable()%>%
  kable_styling()

model2 %>% 
  broom::glance()%>%
   kable()%>%
  kable_styling()
```
All else equal, room_typePrivate room and room_typeShared room are significant predictors of the cost for 4 night as it has T-statistics >2 and p-vale <0.05. 

room_typeHotel room does not seem to be significantly different from the baseline room_type variable ("Entire home/apt") as its |T statistic| < 2.

Now we will explore adding further variables. 

### Model 3: Improving model

We start our own exploration now. We first wonder if we can improve model 2.

```{r}
model3 <- lm(log (price_4_nights) ~ prop_type_simplified + review_scores_rating + room_type, data= listings)

model3 %>% 
  broom::tidy(conf.int=TRUE)%>%
  kable()%>%
  kable_styling()

model3 %>% 
  broom::glance()%>%
  kable()%>%
  kable_styling()

msummary(model3)
car::vif(model3)
autoplot(model3)
```

After taking number_of_reviews away, we find that there is almost no change in adjusted R-squared. Also, there is no change in collinearity and significance after eliminating number_of_reviews. So, we can get rid of number_of_reviews in our model.

### Model 4: Adding bathrooms, bedrooms, beds, accommodates

Now, let's take a look at the number of bathrooms, bedrooms, beds and the number of people they can accommodates. We want to know if they are significant in predicting price of 4 nights in Cape Town.

```{r}
model4 <- lm(log (price_4_nights) ~ prop_type_simplified + 
               review_scores_rating + room_type + bathrooms + bedrooms + beds + 
               accommodates, data= listings)

model4 %>% 
  broom::tidy(conf.int=TRUE)%>%
  kable()%>%
  kable_styling()

model4 %>% 
  broom::glance()%>%
  kable()%>%
  kable_styling()

msummary(model4)
car::vif(model4)
autoplot(model4)
```

Bathrooms, bedrooms, beds and accommodates are all significant predictors of the cost for 4 night as they all have T-statistics >2 and p-vale <0.05. That is to say, the more bathrooms/bedrooms/beds it has or the more people it can accommodates, the more expensive it is.

However, as we mentioned above, the four variables mentioned above are highly correlated with correlation of around 0.7. Considering about collinearity, we can only keep one of those variables. Noticing that accommodates has the biggest T-statistics, we decide to keep it.


### Model 5: Adding Air Conditioning

Since as stated earlier only 6% of the households in South Africa have air conditioning, we examine how significant it is in predicting the price (Statista, 2020).

```{r}
model5 <- lm(log (price_4_nights) ~ prop_type_simplified + review_scores_rating + room_type + accommodates + ac, data= listings)

model5 %>% 
  broom::tidy(conf.int=TRUE)%>%
  kable()%>%
  kable_styling()

model5 %>% 
  broom::glance()%>%
  kable()%>%
  kable_styling()

msummary(model5)
car::vif(model5)
autoplot(model5)
```

As we supposed, A/C is a significant predictor of the cost for 4 night as it has T-statistics >2 and p-vale <0.05! As we predict, if there is air conditioning in the property, it will be more expensive to rent.

Also, all variables' VIFs are smaller than 5, indicating that there is no collinearity. So we can accept A/C in our model.


### Model 6: Adding host_is_superhost

After discussing about the property itself, we also would like to see if the host is good. We think that superhosts would charge us more since they believe their properties are popular in the market.

```{r}
model6 <- lm(log (price_4_nights) ~ prop_type_simplified + review_scores_rating + room_type + accommodates + ac + host_is_superhost, data= listings)

model6 %>% 
  broom::tidy(conf.int=TRUE)%>%
  kable()%>%
  kable_styling()

model6 %>% 
  broom::glance()%>%
  kable()%>%
  kable_styling()

msummary(model6)
car::vif(model6)
autoplot(model6)
```


Unfortunately, we are wrong. Superhosts don't charge us more! We can see that is_super_host is NOT a significant predictor of the cost for 4 night as it has T-statistics <2 and p-vale >0.05.


### Model 7: Adding cancellation_policy

We also wonder would strict cancellation policy be an indicator of the price. Intuitively, the higher the price, the more the host would lose if guests canceled. Is that true for property in Cape Town?

```{r}
model7 <- lm(log (price_4_nights) ~ prop_type_simplified + review_scores_rating + room_type + accommodates + ac + cancellation_policy, data= listings)

model7 %>% 
  broom::tidy(conf.int=TRUE)%>%
  kable()%>%
  kable_styling()

model7 %>% 
  broom::glance()%>%
  kable()%>%
  kable_styling()

msummary(model7)
car::vif(model7)
autoplot(model7)
```


We can see that cancellation_policy is a significant predictor of the cost for 4 night as it has T-statistics <2 and p-vale >0.05. It is true that host will impose strict cancellation policy if the price of his property is high.

Also, all variables' VIFs are smaller than 5, indicating that there is no collinearity. So we can accept cancellation_policy in our model.


As mentioned previously, we have two ways to simplify neighbourhood information. Here, we compare two methods (Method 1 in model 8, Method 2 in model 9) and select the best to add to our model.


### Model 8: Adding neighbourhood information 

```{r}
model8 <- lm(log (price_4_nights) ~ prop_type_simplified + review_scores_rating + room_type + accommodates + ac + cancellation_policy + neighbourhood_simplified, data= listings)

model8 %>% 
  broom::tidy(conf.int=TRUE)%>%
  kable()%>%
  kable_styling()

model8 %>% 
  broom::glance()%>%
  kable()%>%
  kable_styling()

msummary(model8)
car::vif(model8)
autoplot(model8)
```

#  Final Model 

So, here is our final model. It includes the following explanatory variables: prop_type_simplified, review_scores_rating, room_type, accommodates, ac, cancellation_policy, and neighbourhood_simplified.

```{r}

model_final <- lm(log (price_4_nights) ~ prop_type_simplified + 
                    review_scores_rating +  room_type + accommodates + ac + 
                    cancellation_policy + neighbourhood_simplified, data= listings)

model_final %>% 
  broom::tidy(conf.int=TRUE)%>%
  kable()%>%
  kable_styling()

model_final %>% 
  broom::glance()%>%
  kable()%>%
  kable_styling()

msummary(model_final)

#To check the regression table, collinearity and residual
car::vif(model_final)
autoplot(model_final)
```

Below, we interpret the final model coefficients: 

**cancellation_policy**
For every 1 unit increase in review_scores_rating, there is an increase of **0.28%** in price_4_nights (Equivalent to (exp(0.00281) - 1)*100)

**property_type**
Price for 4 nights at a **Bed and Breakfast** is on average **18.0% higher** than the price for an apartment.
(Equivalent to (exp(0.16552) - 1)*100) = (18.0%)
Price for 4 nights at a **Guest Suite** is on average ** **13.9% cheaper** than the price for an apartment.
(Equivalent to (exp(-0.14981) - 1)*100) = -13.9%)
Price for 4 nights at a **House** is on average **9.5% higher** than the price for an apartment.
(Equivalent to (exp(0.09109) - 1)*100) = 9.5%)
Price for 4 nights at a **Villa** is on average **63.4% higher** than the price for an apartment.
(Equivalent to (exp(0.49078) - 1)*100)= 63.4%)

**cancellation_policy**
The price of a property with a moderate cancellation policy does not differ significantly from the price for a property with a flexible cancellation policy.
The price of a property with a strict cancellation policy is on average **14.7% higher** than the price for a property with a flexible cancellation policy.
(Equivalent to (exp(0.13723) - 1)*100)= 14.7%)

**room_type**
The price of a **Hotel room** is on average **8.3% higher** than the price for an **Entire home or apartment**.
(Equivalent to (exp(0.08017) - 1)*100)= 8.3%). However, the price is significant only at a 10% significance level.
The price of a **Private room** is on average **26.8% lower** than the price for an **Entire home or apartment**.
(Equivalent to (exp(-0.31320) - 1)*100)= -26.8%)
The price of a **Shared room** is on average **66.2% lower** than the price for an **Entire home or apartment**.
(Equivalent to (exp(-1.08399) - 1)*100)= -66.2%)

**accomodates**
For each additional person a property **accommodates**, the average increase in price for 4 nights is **18.2%**.
(Equivalent to (exp(0.16738) - 1)*100)= 18.2%)

**neighbourhood_simplified**
The price of a property in a **better neighbourhood** is on average **16.7% lower** than the price of a property in a best neighbourhood.
(Equivalent to (exp(-0.18313) - 1)*100)= -16.7%)
The price of a property in a **good neighbourhood** is on average **26.5% lower** than the price of a property in a best neighbourhood.
(Equivalent to (exp(-0.30820) - 1)*100)= -26.5%)
The price of a property in a **standard neighbourhood** is on average **40.7% lower** than the price of a property in a best neighbourhood.
(Equivalent to (exp(-0.52263) - 1)*100)= -40.7%)

**ac**
The price of a property with an **ac** is on average **35.7% higher** than the price of a property without one.
(Equivalent to (exp(0.30529) - 1)*100)= 35.7%)



##  Summary table of final model and rationale


```{r}
#creating comparison table
huxreg(model1,model2, model3, model4, model5, model6, model7, model_final,
       statistics = c('#observations' = 'nobs',
                      'R squared' = 'r.squared',
                      'Adj. R Squared' = 'adj.r.squared',
                      'Residual SE' = 'sigma'),
       bold_signif = 0.05,
       stars = NULL
) %>%
  set_caption('Comparison of models')
```

This table shows all of the coefficients and R squared of the models that we fit. Our final model has the highest R2 out of the models that we fit. 

##  Final model diagnostics 

We can see from "Residual Vs Fitted" that residuals are random, with no pattern and around Y=0, indicating that the the regression follows the linearity assumption.

In "Normal Q-Q" plot, we notice that residuals don't deviate from the straight line, so we can confirm that our regression follows the normality assumption.

Checking on "Scale-Location" plot, we find that residuals are stable at around 1 when fitted values are around 7 to 10. It is acceptable to say that our regression satisfies the Equal Variance assumption in general.

Lastly, we look at "Residuals Vs Leverage" plot. We can clearly see that leverage is smaller than 0.5, indicating that most data points don't have unusual values of predictors.


## Final model Conclusion

Based on Airbnb data, the best predictors of the price for 2 persons for 4 nights are:
property type, room type, review ratings scores, how strict the cancellation policy is, how many people the property accommodates, which neighbourhood the property is in, and if the property has air conditioning installed.

We find that the price of booking a Villa is the highest, whereas the price of booking a Guest suite is the lowest. Additionally, we find it interesting that the price of a Bed and Breakfast is on average higher than the price of an Apartment. Furthermore, it seems that the stricter the cancellation policy the higher the price.
In addition, the model shows that the price varies significantly based on which neighbourhood the property is located in. Better neighbourhoods have a higher price on average, and so do the properties that accommodate more people.
To conclude, since the average temperatures in Cape Town are high, having Air conditioning installed is associated with an average price increase of 30%.


##  Predicting the total cost to stay at an Airbnb for 4 nights using our model 


We will now use our final model to predict the total cost to stay at an Airbnb for 4 nights, filtering for listings that have at least 10 reviews and an average rating of at least 90. 

Preparing the testing data set:

```{r}

#First we create a new data set accoring to the conditions mentioned above

listings_test_filtered <- listings %>% 
  filter(property_type == "Apartment",
         room_type == "Private room", 
         number_of_reviews >= 10,
         review_scores_rating >= 90
         )

# A quick look at the dimensions of the data frame shows us that we are left with 128 observations that fulfill those conditions

dim(listings_test_filtered)

```

Predicting prices for our test listings:

```{r}

#listings_test_filtered <- listings_test_filtered %>% 
#  add_predictions(model3, var="predict_price")


#using the predict function to get create a dataframe that contains the point estimate as well as the prediction interval
prediction_results <- predict(model_final, newdata = listings_test_filtered, interval = c("prediction"))

#as all estimates are still in log, we have to reverse that
prediction_results <-exp(prediction_results)

#we have a quick look at the results table
prediction_results%>%
  kable()%>%
  kable_styling()



```



Merging our prediction results with the the listing test data set:

```{r}

listings_test_filtered_2 <- listings_test_filtered %>% 
  
  #keeping only columns which are relevant for further analysis
  select(-c(2:43, 45)) %>% 
  
  #adding the estimates, upper and lower bounds
  mutate(pred_price_fit = prediction_results[,1],
         pred_price_lwr = prediction_results[,2],
         pred_price_upr = prediction_results[,3],
         
         #creates new variables containing diffrence between estimate and actual value
         pred_difference = pred_price_fit - price_4_nights,
         
         #crates new variable that checks how far off the actual values are from the mean price of the overall data set (mean = 7,799 $)
         difference_mean = mean(listings$price_4_nights) - price_4_nights
         )

listings_test_filtered_2%>%
   kable()%>%
  kable_styling()

```



Summary Statistics:

In this section we will briefly run through some summary statistics to get a feel for how well we predicted the actual price for 4 nights.

```{r}

# In this table we compare the average 4 night price of the testing data set with the average of our predictions

listings_test_filtered_2 %>% 
  summarize(mean_pred = mean(pred_price_fit),
         mean_actual = mean(price_4_nights))

# In this table we look at the average amount our predictions differ from the actual listing prices
# We then compare it to the average difference to the overall average price to confirm that our model acts as a better predictor of price than the average

listings_test_filtered_2 %>% 
  summarize(using_model = mean(pred_difference),
            using_mean = mean(difference_mean))

```

The first table shows that on average, our model slightly underestimates the true mean price of the test data set (diff of 118 USD).

The second table gives us a rough estimate on how our model's prediction performance as compared to just using the overall mean price of the original data set. while our model slightly overestimates the true values (on average by 118 USD), an prediction model simply using the  overall mean price would heavily overstate the prices (on average by 5256 USD)


Plotting
```{r}

# We visualize the distributions of the actual prices and our predictions


listings_test_filtered_2 %>% 
  
  #in order to plot the data via ggplot, we have to put the data in "tidy" format, using the pivot longer function
  pivot_longer(names_to = "pred_vs_actual", values_to = "value", cols = c(price_4_nights, pred_price_fit)) %>% 
  
  #we can then continue plotting
  ggplot(aes(x=value, fill = pred_vs_actual)) +
  geom_density(alpha = 0.3)


# The second graph investigates how the difference between prediction and actual values is connect to the actual price for 4 nights

ggplot(listings_test_filtered_2, aes(x=price_4_nights, y=pred_difference)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(se =FALSE, method = lm) +
  labs(title = "The estimation errors seem to be correlated with the price", y="Difference between estimate and actual price", x="Actual price") 
  


```

In the first plot we can see the density plot for our estimates and the actual values.
The distribution of the actual prices look heavily right skewed, while the model predictions are distributed symetrically.

In the second plot we can see that the prediction error seem to be correlated to the actual price of the listing. 
The higher the price of the listing, the more our model will underestimate the price.

This could be a sign of a variable currently not included in our model, that would potentially explain this tendency.

Overall however, our final model delivers a good estimate on what how much a couple would have to spend on rent during a 4 day trip to beautiful Cape Town!



# References

Statista. 2020. Global AC Penetration Rate By Country 2016 | Statista. [online] Available at: <https://www.statista.com/statistics/911064/worldwide-air-conditioning-penetration-rate-country/> [Accessed 17 September 2020].

