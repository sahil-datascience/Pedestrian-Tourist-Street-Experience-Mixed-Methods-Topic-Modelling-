

##############################################################
################ Spatial Variable ############################
##############################################################

library(tidyverse)

gangtok_reviews <- read_csv("Data/tidy/gangtok_reviews_1.csv")

gangtok_reviews # n = 1385


###############  DOMESTIC LOCATIONS ##########################

##### Cities ########

gangtok_reviews$Cities <- str_extract(gangtok_reviews$Tourist_Home_location, "^[^,]+")
gangtok_reviews$Cities <- ifelse(str_detect(gangtok_reviews$Cities, regex("contribut", ignore_case = TRUE)),
                                 NA,
                                 gangtok_reviews$Cities)

# Upper case first letter of each city
gangtok_reviews$Cities <- str_to_title(gangtok_reviews$Cities)

# Count cities
gangtok_reviews %>%
        group_by(Cities) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))

unique(gangtok_reviews$Cities) # Ask Chat GPT for Local Locations (Sikkim)

#### Remove Local Users ########

remove_local <- c("Sikkim", "Gangtok", "Namchi", "Jorethang", "South Sikkim")

dim(gangtok_reviews)
gangtok_reviews <- gangtok_reviews %>%
        filter(!Cities %in% remove_local)
dim(gangtok_reviews)                            # 25 Local Reviews Removed


############################################

# Count How many cities == "India"
sum(gangtok_reviews$Cities == "India", na.rm = TRUE) # 18 Total

# Replace India with NA (Exceptional City name)
gangtok_reviews$Cities <- ifelse(gangtok_reviews$Cities == "India",
                                 NA,
                                 gangtok_reviews$Cities)

# Total Reviews with City Information Available
table(gangtok_reviews$Cities != "NA") # 1360 - 1113 (Cities) = 247 (NA)
# Total Unique Cities
length(unique(gangtok_reviews$Cities)) # 244 Unique Cities (Including NA)

####### Standardise Names of Domestic Cities ########
### Standardise Names

# Create a mapping of variants to standardized names
# Create a mapping of variants to standardized names
standardized_names <- c(
        # Standardize Delhi
        "New Delhi" = "Delhi",
        "National Capital Territory Of Delhi" = "Delhi",
        "Delhi" = "Delhi",
        
        # Standardize Mumbai
        "Mumbai (Bombay)" = "Mumbai",
        "Navi Mumbai" = "Mumbai",
        "Maharashtra" = "Mumbai",
        "Mumbai" = "Mumbai",
        
        # Standardize Bangalore
        "Bengaluru" = "Bangalore",
        "Bangalore District" = "Bangalore",
        "Bangalore" = "Bangalore",
        
        # Standardize Kolkata
        "Kolkata (Calcutta)" = "Kolkata",
        "Calcutta" = "Kolkata",
        "Kolkatta" = "Kolkata",
        "Kolkata District" = "Kolkata",
        "West Bengal" = "Kolkata",
        "Kolkata" = "Kolkata",
        
        # Standardize Chennai
        "Chennai (Madras)" = "Chennai",
        "Chennai District" = "Chennai",
        "Chennai" = "Chennai",
        
        # Standardize Kochi
        "Kochi (Cochin)" = "Kochi",
        "Ernakulam" = "Kochi",
        "Kochi" = "Kochi",
        
        # Standardize Goa
        "Goa Velha" = "Goa",
        "North Goa District" = "Goa",
        "Panaji" = "Goa",
        "Panjim" = "Goa",
        "Candolim" = "Goa",
        "Goa" = "Goa",
        
        # Standardize Hyderabad
        "Hyderabad District" = "Hyderabad",
        "Secunderabad" = "Hyderabad",
        "Hyderabad" = "Hyderabad",
        
        # Standardize Ahmedabad
        "Ahmedabad District" = "Ahmedabad",
        "Ahmedabad" = "Ahmedabad",
        
        # Standardize Shimla
        "Shimla District" = "Shimla",
        "Shimla" = "Shimla",
        
        # Standardize Mysore
        "Mysuru (Mysore)" = "Mysore",
        "Mysore" = "Mysore",
        
        # Standardize Thiruvananthapuram
        "Thiruvananthapuram (Trivandrum)" = "Thiruvananthapuram",
        "Trivandrum" = "Thiruvananthapuram",
        "Thiruvananthapuram" = "Thiruvananthapuram",
        
        # Standardize Gurgaon
        "Gurugram (Gurgaon)" = "Gurgaon",
        "Gurgaon District" = "Gurgaon",
        "Gurgaon" = "Gurgaon",
        
        # Standardize Vadodara
        "Vadodara District" = "Vadodara",
        "Baroda" = "Vadodara",
        "Vadodara" = "Vadodara",
        
        # Standardize Jammu
        "Jammu City" = "Jammu",
        
        # Standardize Leh
        "Leh-Ladakh" = "Leh",
        
        # Standardize Rajkot
        "Rajkot District" = "Rajkot",
        "Rajkot" = "Rajkot",
        
        # Standardize Visakhapatnam
        "Vizag" = "Visakhapatnam",
        
        # Standardize Pondicherry
        "Pondicherry" = "Puducherry",
        "Puducherry" = "Puducherry",
        
        # Standardize Aurangabad
        "Aurangabad District" = "Aurangabad",
        "Chhatrapati Sambhaji Nagar" = "Aurangabad",
        "Aurangabad" = "Aurangabad",
        
        # Standardize Bhopal
        "Bhopal District" = "Bhopal",
        "Bhopal" = "Bhopal",
        
        # Standardize Kozhikode
        "Kozhikode District" = "Kozhikode",
        "Kozhikode" = "Kozhikode",
        
        # Standardize Gorakhpur
        "Gorakhpur District" = "Gorakhpur",
        "Gorakhpur" = "Gorakhpur",
        
        # Standardize Ooty
        "Ooty (Udhagamandalam)" = "Ooty",
        
        # Standardize Amer (part of Jaipur)
        "Amer" = "Jaipur",
        
        # Standardize Odisha
        "Odisha" = "Bhubaneswar",
        
        # Standardize Assam
        "Assam" = "Dispur",   # Dispur is capital of Assam
        
        # Standardize Kerala
        "Kerala" = "Thiruvananthapuram",
        
        # Standardize UAE
        "United Arab Emirates" = "Abu Dhabi",
        
        # Standardize Kuwait
        "Kuwait" = "Kuwait City",
        
        # Standardize Bangladesh (Dhaka)
        "Dhaka Division" = "Dhaka",
        "Dhaka City" = "Dhaka",
        
        # Standardize Sri Lanka
        "Sri Lanka" = "Colombo",
        
        # Standardize South Africa
        "South Africa" = "Pretoria",
        
        # Standardize Cambodia
        "Cambodia" = "Phnom Penh"
)


# Apply the mapping to standardize city names
gangtok_reviews <- gangtok_reviews %>%
        mutate(Cities = ifelse(Cities %in% names(standardized_names), 
                               standardized_names[Cities], 
                               Cities))
# Check
table(gangtok_reviews$Cities != "NA")
length(unique(gangtok_reviews$Cities)) # 215 Unique cities (excluding NA)

# Count cities (Quick Overview)
gangtok_reviews %>%
        group_by(Cities) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))



#################### INTERNATIONAL LOCATIONS ########################


##### Countries ########  

gangtok_reviews$Countries <- str_trim(
        str_extract(gangtok_reviews$Tourist_Home_location, "[^,]+$")
)

# Replace with NA if contains "contribut"
gangtok_reviews$Countries <- ifelse(str_detect(gangtok_reviews$Countries,
                                               regex("contribut", ignore_case = TRUE)),
                                    NA,
                                    gangtok_reviews$Countries)

gangtok_reviews$Countries <- ifelse(str_detect(gangtok_reviews$Countries, ","), 
                                    str_extract(gangtok_reviews$Countries, "(?<=\\,\\s).*$"), 
                                    gangtok_reviews$Countries)

unique(gangtok_reviews$Countries) 

#Rename Misspelled Countries
ind_codes <- c("imphal", "new delhi","Delhi","Roorkee","CHANDIGARH",
               "Jodhpur", "Kolkata", "Mumbai", "Gurgaon","delhi","Bengaluru",
               "Kolkatta", "Aurangabad", "bangalore", "Noida", "mumbai",
               "Ludhiana", "West Bengal", "Bangalore")


us_codes <- c("NY", "NJ", "TX", "Utah", "PA U.S.A", "GA", "IL", "MN", "PA", 
              "FL", "MD", "MI", "WA", "AR", "AZ", "DC", "IN", "KS", "NH", 
              "OH","CA", "TN", "UT", "VA", "PA U.S.A.", "Idaho", "New York",
              "Fort Bragg", "Baltimore",  "Colorado", "Texas", "Virginia",
              "Florida", "OR", "MA", "Albuquerque")

canada_codes <- c("Alberta","Ontario", "Richmond BC Canada")

uk_codes <- c("UK", "uk", "United Kingdom", "England", "Scotland", "Wales",
              "Nottinghamshire", "London", "Birmingham", "Manchester",
              "edinburgh","Essex",  "Hertfordshire", 
              "devon", "Wigan", "Bangor", "Long buckby", "Northamptonshire",
              "peterborough", "sutton coldfield", "Mid Sussex", "Lancs", "Anglesey", "kent",
              "North Wales - UK", "Ystrad Mynach", "Devon England", "Doncaster",
              "peterborough UK", "Milton Keynes", "Hove", "Somerset", "london", "SCOTLAND",
              "s glos", "lancashire", "Berkshire", "yeovil")

aus_codes <- c("Shepparton", "Perth", "Bathurst", "adelaide")

mauritius_codes <-  c("Grand Baie","Grand Port District", "Port Louis",
                      "Curepipe")


#replace
gangtok_reviews <- gangtok_reviews %>% 
        mutate(Countries = case_when(
                Countries %in% us_codes ~ "United States",
                Countries %in% canada_codes ~ "Canada",
                Countries %in% uk_codes ~ "United Kingdom",
                Countries %in% ind_codes ~ "India",
                Countries %in% aus_codes ~ "Australia",
                Countries %in% mauritius_codes ~ "Mauritius",
                
                
                # Individual
                Countries == "Lahore" ~ "Pakistan",
                Countries == "Dhaka" ~ "Bangladesh",
                Countries == "Kathmandu" ~ "Nepal",
                Countries == "Tel Aviv" ~ "Israel",
                Countries == "Alicante" ~ "Spain",
                Countries == "Florence"  ~ "Italy",
                Countries ==  "Oslo" ~ "Norway",
                Countries == "Doha" ~ "Qatar",
                Countries == "Kuala Lumpur" ~ "Malaysia",
                
                Countries == "World" ~ NA,
                Countries == "world" ~ NA,
                Countries == "Asia" ~ NA,
                T ~ Countries #keep original if no match
        )) 

unique(gangtok_reviews$Countries)

# There is one exception homelocation was birmingham, assign uk
gangtok_reviews$Countries <- ifelse(gangtok_reviews$Countries == "Asia",
                                    NA,
                                    gangtok_reviews$Countries)



# Unique Countries
unique(gangtok_reviews$Countries)     # 36 Unique Countries (excluding NA)



# Count
gangtok_reviews %>%
        group_by(Countries) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))


########## Top International Cities ###########################

# Create a mapping of variants to standardized names
standardized_names_int <- c(
        # Standardize New York
        "New York City" = "New York",
        
        # Standardize London
        "Greater London" = "London",
        
        # Standardize Sydney
        "Greater Sydney" = "Sydney",
        
        # Standardize Abu Dhabi
        "Emirate Of Abu Dhabi" = "Abu Dhabi",
        
        # Standardize Dhaka
        "Dhaka City" = "Dhaka",
        
        # Standardize Auckland
        "Auckland Central" = "Auckland",
        
        # Standardize Cape Town
        "Cape Town Central" = "Cape Town",
        
        # Standardize United States
        "United States" = NA,  # Remove non-city entry
        "USA" = NA,            # Remove non-city entry
        
        # Standardize United Kingdom
        "United Kingdom" = NA, # Remove non-city entry
        "England" = NA,        # Remove non-city entry
        "Scotland" = NA,       # Remove non-city entry
        "Wales" = NA,          # Remove non-city entry
        "Uk" = NA,             # Remove non-city entry
        
        # Standardize Australia
        "Australia" = NA,      # Remove non-city entry
        
        # Standardize Canada
        "Canada" = NA,         # Remove non-city entry
        "Richmond Bc Canada" = "Richmond",
        
        # Standardize Malaysia
        "Malaysia" = NA,       # Remove non-city entry
        
        # Standardize Kuwait
        "Kuwait City" = "Kuwait",
        
        # Standardize Tel Aviv
        "Tel Aviv" = "Tel Aviv",
        
        # Standardize Dubai
        "Dubai" = "Dubai",
        
        # Standardize Manchester
        "Manchester" = "Manchester"
)

# Apply the mapping to standardize city names
gangtok_reviews <- gangtok_reviews %>%
        mutate(Cities = ifelse(Cities %in% names(standardized_names_int), 
                               standardized_names_int[Cities], 
                               Cities)) 



##
gangtok_reviews

names(gangtok_reviews)

dim(gangtok_reviews)

## Select Relevant Columns
gangtok_reviews <- gangtok_reviews %>%
        select(doc_id, Source, Date, Season, 
               Cities, Countries, raw_text)

dim(gangtok_reviews) # 1360
head(gangtok_reviews)

#-------------------------------- Summary ---------------------------------------#
table(is.na(gangtok_reviews$Cities)) # 247 Reviews with no city information
table(is.na(gangtok_reviews$Countries)) # 232 Review with no country information

# Total Unique 
length(unique(gangtok_reviews$Cities)) # 214 Unique Cities (excluding NA)
length(unique(gangtok_reviews$Countries)) # 36 Unique Countries (excluding NA)

# Total Reviews with City or Country Information
table(!is.na(gangtok_reviews$Cities) | !is.na(gangtok_reviews$Countries)) # 1131 Reviews

# Total Reviews with Both City and Country Information
table(!is.na(gangtok_reviews$Cities) & !is.na(gangtok_reviews$Countries)) # 1010 Reviews
#---------------------------------------------------------------------------------#

# Save Data
write_csv(gangtok_reviews, "Data/tidy/gangtok_reviews_2.csv") # n = 1360 (Before removing local, 1385)
