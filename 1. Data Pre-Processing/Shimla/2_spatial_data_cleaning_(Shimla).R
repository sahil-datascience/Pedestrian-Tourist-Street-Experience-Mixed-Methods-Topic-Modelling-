
##############################################################
################ Spatial Variable ############################
##############################################################

library(tidyverse)

all_reviews <- read_csv("Data/tidy/shimla_reviews_1.csv") # n = 3756

###############  DOMESTIC LOCATIONS ##########################

##### Cities ########

all_reviews$Cities <- str_extract(all_reviews$Tourist_Home_Location, "^[^,]+")
all_reviews$Cities <- ifelse(str_detect(all_reviews$Cities, regex("contribut", ignore_case = TRUE)),
                             NA,
                             all_reviews$Cities)

# Upper case first letter of each city
all_reviews$Cities <- str_to_title(all_reviews$Cities)

# Count cities
all_reviews %>%
        group_by(Cities) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))



## Unique
unique(all_reviews$Cities) # Ask Chat GPT for Local Locations (Himachal)

#### Remove Local (Himachal) Users ########
remove_hp <- c("Shimla", "Himachal Pradesh", "Shimla District",
               "Dharamsala", "Bilaspur", "Barog","Kullu","Mashobra",
               "Mcleod Ganj", "Sangla", 	
               "Parwanoo","Mandi", "Solan", "Manali",
               "Rampur", "Kangra")

# Remove Himachal Pradesh cities from all_reviews
print(paste("Total reviews before removing HP", length(all_reviews$Cities)))
all_reviews <- all_reviews %>%
        filter(!Cities %in% remove_hp)
print(paste("Total reviews after removing HP", length(all_reviews$Cities)))

# 104 reviews from Locals removed.

############################################

# Replace India with NA (Exceptional City name)
all_reviews$Cities <- ifelse(all_reviews$Cities == "India",
                                NA,
                                all_reviews$Cities)

####### Standaridse Names of Domestic Cities ########

# Unique
unique(all_reviews$Cities) # Ask Chat GPT for Local Locations (Himachal)

### Standardise Names

## Create a mapping of variants to standardized names
standardized_names <- c(
        # --- Existing mappings (unchanged) ---
        # Standardize Delhi
        "New Delhi" = "Delhi",
        "National Capital Territory Of Delhi" = "Delhi",
        "Delhi" = "Delhi",
        
        # Standardize Mumbai
        "Mumbai (Bombay)" = "Mumbai",
        "Navi Mumbai" = "Mumbai",
        "Bombay" = "Mumbai",
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
        "Kolkata" = "Kolkata",
        
        # Standardize Chennai
        "Chennai (Madras)" = "Chennai",
        "Chennai District" = "Chennai",
        "Chennai" = "Chennai",
        
        # Standardize Kochi
        "Kochi (Cochin)" = "Kochi",
        "Ernakulam" = "Kochi",
        "Cochin" = "Kochi",
        "Kochi" = "Kochi",
        
        # Standardize Goa
        "Goa Velha" = "Goa",
        "North Goa District" = "Goa",
        "Panaji" = "Goa",    # capital of Goa
        "Panjim" = "Goa",
        "Goa" = "Goa",
        
        # Standardize Hyderabad
        "Hyderabad District" = "Hyderabad",
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
        "Jammu" = "Jammu",
        
        # Standardize Leh
        "Leh-Ladakh" = "Leh",
        "Leh" = "Leh",
        
        # Standardize Rajkot
        "Rajkot District" = "Rajkot",
        "Rajkot" = "Rajkot",
        
        # Standardize Visakhapatnam
        "Vizag" = "Visakhapatnam",
        "Visakhapatnam" = "Visakhapatnam",
        
        # Standardize Pondicherry
        "Pondicherry" = "Puducherry",
        "Puducherry" = "Puducherry",
        
        # --- Replace countries/states with capitals (existing) ---
        "India" = "New Delhi",
        "United Kingdom" = "London",
        "England" = "London",
        "Uk" = "London",
        "Scotland" = "Edinburgh",
        "Wales" = "Cardiff",
        "Northern Ireland" = "Belfast",
        "United States" = "Washington Dc",
        "USA" = "Washington Dc",
        "Canada" = "Ottawa",
        "Australia" = "Canberra",
        "New Zealand" = "Wellington",
        "Pakistan" = "Islamabad",
        "Bangladesh" = "Dhaka",
        "Nepal" = "Kathmandu",
        "Sri Lanka" = "Colombo",
        "UAE" = "Abu Dhabi",
        "Emirate Of Abu Dhabi" = "Abu Dhabi",
        "Dubai" = "Abu Dhabi",   # keep under UAE capital
        "Qatar" = "Doha",
        "Singapore" = "Singapore",
        "Malaysia" = "Kuala Lumpur",
        "Thailand" = "Bangkok",
        "Indonesia" = "Jakarta",
        "Philippines" = "Manila",
        "Vietnam" = "Hanoi",
        "China" = "Beijing",
        "Japan" = "Tokyo",
        "South Korea" = "Seoul",
        "Russia" = "Moscow",
        "France" = "Paris",
        "Germany" = "Berlin",
        "Italy" = "Rome",
        "Spain" = "Madrid",
        "Portugal" = "Lisbon",
        "Netherlands" = "Amsterdam",
        "Belgium" = "Brussels",
        "Switzerland" = "Bern",
        "Austria" = "Vienna",
        "Luxembourg" = "Luxembourg City",
        "Norway" = "Oslo",
        "Sweden" = "Stockholm",
        "Denmark" = "Copenhagen",
        "Finland" = "Helsinki",
        "Ireland" = "Dublin",
        "Poland" = "Warsaw",
        "South Africa" = "Pretoria",
        "Mauritius" = "Port Louis",
        "Maldives" = "Male",
        "Bahrain" = "Manama",
        "Kuwait" = "Kuwait City",
        "Oman" = "Muscat",
        "Saudi Arabia" = "Riyadh",
        "Turkey" = "Ankara",
        "Egypt" = "Cairo",
        "Israel" = "Jerusalem",
        "Iran" = "Tehran",
        "Iraq" = "Baghdad",
        
        # --- EXTENSIONS: additional state/region/county/district -> city mappings ---
        # United States (states present in list -> capitals)
        "Florida" = "Tshimlaahassee",
        "Texas" = "Austin",
        "Virginia" = "Richmond",
        "Colorado" = "Denver",
        "Idaho" = "Boise",
        
        # Canada / provinces / provinces abbreviations
        "British Columbia" = "Victoria",
        "Bc" = "Victoria",
        "Quebec" = "Quebec City",
        "Ontario" = "Toronto",
        
        # Australia (states / abbreviations / regions)
        "Queensland" = "Brisbane",
        "Qld" = "Brisbane",
        "Victoria" = "Melbourne",
        "New South Wales" = "Sydney",
        "Tasmania" = "Hobart",
        "Western Australia" = "Perth",
        "South Australia" = "Adelaide",
        
        # New Zealand regions
        "Auckland" = "Auckland",
        "Wellington Region" = "Wellington",
        "Tauranga" = "Tauranga",
        
        # UK counties / regions -> principal/town/city
        "Devon" = "Exeter",
        "Cornwshimla" = "Truro",
        "Lancashire" = "Lancaster",
        "Oxfordshire" = "Oxford",
        "Hampshire" = "Winchester",
        "Suffolk" = "Ipswich",
        "Berkshire" = "Reading",
        "Surrey" = "Guildford",
        "North Yorkshire" = "Northshimlaerton",
        "Northamptonshire" = "Northampton",
        "Leicestershire" = "Leicester",
        "Greater London" = "London",
        "West Yorkshire" = "Leeds",
        "Anglesey" = "Bangor",
        "Isle Of Anglesey" = "Bangor",
        "Kent" = "Maidstone",
        "Somerset" = "Taunton",
        "Lancs" = "Lancaster",
        "Oxfordshire" = "Oxford",
        
        # UK-specific variants already in list mapped to cities
        "Devon England" = "Exeter",
        "North Wales - Uk" = "Cardiff",
        "Peterborough Uk" = "Peterborough",
        "Stoke-On-Trent" = "Stoke-On-Trent",
        
        # Indian states / big regions -> capitals
        "Uttar Pradesh" = "Lucknow",
        "Maharashtra" = "Mumbai",
        "Karnataka" = "Bangalore",
        "Tamil Nadu" = "Chennai",
        "Kerala" = "Thiruvananthapuram",
        "Gujarat" = "Gandhinagar",
        "Rajasthan" = "Jaipur",
        "Odisha" = "Bhubaneswar",
        "Bihar" = "Patna",
        "Assam" = "Dispur",
        "Jharkhand" = "Ranchi",
        "Madhya Pradesh" = "Bhopal",
        "Chhattisgarh" = "Raipur",
        "Telangana" = "Hyderabad",
        "Andhra Pradesh" = "Amaravati",
        "Punjab" = "Chandigarh",
        "Haryana" = "Chandigarh",
        "Goa" = "Panaji",
        "Manipur" = "Imphal",
        "Mizoram" = "Aizawl",
        "Nagaland" = "Kohima",
        "Tripura" = "Agartala",
        "Meghalaya" = "Shillong",
        "Arunachal Pradesh" = "Itanagar",
        "Sikkim" = "Gangtok",
        "Puducherry" = "Puducherry",
        "Dadra And Nagar Haveli" = "Daman",
        
        # Indian districts / administrative -> map to district HQ / nearest city (where obvious)
        "Bellary District" = "Bellary",
        "Jagtial District" = "Jagtial",
        "Meerut District" = "Meerut",
        "Muzaffarpur District" = "Muzaffarpur",
        "Nagpur District" = "Nagpur",
        "Coimbatore District" = "Coimbatore",
        "Sri Muktsar Sahib District" = "Muktsar",
        "Hubli-Dharwad" = "Hubli",
        "Hinjewadi" = "Pune",
        "Pimpri-Chinchwad" = "Pune",
        "Mysore" = "Mysore",
        
        # Regions / island / other territories -> representative city
        "Sabah" = "Kota Kinabalu",
        "Penang" = "George Town",
        "Sabah" = "Kota Kinabalu",
        "Grand Port District" = "Mahébourg",
        "Mauritius" = "Port Louis",
        "Anglesey" = "Bangor",
        "Aegina Town" = "Aegina",
        "Lugano" = "Lugano",
        
        # Middle East & Gulf subdivisions -> capitals (if region given)
        "Emirate Of Abu Dhabi" = "Abu Dhabi",
        
        # Other countries / territories that appeared in list but were not in original mapping
        "Cuba" = "Havana",
        "Gabon" = "Libreville",
        "Sabah" = "Kota Kinabalu",
        "Minsk Region" = "Minsk",
        "Minsk" = "Minsk",
        "Minsk Region" = "Minsk",
        
        # Cities used as proxies for country/regional names (where list had region-only entries)
        "Metro Manila" = "Manila",
        "Greater Noida" = "Noida",
        "Greater Sydney" = "Sydney",
        "Brisbane Region" = "Brisbane",
        "Grand Baie" = "Grand Baie",
        "Port Blair" = "Port Blair",
        
        # Map generic ambiguous strings or abbreviations to reasonable capitals / cities
        "Phila" = "Philadelphia",        # abbreviation -> city
        "Mbd" = "Mumbai",                # unknown abbreviation; mapped to large nearby city
        "Marathi Manoos" = "Mumbai",     # non-place text mapped to major city in Maharashtra
        "World" = "London",              # arbitrary neutral global proxy (choose London)
        
        # --- End of extensions ---
        # (If you want more strict / different choices for ambiguous items, we can refine.)
        NULL
)


# Apply the mapping to standardize city names
all_reviews <- all_reviews %>%
        mutate(Cities = ifelse(Cities %in% names(standardized_names), 
                               standardized_names[Cities], 
                               Cities))

# Unique Cities
unique(all_reviews$Cities) # 563 Unique Cities (including NA)

#################### INTERNATIONAL LOCATIONS ########################


##### Countries ########  

all_reviews$Countries <- str_trim(
        str_extract(all_reviews$Tourist_Home_Location, "[^,]+$")
)

# Replace contributory with NA
all_reviews$Countries <- ifelse(str_detect(all_reviews$Countries, regex("contribut", ignore_case = TRUE)),
                                   NA,
                                   all_reviews$Countries)

all_reviews$Countries <- ifelse(str_detect(all_reviews$Countries, ","), 
                                   str_extract(all_reviews$Countries, "(?<=\\,\\s).*$"), 
                                   all_reviews$Countries)


#Rename Misspelled Countries
ind_codes <- c("Mumbai", "Delhi","india","Hyderabad", "noida", "pune","ooty",
               "delhi", "Shimla", "Jaipur", "Kolkata", "Chandigarh", "chennai",
               "chandigarh","mumbai","kochi","Noida","baroda","Bangalore","New Delhi",
               "Punjab", "Bargarh", "udaipur", "Pune", "MBD", "Sirsa", "bangalore", 
               "Gurgaon", "renukoot", "Lucknow", "bhopal", "DELHI", "Ujjain", "Paschim Vihar", 
               "Leh-Ladakh", "kolkata", "Chennai", "Marathi manoos", "jamshedpur", "Indore", 
               "shimla", "NOIDA", "Patiala", "Barog", "tamilnadu state", "Betul", "Kanpur", 
               "Rohtak", "panaji", "vadodara", "Vadodara", "silchar", "calcutta", "Yamuna Nagar", 
               "Assolna", "bhagalpur", "NAGPUR", "jaipur", "Kolkatta",
               "Asia")


us_codes <- c("NY", "NJ", "TX", "Utah", "PA U.S.A", "GA", "IL", "MN", "PA", 
              "FL", "MD", "MI", "WA", "AR", "AZ", "DC", "IN", "KS", "NH", 
              "OH","CA", "TN", "UT", "VA", "PA U.S.A.", "Idaho", "New York",
              "Fort Bragg", "Baltimore",  "Colorado", "Texas", "Virginia",
              "Florida")

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

mauritius_codes <-  c("Grand Baie","Grand Port District", "Port Louis")


#replace
all_reviews <- all_reviews %>% 
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
                
                Countries == "World" ~ NA,
                T ~ Countries #keep original if no match
        ))

# Unique
unique(all_reviews$Countries) # 63 Unique Countries (including NA and India)

# Count
all_reviews %>%
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
all_reviews <- all_reviews %>%
        mutate(Cities = ifelse(Cities %in% names(standardized_names_int), 
                               standardized_names_int[Cities], 
                               Cities)) 



##
all_reviews

names(all_reviews)

dim(all_reviews)

## Select Relevant Columns
all_reviews <- all_reviews %>%
        select(doc_id, Source, Date, Season, 
               Cities, Countries, Main_Text)

#---------------------------- Summary ----------------------------#

table(is.na(all_reviews$Cities)) # 495 Reviews with no city information
table(is.na(all_reviews$Countries)) # 462 Review with no country information

# Total Unique 
length(unique(all_reviews$Cities)) # 558 Unique Cities (excluding NA)
length(unique(all_reviews$Countries)) # 62 Unique Countries (excluding NA)

# Total Reviews with City or Country Information
table(!is.na(all_reviews$Cities) | !is.na(all_reviews$Countries)) # 3192 Reviews

# Total Reviews with Both City and Country Information
table(!is.na(all_reviews$Cities) & !is.na(all_reviews$Countries)) # 3155 Reviews
#---------------------------------------------------------------------------------#

## Save Data
write_csv(all_reviews, "Data/tidy/shimla_reviews_2.csv") # n = 3652 (Before removing locals, 3756)
