# -------------------------------------------------------------------
# Title: Educational Reproduction Amid Women’s Empowerment
# Author: Tomaš Lintner
# Version: 1.0
# Date: 31.8.2024
# -------------------------------------------------------------------
# Description:
# This R script analyzes the influence of women's empowerment on the 
# reproduction of educational attainment across generations, using data 
# from the EVS/WVS and V-Dem Institute. The script includes steps for 
# data extraction, missing data imputation, and the application of 
# cumulative link mixed models (CLMM) to assess the role of gender 
# and parental education in shaping educational outcomes.
#
# Note: This code is part of the study titled "Educational Reproduction 
# Amid Women’s Empowerment".
# -------------------------------------------------------------------

### ENVIRONMENT PREPARATIONS ###

# List of packages to check, install, and load
packages <- c("openxlsx", "ordinal", "countrycode", "dplyr", "purrr", "zoo", "mice", "writexl")

# Loop through the list of packages
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# Use the location on Your computer where You stored the two datasets
setwd("???")

# Load the data
EVS_WVS_raw <- read.csv("EVS_WVS_Joint_Csv_v5_0.csv")
V_Dem_raw <- read.csv("V-Dem-CY-Full+Others-v14.csv")

### DATA WRANGLING ###

# Select only the columns country_name, year, and v2x_gender from the V-Dem dataset
V_Dem_transformed <- V_Dem_raw %>%
  select(country_name, year, v2x_gender)

# Imputing missing V-Dem data by linear interpolation and NOCB

# List of countries and the respective years that need to be imputed
imputation_list <- list(
  "Pakistan" = 1952:1968,
  "Singapore" = 1962:1964,
  "Latvia" = 1991:1992,
  "Montenegro" = 1999:2005,
  "Maldives" = 1952:1980,
  "Nigeria" = 1952:1981
)

# Add missing rows for the required years and countries
for (country in names(imputation_list)) {
  # Extract the relevant years to be imputed
  years_to_impute <- imputation_list[[country]]
  
  # Create a data frame with missing years and country name
  missing_rows <- data.frame(
    year = years_to_impute,
    country_name = country,
    v2x_gender = NA
  )
  
  # Add the missing rows to V_Dem_transformed
  V_Dem_transformed <- V_Dem_transformed %>%
    bind_rows(missing_rows)
}

# Arrange the V_Dem_transformed dataset by country_name and year
V_Dem_transformed <- V_Dem_transformed %>%
  arrange(country_name, year)

# View the updated V_Dem_transformed to ensure the missing rows were added
print(V_Dem_transformed %>% filter(country_name %in% names(imputation_list)))

# Interpolate and use NOCB to fill missing values for the specific countries
for (country in names(imputation_list)) {
  V_Dem_transformed <- V_Dem_transformed %>%
    filter(country_name == country) %>%
    arrange(year) %>%
    mutate(
      # Linear Interpolation
      v2x_gender = if (sum(!is.na(v2x_gender)) >= 2) {
        ifelse(is.na(v2x_gender), 
               approx(year[!is.na(v2x_gender)], v2x_gender[!is.na(v2x_gender)], xout = year, rule = 2)$y, 
               v2x_gender)
      } else {
        v2x_gender
      },
      # NOCB - fill any remaining NAs
      v2x_gender = zoo::na.locf(v2x_gender, na.rm = FALSE),
      v2x_gender = zoo::na.locf(v2x_gender, fromLast = TRUE, na.rm = FALSE)
    ) %>%
    ungroup() %>%
    bind_rows(V_Dem_transformed %>% filter(country_name != country)) %>%
    arrange(country_name, year)
}

# Missing age and date of birth data handling

# Replace values -1, -2, -3, -4, and -5 with NA in birth_year and age columns
EVS_WVS_raw <- EVS_WVS_raw %>%
  mutate(
    X002 = ifelse(X002 %in% c(-1, -2, -3, -4, -5), NA, X002),
    X003 = ifelse(X003 %in% c(-1, -2, -3, -4, -5), NA, X003)
  )

# Impute missing values in birth_year and age
EVS_WVS_raw <- EVS_WVS_raw %>%
  mutate(
    # Impute age from birth_year
    X003 = ifelse(is.na(X003) & !is.na(X002), year - X002, X003),
    
    # Impute birth_year from age
    X002 = ifelse(is.na(X002) & !is.na(X003), year - X003, X002)
  )

# Define the column names
survey_country_col <- "cntry_AN"
origin_country_col <- "X002_02B"
immigrant_status_col <- "G027A"

# Replace negative or missing values with NA in survey_country, origin_country, and immigrant_status
EVS_WVS_raw <- EVS_WVS_raw %>%
  mutate(
    # Replace missing or invalid values in survey_country with NA
    !!survey_country_col := ifelse(is.na(.data[[survey_country_col]]) | .data[[survey_country_col]] < 0, NA, .data[[survey_country_col]]),
    !!origin_country_col := ifelse(is.na(.data[[origin_country_col]]) | .data[[origin_country_col]] < 0, NA, .data[[origin_country_col]]),
    !!immigrant_status_col := ifelse(is.na(.data[[immigrant_status_col]]) | .data[[immigrant_status_col]] < 0, NA, .data[[immigrant_status_col]])
  )

# Impute missing origin_country where immigrant_status is 1 and survey_country is available
EVS_WVS_raw <- EVS_WVS_raw %>%
  mutate(
    !!origin_country_col := ifelse(is.na(.data[[origin_country_col]]) & .data[[immigrant_status_col]] == 1 & !is.na(.data[[survey_country_col]]),
                                   .data[[survey_country_col]], .data[[origin_country_col]])
  )

# Making new dataset

# Correct the survey_country_name (ignore the errors)
EVS_WVS_raw <- EVS_WVS_raw %>%
  mutate(survey_country_name = countrycode(cntry_AN, origin = "iso2c", destination = "country.name"),
         survey_country_name = ifelse(cntry_AN == "NIR", "United Kingdom", survey_country_name),
         survey_country_name = ifelse(cntry_AN == "HK", "Hong Kong", survey_country_name),
         survey_country_name = ifelse(cntry_AN == "BA", "Bosnia and Herzegovina", survey_country_name),
         survey_country_name = ifelse(cntry_AN == "MM", "Burma/Myanmar", survey_country_name),
         survey_country_name = ifelse(cntry_AN == "MV", "Maldives", survey_country_name),
         survey_country_name = ifelse(cntry_AN == "NG", "Nigeria", survey_country_name),
         survey_country_name = ifelse(cntry_AN == "PK", "Pakistan", survey_country_name),
         survey_country_name = ifelse(cntry_AN == "SG", "Singapore", survey_country_name),
         survey_country_name = ifelse(cntry_AN == "TR", "Türkiye", survey_country_name),
         survey_country_name = ifelse(cntry_AN == "US", "United States of America", survey_country_name))

# Remove rows with Andorra, Macao, and Puerto Rico (no V-Dem data available)
EVS_WVS_raw <- EVS_WVS_raw %>%
  filter(!(cntry_AN %in% c("AD", "MO", "PR")))

# Calculate year when the respondent was 15 years old
EVS_WVS_raw <- EVS_WVS_raw %>%
  mutate(year_when_15 = year - X003 + 15)

# Handle historical country mappings
EVS_WVS_raw <- EVS_WVS_raw %>%
  mutate(survey_country_name = case_when(
    survey_country_name %in% c("Armenia", "Azerbaijan", "Belarus", "Estonia", "Georgia", "Kyrgyzstan", "Kazakhstan", "Lithuania", "Latvia", "Tajikistan", "Ukraine", "Uzbekistan") & year_when_15 <= 1989 ~ "Russia",
    survey_country_name == "Bosnia and Herzegovina" & year_when_15 <= 1991 ~ "Serbia",
    survey_country_name == "Croatia" & year_when_15 <= 1990 ~ "Serbia",
    survey_country_name == "Montenegro" & year_when_15 <= 1997 ~ "Serbia",
    survey_country_name == "North Macedonia" & year_when_15 <= 1990 ~ "Serbia",
    survey_country_name == "Slovenia" & year_when_15 <= 1988 ~ "Serbia",
    survey_country_name == "Bangladesh" & year_when_15 <= 1970 ~ "Pakistan",
    survey_country_name == "Slovakia" & year_when_15 <= 1992 ~ "Czechia",
    TRUE ~ survey_country_name
  ))

# Join with V-Dem data to get WEI_15 (ignore the errors)
new_data <- EVS_WVS_raw %>%
  left_join(V_Dem_transformed, by = c("survey_country_name" = "country_name", "year_when_15" = "year")) %>%
  rename(WEI_15 = v2x_gender)

# Select the relevant columns for the final dataset
new_data <- new_data %>%
  select(
    case_ID = uniqid,
    survey_year = year,
    survey_country = cntry_AN,
    case_weight = gwght,
    birth_year = X002,
    age = X003,
    sex = X001,
    origin_country = X002_02B,
    immigrant_status = G027A,
    origin_country_mother = V002A_01,
    origin_country_father = V001A_01,
    attainment = X025A_01,
    attainment_mother = V004AM_01,
    attainment_father = V004AF_01,
    WEI_15
  )

# Interpolation and imputation to create imputed_values
imputed_values <- V_Dem_transformed %>%
  filter(country_name %in% unique(new_data$survey_country)) %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(
    WEI_15_interp = na.approx(v2x_gender, rule = 2),  # Linear interpolation
    WEI_15_nocb = zoo::na.locf(v2x_gender, fromLast = TRUE)  # NOCB imputation
  ) %>%
  ungroup() %>%
  select(country_name, year, WEI_15_interp, WEI_15_nocb)

# Merge imputed values back into new_data
new_data <- new_data %>%
  left_join(imputed_values, by = c("survey_country" = "country_name", "survey_year" = "year")) %>%
  mutate(
    # Replace NA values in WEI_15 with imputed values
    WEI_15 = ifelse(is.na(WEI_15), WEI_15_interp, WEI_15)
  ) %>%
  select(-WEI_15_interp, -WEI_15_nocb)  # Drop the temporary imputation columns if no longer needed

# Calculate the year when the respondent was 15 years old
new_data <- new_data %>%
  mutate(year_when_15 = survey_year - age + 15)

# Create the WEI_mother and WEI_father columns, initializing with 0
new_data <- new_data %>%
  mutate(
    WEI_mother = 0,
    WEI_father = 0
  )

# Set WEI_mother and WEI_father to NA if origin_country_mother/father does not match survey_country
new_data <- new_data %>%
  mutate(
    WEI_mother = ifelse(origin_country_mother != survey_country, NA, WEI_mother),
    WEI_father = ifelse(origin_country_father != survey_country, NA, WEI_father)
  )

# Calculate WEI_mother and WEI_father for cases where origin_country_mother/father does not match survey_country
new_data <- new_data %>%
  rowwise() %>%
  mutate(
    WEI_mother = ifelse(is.na(WEI_mother),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == countrycode(origin_country_mother, origin = "iso2c", destination = "country.name") &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_mother),
    WEI_father = ifelse(is.na(WEI_father),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == countrycode(origin_country_father, origin = "iso2c", destination = "country.name") &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_father)
  ) %>%
  ungroup()

# Handle historical country mappings for mother's and father's origin countries (ignore the warnings)
new_data <- new_data %>%
  mutate(
    origin_country_mother_name = countrycode(origin_country_mother, origin = "iso2c", destination = "country.name"),
    origin_country_father_name = countrycode(origin_country_father, origin = "iso2c", destination = "country.name"),
    
    origin_country_mother_name = case_when(
      origin_country_mother_name %in% c("Armenia", "Azerbaijan", "Belarus", "Estonia", "Georgia", "Kyrgyzstan", "Kazakhstan", "Lithuania", "Latvia", "Tajikistan", "Ukraine", "Uzbekistan") & year_when_15 <= 1989 ~ "Russia",
      origin_country_mother_name == "Bosnia and Herzegovina" & year_when_15 <= 1991 ~ "Serbia",
      origin_country_mother_name == "Croatia" & year_when_15 <= 1990 ~ "Serbia",
      origin_country_mother_name == "Montenegro" & year_when_15 <= 1997 ~ "Serbia",
      origin_country_mother_name == "North Macedonia" & year_when_15 <= 1990 ~ "Serbia",
      origin_country_mother_name == "Slovenia" & year_when_15 <= 1988 ~ "Serbia",
      origin_country_mother_name == "Bangladesh" & year_when_15 <= 1970 ~ "Pakistan",
      origin_country_mother_name == "Slovakia" & year_when_15 <= 1992 ~ "Czechia",
      TRUE ~ origin_country_mother_name
    ),
    
    origin_country_father_name = case_when(
      origin_country_father_name %in% c("Armenia", "Azerbaijan", "Belarus", "Estonia", "Georgia", "Kyrgyzstan", "Kazakhstan", "Lithuania", "Latvia", "Tajikistan", "Ukraine", "Uzbekistan") & year_when_15 <= 1989 ~ "Russia",
      origin_country_father_name == "Bosnia and Herzegovina" & year_when_15 <= 1991 ~ "Serbia",
      origin_country_father_name == "Croatia" & year_when_15 <= 1990 ~ "Serbia",
      origin_country_father_name == "Montenegro" & year_when_15 <= 1997 ~ "Serbia",
      origin_country_father_name == "North Macedonia" & year_when_15 <= 1990 ~ "Serbia",
      origin_country_father_name == "Slovenia" & year_when_15 <= 1988 ~ "Serbia",
      origin_country_father_name == "Bangladesh" & year_when_15 <= 1970 ~ "Pakistan",
      origin_country_father_name == "Slovakia" & year_when_15 <= 1992 ~ "Czechia",
      TRUE ~ origin_country_father_name
    )
  )

# Impute the WEI_mother and WEI_father for historical country mappings (ignore the warnings)
new_data <- new_data %>%
  rowwise() %>%
  mutate(
    WEI_mother = ifelse(is.na(WEI_mother),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == origin_country_mother_name &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_mother),
    WEI_father = ifelse(is.na(WEI_father),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == origin_country_father_name &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_father)
  ) %>%
  ungroup()

# Identify special cases based on the origin_country_mother and origin_country_father
special_cases <- new_data %>%
  filter(origin_country_mother %in% c("CSHH", "CSXX", "DDDE", "SUHH", "XK", "YUCS") |
           origin_country_father %in% c("CSHH", "CSXX", "DDDE", "SUHH", "XK", "YUCS"))

# Manually handle the special cases
new_data <- new_data %>%
  rowwise() %>%
  mutate(
    WEI_mother = ifelse(origin_country_mother == "CSHH" & is.na(WEI_mother),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Czechia" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_mother),
    WEI_mother = ifelse(origin_country_mother == "CSXX" & is.na(WEI_mother),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Serbia" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_mother),
    WEI_mother = ifelse(origin_country_mother == "DDDE" & is.na(WEI_mother),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Germany" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_mother),
    WEI_mother = ifelse(origin_country_mother == "SUHH" & is.na(WEI_mother),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Russia" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_mother),
    WEI_mother = ifelse(origin_country_mother == "XK" & is.na(WEI_mother),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Serbia" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_mother),
    WEI_mother = ifelse(origin_country_mother == "YUCS" & is.na(WEI_mother),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Serbia" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_mother),
    
    WEI_father = ifelse(origin_country_father == "CSHH" & is.na(WEI_father),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Czechia" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_father),
    WEI_father = ifelse(origin_country_father == "CSXX" & is.na(WEI_father),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Serbia" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_father),
    WEI_father = ifelse(origin_country_father == "DDDE" & is.na(WEI_father),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Germany" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_father),
    WEI_father = ifelse(origin_country_father == "SUHH" & is.na(WEI_father),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Russia" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_father),
    WEI_father = ifelse(origin_country_father == "XK" & is.na(WEI_father),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Serbia" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_father),
    WEI_father = ifelse(origin_country_father == "YUCS" & is.na(WEI_father),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Serbia" &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_father)
  ) %>%
  ungroup()

# Create a mapping for the special country names and handle historical mappings
new_data <- new_data %>%
  mutate(
    origin_country_mother_name = case_when(
      origin_country_mother_name == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
      origin_country_mother_name == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
      origin_country_mother_name == "Côte d’Ivoire" ~ "Ivory Coast",
      origin_country_mother_name == "Hong Kong SAR China" ~ "Hong Kong",
      origin_country_mother_name == "Myanmar (Burma)" ~ "Burma/Myanmar",
      origin_country_mother_name == "Palestinian Territories" ~ "Palestine/British Mandate",
      origin_country_mother_name == "São Tomé & Príncipe" ~ "Sao Tome and Principe",
      origin_country_mother_name == "Trinidad & Tobago" ~ "Trinidad and Tobago",
      origin_country_mother_name == "Turkey" ~ "Türkiye",
      origin_country_mother_name == "United States" ~ "United States of America",
      
      origin_country_mother_name %in% c("Armenia", "Azerbaijan", "Belarus", "Estonia", "Georgia", "Kyrgyzstan", "Kazakhstan", "Lithuania", "Latvia", "Moldova", "Tajikistan", "Turkmenistan", "Ukraine", "Uzbekistan") & year_when_15 <= 1989 ~ "Russia",
      origin_country_mother_name == "Bosnia and Herzegovina" & year_when_15 <= 1991 ~ "Serbia",
      origin_country_mother_name == "Croatia" & year_when_15 <= 1990 ~ "Serbia",
      origin_country_mother_name == "Montenegro" & year_when_15 <= 1997 ~ "Serbia",
      origin_country_mother_name == "North Macedonia" & year_when_15 <= 1990 ~ "Serbia",
      origin_country_mother_name == "Slovenia" & year_when_15 <= 1988 ~ "Serbia",
      origin_country_mother_name == "Bangladesh" & year_when_15 <= 1970 ~ "Pakistan",
      origin_country_mother_name == "Slovakia" & year_when_15 <= 1992 ~ "Czechia",
      TRUE ~ origin_country_mother_name
    ),
    origin_country_father_name = case_when(
      origin_country_father_name == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
      origin_country_father_name == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
      origin_country_father_name == "Côte d’Ivoire" ~ "Ivory Coast",
      origin_country_father_name == "Hong Kong SAR China" ~ "Hong Kong",
      origin_country_father_name == "Myanmar (Burma)" ~ "Burma/Myanmar",
      origin_country_father_name == "Palestinian Territories" ~ "Palestine/British Mandate",
      origin_country_father_name == "São Tomé & Príncipe" ~ "Sao Tome and Principe",
      origin_country_father_name == "Trinidad & Tobago" ~ "Trinidad and Tobago",
      origin_country_father_name == "Turkey" ~ "Türkiye",
      origin_country_father_name == "United States" ~ "United States of America",
      
      origin_country_father_name %in% c("Armenia", "Azerbaijan", "Belarus", "Estonia", "Georgia", "Kyrgyzstan", "Kazakhstan", "Lithuania", "Latvia", "Moldova", "Tajikistan", "Turkmenistan", "Ukraine", "Uzbekistan") & year_when_15 <= 1989 ~ "Russia",
      origin_country_father_name == "Bosnia and Herzegovina" & year_when_15 <= 1991 ~ "Serbia",
      origin_country_father_name == "Croatia" & year_when_15 <= 1990 ~ "Serbia",
      origin_country_father_name == "Montenegro" & year_when_15 <= 1997 ~ "Serbia",
      origin_country_father_name == "North Macedonia" & year_when_15 <= 1990 ~ "Serbia",
      origin_country_father_name == "Slovenia" & year_when_15 <= 1988 ~ "Serbia",
      origin_country_father_name == "Bangladesh" & year_when_15 <= 1970 ~ "Pakistan",
      origin_country_father_name == "Slovakia" & year_when_15 <= 1992 ~ "Czechia",
      TRUE ~ origin_country_father_name
    )
  )

# Recalculate WEI_mother and WEI_father for these manually corrected and historical country names
new_data <- new_data %>%
  rowwise() %>%
  mutate(
    WEI_mother = ifelse(is.na(WEI_mother) & !is.na(origin_country_mother_name),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == origin_country_mother_name &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_mother),
    WEI_father = ifelse(is.na(WEI_father) & !is.na(origin_country_father_name),
                        V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == origin_country_father_name &
                                                             V_Dem_transformed$year == year_when_15)],
                        WEI_father)
  ) %>%
  ungroup()

# Focus on Bosnia and Herzegovina when it was part of Yugoslavia (did not work in the previous step)
new_data <- new_data %>%
  rowwise() %>%
  mutate(
    # Handle WEI_mother
    WEI_mother = ifelse(
      origin_country_mother_name == "Bosnia and Herzegovina" & year_when_15 <= 1991 & is.na(WEI_mother),
      V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Serbia" &
                                           V_Dem_transformed$year == year_when_15)],
      WEI_mother
    ),
    # Handle WEI_father
    WEI_father = ifelse(
      origin_country_father_name == "Bosnia and Herzegovina" & year_when_15 <= 1991 & is.na(WEI_father),
      V_Dem_transformed$v2x_gender[which(V_Dem_transformed$country_name == "Serbia" &
                                           V_Dem_transformed$year == year_when_15)],
      WEI_father
    )
  ) %>%
  ungroup()

# Ensure that the necessary columns exist in new_data
new_data <- new_data %>%
  mutate(
    occupation = NA_character_,  # Initialize as character
    occupation_father = NA_character_,  # Initialize as character
    children_number = NA_integer_,  # Initialize as integer
    occupation_ISCO = NA_character_  # Initialize as character
  )

# Copy data from EVS_WVS_raw into new_data
new_data <- new_data %>%
  left_join(
    EVS_WVS_raw %>%
      select(
        case_ID = uniqid,
        occupation = X036E_WVS7,
        occupation_father = V097EF,
        children_number = X011,
        occupation_ISCO = X035_EVS5
      ) %>%
      mutate(
        # Exclude missing values for occupation (no negative values, no 11, no 66)
        occupation = ifelse(occupation %in% c(-1, -2, -3, -4, 11, 66), NA, as.character(occupation)),
        # Exclude missing values for occupation_father (no negative values, no 11, no 66)
        occupation_father = ifelse(occupation_father %in% c(-1, -2, -3, -4, 11, 66), NA, as.character(occupation_father)),
        # Exclude missing values for children_number (no negative values, no 11, no 66)
        children_number = ifelse(children_number %in% c(-1, -2, -3, -4, 11, 66), NA, as.integer(children_number)),
        # Exclude missing values for occupation_ISCO (no negative values, no 99)
        occupation_ISCO = ifelse(occupation_ISCO %in% c(-1, -2, -3, -4, 99), NA, as.character(occupation_ISCO)),
        # Keep only the first digit of the occupation_ISCO code
        occupation_ISCO = substr(occupation_ISCO, 1, 1)
      ),
    by = "case_ID"
  ) %>%
  mutate(
    # Copy non-missing values into new_data
    occupation = coalesce(occupation.y, occupation.x),
    occupation_father = coalesce(occupation_father.y, occupation_father.x),
    children_number = coalesce(children_number.y, children_number.x),
    occupation_ISCO = coalesce(occupation_ISCO.y, occupation_ISCO.x)
  ) %>%
  select(-occupation.y, -occupation.x, -occupation_father.y, -occupation_father.x, -children_number.y, -children_number.x, -occupation_ISCO.y, -occupation_ISCO.x)

### HANDLING MISSING DATA ###

# Define a function to count missing data, including NAs, empty strings, and negative values
count_missing <- function(x) {
  sum(is.na(x) | x == "" | grepl("^-", x))
}

# Apply the function to each column in new_data and create a summary dataframe
missing_data_summary <- sapply(new_data, count_missing)

# Convert to a data frame for better readability
missing_data_summary_df <- data.frame(
  Column = names(missing_data_summary),
  MissingCount = missing_data_summary
)

# Print the summary of missing data
print(missing_data_summary_df)

# List of columns to transform
columns_to_transform <- c("birth_year", "sex", "immigrant_status", 
                          "attainment", "attainment_mother", 
                          "attainment_father", "WEI_15", 
                          "WEI_mother", "WEI_father")

# Transform values starting with '-' to NA
new_data <- new_data %>%
  mutate(across(all_of(columns_to_transform), 
                ~ ifelse(grepl("^-", as.character(.)), NA, .)))

# Convert ordinal factors to numeric and create new columns
new_data <- new_data %>%
  mutate(
    attainment_father_numeric = as.numeric(attainment_father),
    attainment_mother_numeric = as.numeric(attainment_mother)
  )

# Transform sex variable into a categorical variable
new_data <- new_data %>%
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")))

# Same for the immigrant status
new_data <- new_data %>%
  mutate(immigrant_status = factor(immigrant_status, levels = c(1, 2), labels = c("No", "Yes")))

# Convert the variables to ordered factors
new_data$attainment <- factor(new_data$attainment, ordered = TRUE)
new_data$attainment_mother <- factor(new_data$attainment_mother, ordered = TRUE)
new_data$attainment_father <- factor(new_data$attainment_father, ordered = TRUE)

# Create the dataframe for no-imputation sensitivity analysis
data_for_analysis_no_imputation <- new_data %>%
  transmute(
    case_ID = case_ID,
    survey_country = survey_country,
    attainment = attainment,
    birth_year = birth_year,
    
    sex = NA_real_,
    age = NA_real_,
    case_weight = NA_real_,
    female_immigrant_status = NA_real_,
    WEI_15_female = NA_real_,
    WEI_15_male = NA_real_,
    mother_attainment_female = NA_real_,
    mother_attainment_male = NA_real_,
    father_attainment_female = NA_real_,
    father_attainment_male = NA_real_,
    reversed_WEI_15_father_attainment_female = NA_real_,
    reversed_WEI_15_father_attainment_male = NA_real_,
    reversed_WEI_15_mother_attainment_female = NA_real_,
    reversed_WEI_15_mother_attainment_male = NA_real_,
    WEI_mother_female = NA_real_,
    WEI_mother_male = NA_real_,
    WEI_father_female = NA_real_,
    WEI_father_male = NA_real_
  )

# Transform sex: Male -> 0, Female -> 1
data_for_analysis_no_imputation$sex <- ifelse(new_data$sex == "Male", 0, ifelse(new_data$sex == "Female", 1, NA))

# Age remains the same
data_for_analysis_no_imputation$age <- new_data$age

# Birth year remains the same
data_for_analysis_no_imputation$year_when_15 <- new_data$year_when_15

# Case weight remains the same
data_for_analysis_no_imputation$case_weight <- new_data$case_weight

# Create female_immigrant_status: 1 if Female and Immigrant, else 0
data_for_analysis_no_imputation$female_immigrant_status <- ifelse(new_data$sex == "Female" & new_data$immigrant_status == "Yes", 1, 0)

# WEI_15_female: 0 for males, WEI_15 for females
data_for_analysis_no_imputation$WEI_15_female <- ifelse(new_data$sex == "Female", new_data$WEI_15, 0)

# WEI_15_male: 0 for females, WEI_15 for males
data_for_analysis_no_imputation$WEI_15_male <- ifelse(new_data$sex == "Male", new_data$WEI_15, 0)

# mother_attainment_female: Mother's attainment for females, 0 for others
data_for_analysis_no_imputation$mother_attainment_female <- ifelse(new_data$sex == "Female", new_data$attainment_mother, 0)

# mother_attainment_male: Mother's attainment for males, 0 for others
data_for_analysis_no_imputation$mother_attainment_male <- ifelse(new_data$sex == "Male", new_data$attainment_mother, 0)

# father_attainment_female: Father's attainment for females, 0 for others
data_for_analysis_no_imputation$father_attainment_female <- ifelse(new_data$sex == "Female", new_data$attainment_father, 0)

# father_attainment_male: Father's attainment for males, 0 for others
data_for_analysis_no_imputation$father_attainment_male <- ifelse(new_data$sex == "Male", new_data$attainment_father, 0)

# reversed_WEI_15_father_attainment_female: (1 - WEI_15) * father's attainment for females, 0 for males
data_for_analysis_no_imputation$WEI_15_mother_attainment_female <- ifelse(new_data$sex == "Female", new_data$WEI_15 * new_data$attainment_mother_numeric, 0)

# reversed_WEI_15_father_attainment_male: (1 - WEI_15) * father's attainment for males, 0 for females
data_for_analysis_no_imputation$WEI_15_mother_attainment_male <- ifelse(new_data$sex == "Male", new_data$WEI_15 * new_data$attainment_mother_numeric, 0)

# reversed_WEI_15_father_attainment_female: (1 - WEI_15) * father's attainment for females, 0 for males
data_for_analysis_no_imputation$WEI_15_father_attainment_female <- ifelse(new_data$sex == "Female", new_data$WEI_15 * new_data$attainment_father_numeric, 0)

# reversed_WEI_15_father_attainment_male: (1 - WEI_15) * father's attainment for males, 0 for females
data_for_analysis_no_imputation$WEI_15_father_attainment_male <- ifelse(new_data$sex == "Male", new_data$WEI_15 * new_data$attainment_father_numeric, 0)

# WEI_mother_female: WEI_mother for females, 0 for males
data_for_analysis_no_imputation$WEI_mother_female <- ifelse(new_data$sex == "Female", new_data$WEI_mother, 0)

# WEI_mother_male: WEI_mother for males, 0 for females
data_for_analysis_no_imputation$WEI_mother_male <- ifelse(new_data$sex == "Male", new_data$WEI_mother, 0)

# WEI_father_female: WEI_father for females, 0 for males
data_for_analysis_no_imputation$WEI_father_female <- ifelse(new_data$sex == "Female", new_data$WEI_father, 0)

# WEI_father_male: WEI_father for males, 0 for females
data_for_analysis_no_imputation$WEI_father_male <- ifelse(new_data$sex == "Male", new_data$WEI_father, 0)

# Initialize the new columns with NA_real_
data_for_analysis_no_imputation$female_birth_year <- NA_real_
data_for_analysis_no_imputation$mother_attainment_female_birth_year <- NA_real_
data_for_analysis_no_imputation$mother_attainment_male_birth_year <- NA_real_
data_for_analysis_no_imputation$father_attainment_female_birth_year <- NA_real_
data_for_analysis_no_imputation$father_attainment_male_birth_year <- NA_real_

# Calculate the values for each new column using the numeric versions of attainment columns from new_data
data_for_analysis_no_imputation <- data_for_analysis_no_imputation %>%
  mutate(
    altered_birth_year = ((new_data$birth_year - 1900) / 100),
    
    # female_birth_year: 0 for males, 1*(birth_year-1900)/100 for females
    female_birth_year = ifelse(sex == 0, 0, (birth_year - 1900) / 100),
    
    # mother_attainment_female_birth_year: 0 for males, mother_attainment_numeric*((birth_year-1900)/100) for females
    mother_attainment_female_birth_year = ifelse(sex == 0, 0, new_data$attainment_mother_numeric * ((birth_year - 1900) / 100)),
    
    # mother_attainment_male_birth_year: 0 for females, mother_attainment_numeric*((birth_year-1900)/100) for males
    mother_attainment_male_birth_year = ifelse(sex == 1, 0, new_data$attainment_mother_numeric * ((birth_year - 1900) / 100)),
    
    # father_attainment_female_birth_year: 0 for males, father_attainment_numeric*((birth_year-1900)/100) for females
    father_attainment_female_birth_year = ifelse(sex == 0, 0, new_data$attainment_father_numeric * ((birth_year - 1900) / 100)),
    
    # father_attainment_male_birth_year: 0 for females, father_attainment_numeric*((birth_year-1900)/100) for males
    father_attainment_male_birth_year = ifelse(sex == 1, 0, new_data$attainment_father_numeric * ((birth_year - 1900) / 100))
  )

# Initialize method vector with empty strings
method_vector <- rep("", ncol(new_data))
names(method_vector) <- colnames(new_data)

# Define the method for each variable to be imputed
method_vector["birth_year"] <- "pmm"  # Predictive Mean Matching for continuous variables
method_vector["sex"] <- "logreg"  # Logistic regression for binary categorical variable
method_vector["attainment"] <- "polr"  # Proportional Odds Logistic Regression for ordinal variable
method_vector["attainment_mother"] <- "polr"  # Proportional Odds Logistic Regression for ordinal variable
method_vector["attainment_father"] <- "polr"  # Proportional Odds Logistic Regression for ordinal variable

# Define the predictor matrix as before
predictor_matrix <- make.predictorMatrix(new_data)

# Specify only the variables that should be used as predictors
predictor_matrix[, c("birth_year", "sex", "attainment", "attainment_mother", "attainment_father",
                     "occupation", "occupation_father", "children_number", "occupation_ISCO", "immigrant_status")] <- 1

# Ensure that the grouping variable 'survey_country' is excluded as a predictor but available for grouping
predictor_matrix["survey_country", ] <- 0  # Exclude it from predicting others
predictor_matrix[, "survey_country"] <- 0  # Exclude others from predicting it

# Run the multilevel imputation with survey_country as the grouping variable (this may take some time...)
imp <- mice(
  new_data, 
  method = method_vector, 
  predictorMatrix = predictor_matrix, 
  m = 10,  # Number of imputations
  maxit = 10,  # Number of iterations
  seed = 2012,  # For reproducibility
  cluster = new_data$survey_country,  # Grouping variable
  printFlag = TRUE
)

# Check the imputation results
summary(imp)

# Save imputed data as backup
wb <- createWorkbook()

# Loop through each imputed dataset, save each as a separate sheet
for (i in 1:10) {
  # Complete the i-th imputation
  imputed_data <- complete(imp, action = i)
  
  # Add the imputed data to a new sheet in the workbook
  addWorksheet(wb, paste0("Imputation_", i))
  writeData(wb, sheet = paste0("Imputation_", i), x = imputed_data)
}

# Save the workbook as an Excel file
saveWorkbook(wb, file = "imputed_data.xlsx", overwrite = TRUE)

# Extract the imputed dataset
imputed_data_1 <- complete(imp, action = 1)
imputed_data_2 <- complete(imp, action = 2)
imputed_data_3 <- complete(imp, action = 3)
imputed_data_4 <- complete(imp, action = 4)
imputed_data_5 <- complete(imp, action = 5)
imputed_data_6 <- complete(imp, action = 6)
imputed_data_7 <- complete(imp, action = 7)
imputed_data_8 <- complete(imp, action = 8)
imputed_data_9 <- complete(imp, action = 9)
imputed_data_10 <- complete(imp, action = 10)

# Initialize a list to store countries and variables that are completely missing
missing_data_report <- list()

# Function to perform random imputation within countries, or across all data if country data is missing
impute_random_within_country_or_global <- function(data) {
  
  # Check for missing data and update the report
  missing_report <- data %>%
    group_by(survey_country) %>%
    summarize(
      missing_birth_year = all(is.na(birth_year)),
      missing_sex = all(is.na(sex)),
      missing_attainment = all(is.na(attainment)),
      missing_attainment_mother = all(is.na(attainment_mother)),
      missing_attainment_father = all(is.na(attainment_father)),
      missing_WEI_15 = all(is.na(WEI_15)),
      missing_WEI_mother = all(is.na(WEI_mother)),
      missing_WEI_father = all(is.na(WEI_father))
    ) %>%
    filter(
      missing_birth_year | missing_sex | missing_attainment |
        missing_attainment_mother | missing_attainment_father |
        missing_WEI_15 | missing_WEI_mother | missing_WEI_father
    )
  
  # Append to the global missing data report
  missing_data_report <<- append(missing_data_report, list(missing_report))
  
  # Perform imputation
  data %>%
    group_by(survey_country) %>%
    mutate(
      birth_year = ifelse(is.na(birth_year),
                          ifelse(any(!is.na(birth_year)),
                                 sample(birth_year[!is.na(birth_year)], sum(is.na(birth_year)), replace = TRUE),
                                 sample(data$birth_year[!is.na(data$birth_year)], sum(is.na(birth_year)), replace = TRUE)),
                          birth_year),
      sex = ifelse(is.na(sex),
                   ifelse(any(!is.na(sex)),
                          sample(sex[!is.na(sex)], sum(is.na(sex)), replace = TRUE),
                          sample(data$sex[!is.na(data$sex)], sum(is.na(sex)), replace = TRUE)),
                   sex),
      attainment = ifelse(is.na(attainment),
                          ifelse(any(!is.na(attainment)),
                                 sample(attainment[!is.na(attainment)], sum(is.na(attainment)), replace = TRUE),
                                 sample(data$attainment[!is.na(data$attainment)], sum(is.na(attainment)), replace = TRUE)),
                          attainment),
      attainment_mother = ifelse(is.na(attainment_mother),
                                 ifelse(any(!is.na(attainment_mother)),
                                        sample(attainment_mother[!is.na(attainment_mother)], sum(is.na(attainment_mother)), replace = TRUE),
                                        sample(data$attainment_mother[!is.na(data$attainment_mother)], sum(is.na(attainment_mother)), replace = TRUE)),
                                 attainment_mother),
      attainment_father = ifelse(is.na(attainment_father),
                                 ifelse(any(!is.na(attainment_father)),
                                        sample(attainment_father[!is.na(attainment_father)], sum(is.na(attainment_father)), replace = TRUE),
                                        sample(data$attainment_father[!is.na(data$attainment_father)], sum(is.na(attainment_father)), replace = TRUE)),
                                 attainment_father),
      WEI_15 = ifelse(is.na(WEI_15),
                      ifelse(any(!is.na(WEI_15)),
                             sample(WEI_15[!is.na(WEI_15)], sum(is.na(WEI_15)), replace = TRUE),
                             sample(data$WEI_15[!is.na(data$WEI_15)], sum(is.na(WEI_15)), replace = TRUE)),
                      WEI_15),
      WEI_mother = ifelse(is.na(WEI_mother),
                          ifelse(any(!is.na(WEI_mother)),
                                 sample(WEI_mother[!is.na(WEI_mother)], sum(is.na(WEI_mother)), replace = TRUE),
                                 sample(data$WEI_mother[!is.na(data$WEI_mother)], sum(is.na(WEI_mother)), replace = TRUE)),
                          WEI_mother),
      WEI_father = ifelse(is.na(WEI_father),
                          ifelse(any(!is.na(WEI_father)),
                                 sample(WEI_father[!is.na(WEI_father)], sum(is.na(WEI_father)), replace = TRUE),
                                 sample(data$WEI_father[!is.na(data$WEI_father)], sum(is.na(WEI_father)), replace = TRUE)),
                          WEI_father)
    ) %>%
    ungroup()
}

# List of imputed datasets
imputed_datasets <- list(imputed_data_1, imputed_data_2, imputed_data_3, imputed_data_4, imputed_data_5, imputed_data_6, imputed_data_7, imputed_data_8, imputed_data_9, imputed_data_10)

# Apply the imputation function to each dataset
imputed_datasets <- lapply(imputed_datasets, impute_random_within_country_or_global)

# Combine the missing data report into a single dataframe
missing_data_report_df <- do.call(rbind, missing_data_report)

# View the missing data report
print(missing_data_report_df)

# Function to prepare imputed datasets
prepare_imputed_data <- function(imputed_data, original_data) {
  imputed_data_prepared <- original_data %>%
    transmute(
      case_ID = case_ID,
      survey_country = survey_country,
      attainment = attainment,
      birth_year = birth_year,
      
      sex = NA_real_,
      age = NA_real_,
      case_weight = NA_real_,
      female_immigrant_status = NA_real_,
      WEI_15_female = NA_real_,
      WEI_15_male = NA_real_,
      mother_attainment_female = NA_real_,
      mother_attainment_male = NA_real_,
      father_attainment_female = NA_real_,
      father_attainment_male = NA_real_,
      WEI_15_father_attainment_female = NA_real_,
      WEI_15_father_attainment_male = NA_real_,
      WEI_15_mother_attainment_female = NA_real_,
      WEI_15_mother_attainment_male = NA_real_,
      WEI_mother_female = NA_real_,
      WEI_mother_male = NA_real_,
      WEI_father_female = NA_real_,
      WEI_father_male = NA_real_,
      female_birth_year = NA_real_,
      mother_attainment_female_birth_year = NA_real_,
      mother_attainment_male_birth_year = NA_real_,
      father_attainment_female_birth_year = NA_real_,
      father_attainment_male_birth_year = NA_real_
    )
  
  # Populate columns based on imputed data
  imputed_data_prepared$sex <- ifelse(imputed_data$sex == "Male", 0, ifelse(imputed_data$sex == "Female", 1, NA))
  imputed_data_prepared$age <- imputed_data$age
  imputed_data_prepared$case_weight <- imputed_data$case_weight
  imputed_data_prepared$female_immigrant_status <- ifelse(imputed_data$sex == "Female" & imputed_data$immigrant_status == "Yes", 1, 0)
  imputed_data_prepared$WEI_15_female <- ifelse(imputed_data$sex == "Female", imputed_data$WEI_15, 0)
  imputed_data_prepared$WEI_15_male <- ifelse(imputed_data$sex == "Male", imputed_data$WEI_15, 0)
  imputed_data_prepared$mother_attainment_female <- ifelse(imputed_data$sex == "Female", imputed_data$attainment_mother, 0)
  imputed_data_prepared$mother_attainment_male <- ifelse(imputed_data$sex == "Male", imputed_data$attainment_mother, 0)
  imputed_data_prepared$father_attainment_female <- ifelse(imputed_data$sex == "Female", imputed_data$attainment_father, 0)
  imputed_data_prepared$father_attainment_male <- ifelse(imputed_data$sex == "Male", imputed_data$attainment_father, 0)
  imputed_data_prepared$WEI_15_father_attainment_female <- ifelse(imputed_data$sex == "Female", imputed_data$WEI_15 * imputed_data$attainment_father_numeric, 0)
  imputed_data_prepared$WEI_15_father_attainment_male <- ifelse(imputed_data$sex == "Male", imputed_data$WEI_15 * imputed_data$attainment_father_numeric, 0)
  imputed_data_prepared$WEI_15_mother_attainment_female <- ifelse(imputed_data$sex == "Female", imputed_data$WEI_15 * imputed_data$attainment_mother_numeric, 0)
  imputed_data_prepared$WEI_15_mother_attainment_male <- ifelse(imputed_data$sex == "Male", imputed_data$WEI_15 * imputed_data$attainment_mother_numeric, 0)
  imputed_data_prepared$WEI_mother_female <- ifelse(imputed_data$sex == "Female", imputed_data$WEI_mother, 0)
  imputed_data_prepared$WEI_mother_male <- ifelse(imputed_data$sex == "Male", imputed_data$WEI_mother, 0)
  imputed_data_prepared$WEI_father_female <- ifelse(imputed_data$sex == "Female", imputed_data$WEI_father, 0)
  imputed_data_prepared$WEI_father_male <- ifelse(imputed_data$sex == "Male", imputed_data$WEI_father, 0)
  
  imputed_data_prepared <- imputed_data_prepared %>%
    mutate(
      altered_birth_year = ((imputed_data$birth_year - 1900) / 100),
      female_birth_year = ifelse(sex == 0, 0, (birth_year - 1900) / 100),
      mother_attainment_female_birth_year = ifelse(sex == 0, 0, imputed_data$attainment_mother_numeric * ((birth_year - 1900) / 100)),
      mother_attainment_male_birth_year = ifelse(sex == 1, 0, imputed_data$attainment_mother_numeric * ((birth_year - 1900) / 100)),
      father_attainment_female_birth_year = ifelse(sex == 0, 0, imputed_data$attainment_father_numeric * ((birth_year - 1900) / 100)),
      father_attainment_male_birth_year = ifelse(sex == 1, 0, imputed_data$attainment_father_numeric * ((birth_year - 1900) / 100))
    )
  
  return(imputed_data_prepared)
}

# Apply the preparation function to all imputed datasets
imputed_datasets <- list(imputed_data_1, imputed_data_2, imputed_data_3, imputed_data_4, imputed_data_5, imputed_data_6, imputed_data_7, imputed_data_8, imputed_data_9, imputed_data_10)
data_for_analysis_imputed_list <- lapply(imputed_datasets, prepare_imputed_data, original_data = new_data)

# Name the datasets
names(data_for_analysis_imputed_list) <- paste0("data_for_analysis_imputed_", 1:10)

# Subset the data to include only the first 10 survey countries
subset_data <- data_for_analysis_no_imputation %>%
  filter(survey_country %in% unique(survey_country)[1:10])

# Check the subset data to ensure it has been correctly filtered
table(subset_data$survey_country)

### MODELLING PART ###

# Run the cumulative link mixed model (CLMM) with case weights NO IMPUTATION
clmm_model_no_imputation <- clmm(attainment ~
                                 sex +
                                 altered_birth_year +
                                 WEI_15_female +
                                 WEI_15_male +
                                 mother_attainment_female +
                                 mother_attainment_male +
                                 father_attainment_female +
                                 father_attainment_male +
                                 WEI_15_mother_attainment_female +
                                 WEI_15_mother_attainment_male +
                                 WEI_15_father_attainment_female +
                                 WEI_15_father_attainment_male +
                                 WEI_mother_female + WEI_mother_male +
                                 WEI_father_female +  WEI_father_male +
                                 female_birth_year +
                                 mother_attainment_female_birth_year +
                                 father_attainment_female_birth_year +
                                 mother_attainment_male_birth_year +
                                 father_attainment_male_birth_year +
                                 (1 | survey_country), 
                                 data = data_for_analysis_no_imputation,
                                 weights = case_weight  # Include case weights
)

# Summary of the model
summary(clmm_model_no_imputation)

# Save the complete model object
save(clmm_model_no_imputation, file = "clmm_model_no_imputation.RData")

# Extract model coefficients
model_summary <- summary(clmm_model_no_imputation)

# Create a data frame with the relevant information
results_df <- data.frame(
  Estimate = model_summary$coefficients[, "Estimate"],
  `Std. Error` = model_summary$coefficients[, "Std. Error"],
  `z value` = model_summary$coefficients[, "z value"],
  `Pr(>|z|)` = model_summary$coefficients[, "Pr(>|z|)"]
)

# Add row names as a column for clarity
results_df$Predictor <- rownames(results_df)

# Reorder columns to have 'Predictor' first
results_df <- results_df[, c("Predictor", "Estimate", "Std..Error", "z.value", "Pr...z..")]

# Remove the threshold estimates from the dataframe
fixed_effects_df <- results_df %>%
  filter(!grepl("\\|", Predictor))  # Exclude rows where "Predictor" column contains "|"

# Perform Benjamini-Hochberg correction

# Number of tests
m <- nrow(fixed_effects_df)

# Apply the Benjamini-Hochberg procedure
fixed_effects_df <- fixed_effects_df %>%
  mutate(
    BH_p_adj = `Pr...z..` * m / rank(`Pr...z..`)
  )

# Ensure the adjusted p-values do not exceed 1
fixed_effects_df$BH_p_adj <- pmin(fixed_effects_df$BH_p_adj, 1)

# Arrange back to the original order based on the initial order in results_df
fixed_effects_df <- fixed_effects_df[order(match(fixed_effects_df$Predictor, results_df$Predictor)), ]

# View the updated dataframe with adjusted p-values
print(fixed_effects_df)

# Export the results
write_xlsx(fixed_effects_df, "results_no_imputation.xlsx")

# Function to fit the CLMM model and extract the summary
fit_clmm_model <- function(data) {
  clmm_model <- clmm(attainment ~
                       sex +
                       altered_birth_year +
                       WEI_15_female + WEI_15_male +
                       mother_attainment_female +
                       mother_attainment_male +
                       father_attainment_female +
                       father_attainment_male +
                       WEI_15_mother_attainment_female +
                       WEI_15_mother_attainment_male +
                       WEI_15_father_attainment_female +
                       WEI_15_father_attainment_male +
                       WEI_mother_female +
                       WEI_mother_male +
                       WEI_father_female +
                       WEI_father_male +
                       female_birth_year +
                       mother_attainment_female_birth_year +
                       father_attainment_female_birth_year +
                       mother_attainment_male_birth_year +
                       father_attainment_male_birth_year +
                       (1 | survey_country), 
    data = data,
    weights = case_weight  # Include case weights
  )
  
  # Extract model coefficients
  model_summary <- summary(clmm_model)
  
  # Create a data frame with the relevant information
  results_df <- data.frame(
    Estimate = model_summary$coefficients[, "Estimate"],
    `Std. Error` = model_summary$coefficients[, "Std. Error"],
    `z value` = model_summary$coefficients[, "z value"],
    `Pr(>|z|)` = model_summary$coefficients[, "Pr(>|z|)"]
  )
  
  # Add row names as a column for clarity
  results_df$Predictor <- rownames(results_df)
  
  # Reorder columns to have 'Predictor' first
  results_df <- results_df[, c("Predictor", "Estimate", "Std..Error", "z.value", "Pr...z..")]
  
  return(results_df)
}

# Apply the model fitting function to all 10 prepared imputed datasets
results_list <- lapply(data_for_analysis_imputed_list, fit_clmm_model)

# Name the results data frames
names(results_list) <- paste0("results_imputation_", 1:10)

# Extract the relevant columns from each imputation's result
extract_results <- function(results_df) {
  results_df %>%
    select(Predictor, Estimate, `Std..Error`, `Pr...z..`)
}

# Apply extraction to all results
extracted_results <- lapply(results_list, extract_results)

# Combine results using Rubin's rules

# Assuming each result has the same predictors in the same order
combined_results <- extracted_results[[1]]
combined_results$Estimate <- NA
combined_results$`Std..Error` <- NA
combined_results$df_com <- NA

# Calculate the pooled estimate and standard error using Rubin's rules
for (i in 1:nrow(combined_results)) {
  estimates <- sapply(extracted_results, function(x) x$Estimate[i])
  std_errors <- sapply(extracted_results, function(x) x$`Std..Error`[i])
  
  # Rubin's rules
  m <- length(estimates)  # number of imputations
  pooled_estimate <- mean(estimates)
  within_var <- mean(std_errors^2)
  between_var <- var(estimates)
  
  # Pooled standard error
  pooled_se <- sqrt(within_var + (1 + 1/m) * between_var)
  
  # Store results
  combined_results$Estimate[i] <- pooled_estimate
  combined_results$`Std..Error`[i] <- pooled_se
  combined_results$df_com[i] <- pooled_estimate / pooled_se  # Degrees of freedom (z-value)
}

# Calculate pooled p-values based on z-values
combined_results$`Pr...z..` <- 2 * pnorm(-abs(combined_results$df_com))

# Add confidence intervals
combined_results <- combined_results %>%
  mutate(
    CI_lower = Estimate - 1.96 * `Std..Error`,
    CI_upper = Estimate + 1.96 * `Std..Error`
  )

# Remove the threshold estimates from the dataframe
fixed_effects_df <- combined_results %>%
  filter(!grepl("\\|", Predictor))  # Exclude rows where "Predictor" column contains "|"

# Perform Benjamini-Hochberg correction

# Number of tests
m <- nrow(fixed_effects_df)

# Apply the Benjamini-Hochberg procedure
fixed_effects_df <- fixed_effects_df %>%
  mutate(
    BH_p_adj = `Pr...z..` * m / rank(`Pr...z..`)
  )

# Ensure the adjusted p-values do not exceed 1
fixed_effects_df$BH_p_adj <- pmin(fixed_effects_df$BH_p_adj, 1)

# Arrange back to the original order based on the initial order in results_df
fixed_effects_df <- fixed_effects_df[order(match(fixed_effects_df$Predictor, combined_results$Predictor)), ]

# Final combined results with adjusted p-values
final_results <- fixed_effects_df %>%
  select(Predictor, Estimate, `Std..Error`, `Pr...z..`, BH_p_adj, CI_lower, CI_upper)

# Save the final results to an Excel file
write.xlsx(final_results, "final_combined_results.xlsx", rowNames = FALSE)
