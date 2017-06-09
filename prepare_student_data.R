library(cism)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tidyverse)
library(readxl)
library(sp)

# For most recent absenteeism and performance data: 
# Go to https://172.16.236.245:4443/OpenClinica/
# user:jbrew
# pass: normal

#######
# CENSUS
#######

# We need to control for
# - distance to school
# - distance to border
# - socioeconomic status

if('prepared_data.RData' %in% dir('data')){
  load('data/prepared_data.RData')
} else {
  
  #### MANHICA CENSUS
  # Get data from dssodk
  if('manhica_census_data.RData' %in% dir('data')){
    load('data/manhica_census_data.RData')
  } else {
    # Get manhica census data
    HOUSEHOLD_ECONOMICS_CORE <-
      cism::get_data(tab = 'HOUSEHOLD_ECONOMICS_CORE',
                     dbname = 'dssodk')
    INDIVIDUAL_DETAILS_CORE <-
      cism::get_data(tab = 'INDIVIDUAL_DETAILS_CORE',
                     dbname = 'dssodk')
    LOCATION_DETAILS_CORE <-
      cism::get_data(tab = 'LOCATION_DETAILS_CORE',
                     dbname = 'dssodk')
    location <-
      cism::get_data(tab = 'location',
                     dbname = 'openhds')
    save(HOUSEHOLD_ECONOMICS_CORE,
         INDIVIDUAL_DETAILS_CORE,
         LOCATION_DETAILS_CORE,
         location,
         file = 'data/manhica_census_data.RData')
  }
  
  # Clean up manhica data ---------------
  
  # Remove weirdness from column names
  names(LOCATION_DETAILS_CORE) <- 
    gsub('SEC1_|SEC2_|SEC3_|SEC4_|SEC5_', 
         '', 
         names(LOCATION_DETAILS_CORE))
  
  # Get smaller data
  LOCATION_DETAILS_CORE <- 
    LOCATION_DETAILS_CORE %>%
    dplyr::select(LOCATION_ID,
                  COVERAGE_MATERIAL,
                  FLOOR_MATERIAL,
                  HAS_KITCHEN,
                  ILUMINATION_FUEL,
                  IS_KITCHEN_INSIDE,
                  KITCHEN_FUEL,
                  KITCHEN_HAS_COVERAGE,
                  LATRINE_TYPE,
                  NR_CONSTRUCTIONS,
                  NR_HOUSE_DIVISIONS,
                  WALL_MATERIAL,
                  WATER_SOURCE) %>%
    filter(!duplicated(LOCATION_ID))
  INDIVIDUAL_DETAILS_CORE <-
    INDIVIDUAL_DETAILS_CORE %>%
    dplyr::select(LOCATION_ID,
                  PERM_ID,
                  DOB,
                  NAME,
                  GENDER,
                  EDUCATION,
                  OCUPATION)
  HOUSEHOLD_ECONOMICS_CORE <-
    HOUSEHOLD_ECONOMICS_CORE %>%
    dplyr::select(LOCATION_ID,
                  HAS_FREEZER,
                  HAS_GLACIER,
                  HAS_TV,
                  NR_OF_BIKE,
                  NR_OF_CAR,
                  NR_OF_CATTLE,
                  NR_OF_CHICKENS,
                  NR_OF_DUCK,
                  NR_OF_GOAT,
                  NR_OF_MOTO,
                  NR_OF_PIGS,
                  NR_OF_TRACTOR,
                  HAS_TRACTOR) %>%
    filter(!duplicated(LOCATION_ID))
  
  
  # Get dictionary for dssodk
  dictionary <- 
    get_dssodk_dictionary()
  # Remove anything with "nr_of"
  dictionary <-
    dictionary %>%
    filter(!grepl('nr_of', tolower(variable)))
  
  # Apply dictionary to LOCATION_DETAILS_CORE
  LOCATION_DETAILS_CORE <- data.frame(LOCATION_DETAILS_CORE)
  small_dictionary <- dictionary %>%
    filter(table == 'LOCATION_DETAILS_CORE',
           db == 'dssodk')
  for(j in 1:ncol(LOCATION_DETAILS_CORE)){
    message(j)
    this_column <- names(LOCATION_DETAILS_CORE)[j]
    if(grepl('nr_of', tolower(this_column))){
      LOCATION_DETAILS_CORE[,this_column] <- 
        ifelse(as.character(LOCATION_DETAILS_CORE[,this_column]) %in% c('88', '99'),
               NA,
               LOCATION_DETAILS_CORE[,this_column])
    }
    if(this_column %in% small_dictionary$variable){
      # Get the dictionary just for the variable in question
      sub_dictionary <- small_dictionary %>%
        filter(variable == this_column) %>%
        dplyr::select(old, answer_eng)
      # Replace the variable
      LOCATION_DETAILS_CORE$old <- LOCATION_DETAILS_CORE[,this_column]
      # If not the same type, coerce to character before join
      if(class(sub_dictionary$old) != 
         class(LOCATION_DETAILS_CORE$old)){
        sub_dictionary$old <- as.character(sub_dictionary$old)
        LOCATION_DETAILS_CORE$old <- as.character(LOCATION_DETAILS_CORE$old)
      }
      LOCATION_DETAILS_CORE <-
        LOCATION_DETAILS_CORE %>%
        left_join(sub_dictionary,
                  by = 'old')
      LOCATION_DETAILS_CORE[,this_column] <-
        LOCATION_DETAILS_CORE$answer_eng
      LOCATION_DETAILS_CORE$old <- NULL
      LOCATION_DETAILS_CORE$answer_eng <- NULL
    }
  }
  
  # Apply dictionary to INDIVIDUAL_DETAILS_CORE
  INDIVIDUAL_DETAILS_CORE <- data.frame(INDIVIDUAL_DETAILS_CORE)
  small_dictionary <- dictionary %>%
    filter(table == 'INDIVIDUAL_DETAILS_CORE',
           db == 'dssodk')
  for(j in 1:ncol(INDIVIDUAL_DETAILS_CORE)){
    message(j)
    this_column <- names(INDIVIDUAL_DETAILS_CORE)[j]
    if(grepl('nr_of', tolower(this_column))){
      INDIVIDUAL_DETAILS_CORE[,this_column] <- 
        ifelse(as.character(INDIVIDUAL_DETAILS_CORE[,this_column]) %in% c('88', '99'),
               NA,
               INDIVIDUAL_DETAILS_CORE[,this_column])
    }
    if(this_column %in% small_dictionary$variable){
      # Get the dictionary just for the variable in question
      sub_dictionary <- small_dictionary %>%
        filter(variable == this_column) %>%
        dplyr::select(old, answer_eng)
      # Replace the variable
      INDIVIDUAL_DETAILS_CORE$old <- INDIVIDUAL_DETAILS_CORE[,this_column]
      # If not the same type, coerce to character before join
      if(class(sub_dictionary$old) != 
         class(INDIVIDUAL_DETAILS_CORE$old)){
        sub_dictionary$old <- as.character(sub_dictionary$old)
        INDIVIDUAL_DETAILS_CORE$old <- as.character(INDIVIDUAL_DETAILS_CORE$old)
      }
      INDIVIDUAL_DETAILS_CORE <-
        INDIVIDUAL_DETAILS_CORE %>%
        left_join(sub_dictionary,
                  by = 'old')
      INDIVIDUAL_DETAILS_CORE[,this_column] <-
        INDIVIDUAL_DETAILS_CORE$answer_eng
      INDIVIDUAL_DETAILS_CORE$old <- NULL
      INDIVIDUAL_DETAILS_CORE$answer_eng <- NULL
    }
  }
  
  # Apply dictionary to HOUSEHOLD_ECONOMICS_CORE
  HOUSEHOLD_ECONOMICS_CORE <- data.frame(HOUSEHOLD_ECONOMICS_CORE)
  small_dictionary <- dictionary %>%
    filter(table == 'HOUSEHOLD_ECONOMICS_CORE',
           db == 'dssodk')
  for(j in 1:ncol(HOUSEHOLD_ECONOMICS_CORE)){
    message(j)
    this_column <- names(HOUSEHOLD_ECONOMICS_CORE)[j]
    if(grepl('nr_of', tolower(this_column))){
      HOUSEHOLD_ECONOMICS_CORE[,this_column] <- 
        ifelse(as.character(HOUSEHOLD_ECONOMICS_CORE[,this_column]) %in% c('88', '99'),
               NA,
               HOUSEHOLD_ECONOMICS_CORE[,this_column])
    }
    if(this_column %in% small_dictionary$variable){
      # Get the dictionary just for the variable in question
      sub_dictionary <- small_dictionary %>%
        filter(variable == this_column) %>%
        dplyr::select(old, answer_eng)
      # Replace the variable
      HOUSEHOLD_ECONOMICS_CORE$old <- HOUSEHOLD_ECONOMICS_CORE[,this_column]
      # If not the same type, coerce to character before join
      if(class(sub_dictionary$old) != 
         class(HOUSEHOLD_ECONOMICS_CORE$old)){
        sub_dictionary$old <- as.character(sub_dictionary$old)
        HOUSEHOLD_ECONOMICS_CORE$old <- as.character(HOUSEHOLD_ECONOMICS_CORE$old)
      }
      HOUSEHOLD_ECONOMICS_CORE <-
        HOUSEHOLD_ECONOMICS_CORE %>%
        left_join(sub_dictionary,
                  by = 'old')
      HOUSEHOLD_ECONOMICS_CORE[,this_column] <-
        HOUSEHOLD_ECONOMICS_CORE$answer_eng
      HOUSEHOLD_ECONOMICS_CORE$old <- NULL
      HOUSEHOLD_ECONOMICS_CORE$answer_eng <- NULL
    }
  }
  
  # Join individual with location
  manhica_people <-
    left_join(x = INDIVIDUAL_DETAILS_CORE,
              y = location %>%
                dplyr::select(extId,
                              longitude,
                              latitude),
              by = c('LOCATION_ID' = 'extId'))
  # Bring in information from the location_details
  manhica_people <-
    left_join(x = manhica_people,
              y = LOCATION_DETAILS_CORE,
              by = 'LOCATION_ID')
  # Bring in information form household economics
  manhica_people <-
    left_join(x = manhica_people,
              y = HOUSEHOLD_ECONOMICS_CORE,
              by = 'LOCATION_ID')
  # Specify the source
  manhica_people$district <- 'Manhiça'
  
  # remove some extra objects
  rm(HOUSEHOLD_ECONOMICS_CORE,
     INDIVIDUAL_DETAILS_CORE,
     location,
     LOCATION_DETAILS_CORE,
     small_dictionary,
     sub_dictionary,
     j,
     this_column,
     dictionary)
  
  
  
  # Magude census #################
  if('2016-12-07_HOUSEHOLD.RData' %in% dir('data')){
    load('data/2016-12-07_HOUSEHOLD.RData')
  } else {
    HOUSEHOLD <- get_data(dbname = 'MALTEM',
                          tab = 'HOUSEHOLD')
    save(HOUSEHOLD,
         file = 'data/2016-12-07_HOUSEHOLD.RData')
  }
  if('2016-12-07_MEMBER.RData' %in% dir('data')){
    load('data/2016-12-07_MEMBER.RData')
  } else {
    MEMBER <- get_data(dbname = 'MALTEM',
                       tab = 'MEMBER')
    save(MEMBER,
         file = 'data/2016-12-07_MEMBER.RData')
  }
  
  # Join member and household
  magude <- left_join(x = MEMBER,
                      y = HOUSEHOLD,
                      by = c('_PARENT_AURI'='_URI'))
  
  # Get a dictionary for translating responses
  dictionary <- get_maltem_dictionary()
  
  # Apply the dictionary to magude
  magude <- data.frame(magude)
  small_dictionary <- dictionary %>%
    filter(db == 'MALTEM')
  for(j in 1:ncol(magude)){
    message(j)
    this_column <- names(magude)[j]
    if(grepl('nr_of', tolower(this_column))){
      magude[,this_column] <- 
        ifelse(as.character(magude[,this_column]) %in% c('88', '90', '99'),
               NA,
               magude[,this_column])
    }
    if(this_column %in% small_dictionary$variable){
      # Get the dictionary just for the variable in question
      sub_dictionary <- small_dictionary %>%
        filter(variable == this_column) %>%
        dplyr::select(old, answer_eng)
      # Replace the variable
      magude$old <- magude[,this_column]
      
      # If not the same type, coerce to character before join
      if(class(sub_dictionary$old) != 
         class(magude$old)){
        sub_dictionary$old <- as.character(sub_dictionary$old)
        magude$old <- as.character(magude$old)
      }
      
      magude <-
        magude %>%
        left_join(sub_dictionary,
                  by = 'old')
      magude[,this_column] <-
        magude$answer_eng
      magude$old <- NULL
      magude$answer_eng <- NULL
    }
  }
  
  # Get geographic coordinates
  magude <-
    magude %>%
    mutate(latitude = as.numeric(as.character(HOUSEHOLD_HEAD_GPS_LAT)),
           longitude = as.numeric(as.character(HOUSEHOLD_HEAD_GPS_LNG)))
  
  # Define the source
  magude$district <- 'Magude'
  
  # Rename perm id
  magude <-
    magude %>%
    rename(PERM_ID = PERM_ID_MEMBER)
  
  # Rename those columns to match the ones in manhica census
  
  url_of_matcher <- 
    'https://docs.google.com/spreadsheets/d/1bOBq0scJv-id656YUIZAtlWaqObcHFoH6Kmz3gQpj5E/edit#gid=1923569214'
  matcher <- gsheet::gsheet2tbl(url_of_matcher)
  
  # Loop through each name in magude and manhica census and standardize
  for (j in 1:ncol(manhica_people)){
    this_column <- names(manhica_people)[j]
    if(this_column %in% matcher$manhica){
      new_name <- matcher$final[matcher$manhica == this_column]
      names(manhica_people)[j] <- new_name
    }
  }
  for (j in 1:ncol(magude)){
    this_column <- names(magude)[j]
    if(this_column %in% matcher$magude){
      new_name <- matcher$final[matcher$magude == this_column]
      names(magude)[j] <- new_name
    }
  }
  
  # Get birth day
  magude$dob <- as.Date(magude$BIRTH_MEMBER)
  
  # Rename a few colunns in magude
  magude <-
    magude %>%
    rename(name = MEMBER_NAME,
           sex = MEMBER_GENDER) %>%
    mutate(sex = ifelse(sex == 1, 'M', 
                        ifelse(sex == 2, 'F',
                               NA))) 
  
  # rename a few more columns
  manhica_people <- manhica_people %>%
    mutate(dob = as.Date(DOB)) %>%
    mutate(sex = ifelse(GENDER == 'male', 'M',
                        ifelse(GENDER == 'female', 'F', NA))) %>%
    rename(name = NAME) %>%
    mutate(latitude = as.numeric(as.character(latitude)),
           longitude = as.numeric(as.character(longitude)))
  
  # Lowercase permid in both places
  manhica_people$perm_id <- manhica_people$PERM_ID
  magude$perm_id <- magude$PERM_ID
  # Define which columns to keep
  keep <- c('name',
            'perm_id',
            'sex',
            'dob',
            'district',
            'longitude',
            'latitude',
            matcher$final)
  
  # Keep only those columns
  manhica_people <-
    manhica_people[,keep]
  magude <- magude[,keep]
  
  # Make sure types match
  for (j in 1:ncol(magude)){
    if(class(magude[,j]) != 
       class(manhica_people[,j])){
      magude[,j] <- as.character(magude[,j])
      manhica_people[,j] <- as.character(manhica_people[,j])
    }
  }
  
  
  # Combine manhica and magude into one
  census <- bind_rows(manhica_people, magude)
  census <- 
    census %>%
    filter(!duplicated(name, dob))
  
  # Fix classes
  census <- census %>%
    mutate(n_bikes = as.numeric(n_bikes),
           n_cars = as.numeric(n_cars),
           n_chickens = as.numeric(n_chickens),
           n_cows = as.numeric(n_cows),
           n_ducks = as.numeric(n_ducks),
           n_goats = as.numeric(n_goats),
           n_moto = as.numeric(n_moto),
           n_house_divisions = as.numeric(n_house_divisions),
           n_constructions = as.numeric(n_constructions),
           n_pigs = as.numeric(n_pigs))
  
  # Fix some more oddities
  census <-
    census %>%
    mutate(n_bikes = ifelse(n_bikes >= 10, NA, n_bikes),
           n_cars = ifelse(n_cars >= 10, NA, n_cars),
           n_chickens = ifelse(n_chickens >= 87, NA, n_chickens),
           n_cows = ifelse(n_cows >= 87, NA, n_cows),
           n_ducks = ifelse(n_ducks >= 87, NA, n_ducks),
           n_goats = ifelse(n_goats >= 87, NA, n_goats),
           n_moto = ifelse(n_moto >= 10, NA, n_moto),
           n_house_divisions = ifelse(n_house_divisions >= 20,
                                      NA,
                                      n_house_divisions),
           n_constructions = ifelse(n_constructions >= 30, 
                                    NA,
                                    n_constructions),
           n_pigs = ifelse(n_pigs >= 87,
                           NA,
                           n_pigs))
  
  # Keep only those who are between 5 and 15 years old
  # for the purposes of matching with students
  census_all <- census
  census <- 
    census %>%
    filter(dob <= '2012-01-01',
           dob >= '2000-01-01')
  
  # Make a spatial version too
  census_sp <- 
    census %>%
    filter(!is.na(longitude),
           !is.na(latitude)) %>%
    mutate(x = longitude,
           y = latitude)
  coordinates(census_sp) <- ~x+y
  proj4string(census_sp) <- proj4string(man3)
  
  # Remove unecessary objects
  rm(dictionary,
     HOUSEHOLD,
     magude,
     manhica_people,
     matcher,
     MEMBER,
     small_dictionary,
     sub_dictionary,
     keep,
     url_of_matcher)
  
  #######
  # PERFORMANCE
  #######
  performance <- read_csv('data/performance_7_june_extra_rows_removed.csv')
  
  # Clean up
  performance$school <- performance$`Study Subject ID`
  # unlist(lapply(strsplit(performance$`Study Subject ID`, '_'),
  #               function(x){
  #                 x[1]
  #               }))
  
  # Standardize school names
  performance <-
    performance %>%
    mutate(school = ifelse(grepl('GRACA|GRAÇA', school), 'GRACA MACHEL',
                           ifelse(grepl('JOSI|ILHA', school), 'ILHA JOSINA',
                                  ifelse(grepl('FAV|FEV|REIR', school), '3 DE FEV',
                                         ifelse(grepl('XINA|SIV', school), 'XINAVANE',
                                                ifelse(grepl('MAGUI|MAGGU', school), 'MAGUIGUANA',
                                                       ifelse(grepl('MARAG', school), 'MARAGRA',
                                                              ifelse(grepl('SIMB', school), 'SIMBE',
                                                                     ifelse(grepl('MOIN', school), 'MOINE', 
                                                                            ifelse(grepl('MAGU', school), 'MAGUDE', 
                                                                                   ifelse(grepl('DUCO|DUCA', school), 'DUCO', NA)))))))))))
  
  # Remove those with no school
  performance <- 
    performance %>%
    filter(!is.na(school))
  
  # Get year, grade, turma and number
  get_info <- function(x,
                       info = 'year'){
    y <- strsplit(x, '20')
    z <- unlist(lapply(y, function(a){
      if(length(a) > 1){
        a[[2]]
      } else {
        NA
      }
    }))
    z_split <- strsplit(z, '_')
    if(info == 'year'){
      out <- unlist(lapply(z_split, function(a){
        a[[1]]
      }))
      out <- as.numeric(paste0('20', out))
    } else if(info == 'grade'){
      out <- unlist(lapply(z_split, function(a){
        if(length(a) > 1){
          a[[2]]
        } else {
          NA
        }
      }))
      out <- as.numeric(out)
    } else if(info == 'turma'){
      out <- unlist(lapply(z_split, function(a){
        if(length(a) > 2){
          a[[3]]
        } else {
          NA
        }
      }))
    } else if(info == 'number'){
      out <- unlist(lapply(z_split, function(a){
        if(length(a) > 3){
          a[[4]]
        } else {
          NA
        }
      }))
      out <- as.numeric(as.character(out))
    }
    return(out)
  }
  
  # Get year, grade, turma and number
  performance <-
    performance %>%
    mutate(year = get_info(x = `Study Subject ID`,
                           info = 'year'),
           number = get_info(x = `Study Subject ID`,
                             info = 'grade'),
           letter = get_info(x = `Study Subject ID`,
                             info = 'turma'),
           roster_number = get_info(x = `Study Subject ID`,
                                    info = 'number'))
  
  # Remove those with no letter, etc.
  performance <- performance %>%
    filter(!is.na(letter),
           !is.na(number),
           !is.na(year))
  
  # Make long
  performance <- gather(performance, key, value, 
                        portest1_E2_C4:xichamedia_E2_C4)
  
  # Get district
  district_dictionary <- data_frame(school = c('3 DE FEV',
                                               'GRACA MACHEL',
                                               'ILHA JOSINA',
                                               'MAGUDE',
                                               'MAGUIGUANA',
                                               'MARAGRA',
                                               'MOINE',
                                               'SIMBE',
                                               'XINAVANE', # ***
                                               'DUCO'),
                                    district = c('Manhiça',
                                                 'Manhiça',
                                                 'Manhiça',
                                                 'Magude',
                                                 'Magude',
                                                 'Manhiça',
                                                 'Magude',
                                                 'Magude',
                                                 'Manhiça', # ***
                                                 'Magude'))
  
  # Join districts to performance data
  performance <-
    performance %>%
    left_join(district_dictionary,
              by = 'school')
  
  # Get trimester numbers
  performance$trimester <- unlist(
    lapply(strsplit(performance$key, '_'), function(x){
      y <- x[1]
      ncy <- nchar(y)
      substr(y, ncy, ncy)
    }))
  # Remove the media observations
  performance <- 
    performance %>%
    filter(trimester != 'a')
  
  # Get the name of the subject
  performance$subject <- unlist(
    lapply(strsplit(performance$key, '_'), function(x){
      y <- x[1]
      ncy <- nchar(y)
      substr(y, 1, (ncy - 1))
    }))
  # Remove the "test" part of the subject
  performance$subject <- gsub('test', '', performance$subject)
  
  # Create a dictionary of school subjects
  performance_dictionary <-
    data_frame(subject = c('cna', 
                           'csoc',
                           'efis',
                           'evis',
                           'ingles',
                           'ma',
                           'music',
                           'ofic',
                           'por',
                           'xicha'),
               subject_name = c('Natural science',
                                'Social science',
                                'Physical education',
                                'Visual education',
                                'English',
                                'Math',
                                'Music',
                                'Home economics',
                                'Portuguese',
                                NA))
  
  # Join the dictionary to the data
  performance <-
    performance %>%
    left_join(performance_dictionary,
              by = 'subject') %>%
    dplyr::select(-subject) %>%
    rename(subject = subject_name) 
  
  # Make value numeric
  performance$value <- as.numeric(as.character(performance$value))
  
  # Remove those without a turma letter
  performance <- performance %>%
    filter(is.na(as.numeric(letter)))
  
  # Cut down to only necessary columns
  performance <-
    performance %>%
    mutate(name = nome_E2_C4) %>%
    dplyr::select(name, 
                  district,
                  school,
                  year,
                  trimester, 
                  number,
                  letter,
                  subject,
                  key,
                  value)
  
  # Remove absurd values
  performance <- 
    performance %>%
    dplyr::filter(value <= 20, value >= 0)
  
  # Remove those with no year
  performance <- 
    performance %>%
    filter(!is.na(year))
  
  # Remove unecessary objects
  rm(district_dictionary,
     performance_dictionary,
     get_info)
  
  # Clean up the oddities
  performance <-
    performance %>%
    mutate(letter = ifelse(letter == 'U', 'UNICA',
                           ifelse(grepl('B', letter), 'B',
                                  letter)))
  
  performance <- 
    performance %>%
    mutate(number = ifelse(number > 5, NA, number)) %>%
    filter(!is.na(number),
           !is.na(letter))
  
  
  ########
  # ABSENTEEISM
  ########
  
  ab <- read_csv('data/absenteeism_7_june_extra_rows_removed.csv')
  
  # Remove those with no name
  ab <- ab[,!is.na(names(ab))]
  
  # Clean up
  ab$school <- 
    unlist(lapply(strsplit(ab$`Study Subject ID`, '_'),
                  function(x){
                    x[1]
                  }))
  
  ab$year <-
    as.numeric(unlist(lapply(strsplit(ab$`Study Subject ID`, '_'),
                             function(x){
                               x[2]
                             })))
  
  ab$number <- 
    as.numeric(unlist(lapply(strsplit(ab$`Study Subject ID`, '_'),
                             function(x){
                               x[3]
                             })))
  
  ab$letter <- 
    unlist(lapply(strsplit(ab$`Study Subject ID`, '_'),
                  function(x){
                    x[4]
                  }))
  
  ab$roster_number <- 
    as.numeric(unlist(lapply(strsplit(ab$`Study Subject ID`, '_'),
                             function(x){
                               x[5]
                             })))
  
  ab$turma <- paste0(ab$number, '-', ab$letter)
  
  # Make long
  ab <- gather(ab, key, value, dplyr::contains('dia'))
  
  # Remove all those days with no observation
  ab <- ab %>% filter(!is.na(value))
  
  # Create an absence variable
  ab$absent <- ifelse(ab$value %in% c('FALSE', 'F'), TRUE,
                      ifelse(ab$value == 'P', FALSE, 
                             NA))
  
  # Define dataframe for matching months, etc.
  month_matcher <- data_frame(month_number = 1:12,
                              month_name = c('jan',
                                             'fev',
                                             'mar',
                                             'abr',
                                             'mai',
                                             'jun',
                                             'jul',
                                             'ago',
                                             'set',
                                             'out',
                                             'nov',
                                             'dec'))
  
  # Get month number based on date
  ab$month_name <- substr(ab$key, 1, 3)
  ab <- left_join(x = ab,
                  y = month_matcher,
                  by = 'month_name')
  rm(month_matcher)
  
  # Get day number
  ab$day_number <- 
    unlist(lapply(strsplit(ab$key, '_'), function(x){
      z <- x[1]
      as.numeric(substr(z, 7, nchar(z)))
    }))
  
  # Get a year matcher
  ab$year2 <- ifelse(grepl('C2', ab$key), 2016,
                     ifelse(grepl('C1', ab$key), 2015,
                            NA))
  
  # Create a date
  ab$date <- as.Date(paste0(ab$year, 
                            '-',
                            ab$month_number,
                            '-',
                            ab$day_number))
  
  # Get day of week
  ab$dow <- weekdays(ab$date)
  
  # Standardize school names
  # # cat(paste0('"', sort(unique(ab$school)), '"', collapse = ',\n'))
  school_standardizer <-
    data_frame(school = c("1 E 2 GRAU XINAVANE",
                          "1E 2 GRAU XINAVANE",
                          "3 DE FEVEREIRO",
                          "3 DE FEVEREIRO ",
                          "3 DE FEVREIRO",
                          "DUCA",
                          "DUCO",
                          "EPC GRACA MACHEL",
                          "GRACA MACHEL",
                          "ILHA",
                          "ILHA JOSINA",
                          "JOSINA",
                          "JOSINA MACHEL",
                          "MAGUIGUANA",
                          "MARAGRA",
                          "MOINE",
                          "SIMBE",
                          "XINAVANE",
                          "XINAVENE",
                          "XIVANANE"),
               new_school = c('Xinavane', 
                              'Xinavane',
                              '3 de Fev',
                              '3 de Fev',
                              '3 de Fev',
                              'Duco',
                              'Duco',
                              'Graca Machel',
                              'Graca Machel',
                              'Ilha Josina',
                              'Ilha Josina',
                              'Ilha Josina',
                              'Ilha Josina',
                              'Maguiguana',
                              'Maragra',
                              'Moine',
                              'Simbe',
                              'Xinavane',
                              'Xinavane',
                              'Xinavane'))
  ab <- left_join(x = ab,
                  y = school_standardizer,
                  by = 'school') %>%
    dplyr::select(-school) %>%
    rename(school = new_school) %>%
    filter(!is.na(school))
  rm(school_standardizer)
  
  # Define geography
  # # cat(paste0('"', sort(unique(ab$school)), '"', collapse = ',\n'))
  geography <- data_frame(school = c("3 de Fev",
                                     "Duco",
                                     "Graca Machel",
                                     "Maguiguana",
                                     "Maragra",
                                     "Moine",
                                     "Simbe",
                                     "Xinavane",
                                     "Ilha Josina"),
                          district = c('Manhiça',
                                       'Magude',
                                       'Manhiça',
                                       'Magude',
                                       'Manhiça',
                                       'Magude',
                                       'Magude',
                                       'Manhiça',
                                       'Manhiça'))
  ab <- left_join(x = ab,
                  y= geography,
                  by = 'school')
  rm(geography)
  
  # Get a truncated month
  ab$month <- as.Date(paste0(ab$year, '-', 
                             ab$month_number, '-01'))
  
  # Remove those before 2015
  ab <- ab %>%
    filter(year >= 2015)
  
  # Create a year_term variable
  ab$term <-
    ifelse(as.numeric(format(ab$month, '%m')) %in% 2:4, '1',
           ifelse(as.numeric(format(ab$month, '%m')) %in% 5:7, '2',
                  ifelse(as.numeric(format(ab$month, '%m')) %in% 8:10, '3',
                         NA)))
  ab$year_term <- paste0(ab$year,
                         '-',
                         ab$term)
  
  # Get a name
  ab <- ab %>% mutate(name = nome_E1_C1)

  # Cut down to only necessary variables
  ab <- ab %>%
    dplyr::select(name,
                  district,
                  school,
                  year, 
                  year_term,
                  term,
                  month,
                  month_name,
                  month_number,
                  date,
                  dow,
                  number,
                  letter,
                  turma,
                  absent)
  
  # Standardize school names to match those in performance
  school_dictionary <- data_frame(school = c('3 de Fev',
                                             'Duco',
                                             'Graca Machel',
                                             'Ilha Josina',
                                             'Maguiguana',
                                             'Maragra',
                                             'Moine',
                                             'Simbe',
                                             'Xinavane'),
                                  new_school = c('3 DE FEV',
                                                 'DUCO',
                                                 'GRACA MACHEL',
                                                 'ILHA JOSINA',
                                                 'MAGUIGUANA',
                                                 'MARAGRA',
                                                 'MOINE',
                                                 'SIMBE',
                                                 'XINAVANE'))
  ab <-
    ab %>%
    left_join(school_dictionary,
              by = 'school') %>%
    mutate(school = new_school) %>%
    dplyr::select(-new_school)
  
  # Remove unecessary objects
  rm(school_dictionary)
  
  # Clean up oddities
  ab <-
    ab %>%
    mutate(letter = ifelse(letter %in% c('1', '6', '7', '8', '9', 'T'), NA,
                           ifelse(letter == 'U', 'UNICA',
                                  letter)))
  ab <- ab %>%
    mutate(number = ifelse(number > 5, NA, number)) %>%
    filter(!is.na(number),
           !is.na(letter))
  
  #####
  # SCHOOL LOCATIONS
  #####
  
  # Get locations
  geo <-
    data_frame(school = c('3 DE FEV',
                          'GRACA MACHEL',
                          'MAGUDE',
                          'MAGUIGUANA',
                          'MARAGRA',
                          'MOINE',
                          'SIMBE',
                          'XINAVANE',
                          'DUCO',
                          'ILHA JOSINA'),
               lng = c(32.796798928,
                       32.9282333538,
                       32.6450501,
                       32.6698304014,
                       32.7780356379,
                       32.5305054949,
                       32.5159635997,
                       32.7900299315,
                       32.5416627188,
                       32.923104),
               lat = c(-25.157889943,
                       -25.0965166999,
                       -25.0259158,
                       -25.0381675415,
                       -25.4528053754,
                       -24.8901998727,
                       -24.8185362371,
                       -25.0441123614,
                       -24.9252875459,
                       -25.093706))
  
  ########
  # STANDARDIZING
  ########
  
  # Standardize names
  df_names <- data_frame(name = sort(unique(c(performance$name,
                                              ab$name))))
  df_names <- 
    left_join(df_names,
              performance %>%
                dplyr::select(name, school) %>%
                filter(!duplicated(name)),
              by = 'name') %>%
    left_join(ab %>%
                dplyr::select(name,
                              school) %>%
                rename(ab_school = school) %>%
                filter(!duplicated(name)),
              by = 'name') %>%
    mutate(name_source = ifelse(is.na(school), 'ab', 'school')) %>%
    mutate(school = ifelse(is.na(school),
                           ab_school,
                           school)) %>%
    dplyr::select(-ab_school)
  
  df_fuzzy <- cism::fuzzy_match(x = df_names$name)
  
  # Loop through each name, changing it if similar enough to another
  df_names$new_name <- df_names$name
  df_names$already_changed <- FALSE
  df_names$master <- FALSE
  df_names$copies <- 0
  df_names$index <- 1:nrow(df_names)
  threshold <- 0.1
  n_names <- nrow(df_names)
  for (i in 1:length(df_names$name)){
    message(paste0(i, ' of ', n_names))
    # Only change names for those which haven't been modified yet
    if(!df_names$already_changed[i]){
      this_name <- df_names$name[i]
      this_school <- df_names$school[i]
      # Scores
      scores <- df_fuzzy[i,]
      scores_df <- df_names %>%
        mutate(score = scores)
      
      # Remove from scores_df the identical rows
      scores_df <-
        scores_df %>%
        filter(score > 0)
      
      # Identify if there is anything below the threshold
      scores_df <- scores_df %>% filter(score < threshold)
      
      # Keep only those matches with the same school
      scores_df <- 
        scores_df %>%
        filter(school == this_school)
      
      # If there are names below the threshold, change them all
      if(nrow(scores_df) > 0){
        # Make the change to the name
        df_names$new_name[df_names$index %in% scores_df$index] <-
          this_name
        # DON'T revisit this name (ie, it's already been changed)
        df_names$already_changed[df_names$index %in% scores_df$index] <- TRUE
        # Flag this as being the "master" name
        df_names$master[i] <- TRUE
        # Designate how many copies it took
        df_names$copies[i] <- nrow(scores_df)
      } 
    }
  }
  
  # Fix the names in performance and ab to be standardized
  ab <- 
    ab %>%
    left_join(df_names %>%
                dplyr::select(name, 
                              new_name),
              by = 'name') %>%
    mutate(name = new_name) %>%
    dplyr::select(-new_name)
  
  performance <- 
    performance %>%
    left_join(df_names %>%
                dplyr::select(name, 
                              new_name),
              by = 'name') %>%
    mutate(name = new_name) %>%
    dplyr::select(-new_name)
  
  # Identify the closest match in the census
  
  # Get a new dataframe of names
  df_names <- data_frame(name = sort(unique(c(performance$name,
                                              ab$name))))
  df_names <- 
    left_join(df_names,
              performance %>%
                dplyr::select(name, school) %>%
                filter(!duplicated(name)),
              by = 'name') %>%
    left_join(ab %>%
                dplyr::select(name,
                              school) %>%
                rename(ab_school = school) %>%
                filter(!duplicated(name)),
              by = 'name') %>%
    mutate(school = ifelse(is.na(school),
                           ab_school,
                           school)) %>%
    dplyr::select(-ab_school)
  
  # Get a match with the census
  census$name <- gsub("[^[:alnum:]///' ]", "", census$name)
  census_names <- sort(unique(census$name))
  fuzzy_census <- cism::fuzzy_match(x = df_names$name,
                                    y = census_names)
  
  # Loop through each name in our dataframe of names
  # trying to identify the corresponding person in the census
  n_names <- length(df_names$name)
  df_names$census_name <- NA
  df_names$confidence_score <- NA
  for (i in 1:length(df_names$name)){
    message(paste0(i, ' of ', n_names))
    
    this_name <- df_names$name[i]
    # Scores
    scores <- fuzzy_census[i,]
    scores_df <- data_frame(name = census_names,
                            score = scores)
    
    # Get the name with the lowest score
    best_index <- which.min(scores_df$score)[1]
    best_name <- scores_df$name[best_index]
    
    # Plug that name into the dataframe of names
    df_names$census_name[i] <- best_name
    
    # Record the confidence score
    df_names$confidence_score[i] <- scores_df$score[best_index]
  }
  
  # Get distance to the school of `name` from the coordinates of `census_name`
  
  # Apply a differential threshoold: very low/strict for those with high km
  # and somewhat permissive for those with low km
  
  # Get the coordinates of  the school
  df_names <-
    df_names %>%
    left_join(geo,
              by = 'school')
  
  # Get the coordinates of the person
  df_names <-
    df_names %>%
    left_join(census %>%
                mutate(census_name = name) %>%
                dplyr::select(census_name,
                              latitude,
                              longitude),
              by = 'census_name')
  
  # Calculate the distance
  df_names$distance_to_school <- NA
  for (i in 1:nrow(df_names)){
    message(i)
    df_names$distance_to_school[i] <-
      geosphere::distVincentyEllipsoid(p1 = c(df_names$lng[i], 
                                              df_names$lat[i]),
                                       p2 = c(df_names$longitude[i],
                                              df_names$latitude[i]))
  }
  df_names$km <- df_names$distance_to_school / 1000
  
  # View(df_names %>%
  #        dplyr::select(name,
  #                      census_name,
  #                      confidence_score,
  #                      km) %>%
  #        arrange(confidence_score, km)) 
  
  
  # Manually review the matches, and define rules
  df_names$okay <- FALSE
  df_names$okay <-
    ifelse(df_names$confidence_score <0.05, TRUE,
           ifelse(df_names$confidence_score < 0.1 &
                    (is.na(df_names$km) | df_names$km < 30), TRUE,
                  ifelse(df_names$confidence_score < 0.15 &
                           (is.na(df_names$km) | df_names$km < 15), TRUE,
                         ifelse(df_names$confidence_score < 0.2 &
                                  (is.na(df_names$km) | df_names$km < 10), TRUE, FALSE))))
  
  # Matched about half
  
  # Remove those which don't meet our rules
  df_names <-
    df_names %>%
    filter(okay) %>%
    dplyr::select(name,
                  school,
                  census_name,
                  latitude,
                  longitude,
                  km) %>%
    rename(km_to_school = km)
  
  # Join the census names to ab and performance
  ab <-
    left_join(ab,
              df_names,
              by = 'name')
  performance <- 
    left_join(performance,
              df_names,
              by = 'name')
  
  # Clean up a little
  performance <- 
    performance %>%
    rename(school = school.x) %>%
    dplyr::select(-school.y)
  
  # Remov unecessary objects
  rm(best_index,
     best_name,
     census_names,
     i,
     n_names,
     scores,
     this_name,
     this_school,
     threshold,
     df_fuzzy,
     df_names,
     fuzzy_census,
     scores_df,
     j,
     new_name,
     this_column)
  
  # Rename name in census
  census <-
    census %>%
    rename(census_name = name)
  
  # Create a dataset of unique student name / ids
  students <- 
    data_frame(name = sort(unique(c(performance$name,
                                    ab$name))))
  
  # Create an id number
  students$id <- 1:nrow(students)
  
  # Get the name in the census
  students <-
    students %>%
    left_join(ab %>%
                filter(!duplicated(name)) %>%
                dplyr::select(name, census_name) %>%
                rename(census_name_ab = census_name),
              by = 'name') %>%
    left_join(performance %>%
                filter(!duplicated(name)) %>%
                dplyr::select(name, census_name) %>%
                rename(census_name_performance = census_name),
              by = 'name')
  
  # Clean up names
  students <-
    students %>%
    mutate(census_name = ifelse(is.na(census_name_ab), census_name_performance,
                                ifelse(is.na(census_name_performance),
                                       census_name_ab,
                                       census_name_performance))) %>%
    dplyr::select(-census_name_ab,
                  -census_name_performance)
  
  # Clarify whether there is information in the census, etc.
  students <-
    students %>%
    mutate(in_census = !is.na(census_name),
           in_absenteeism = name %in% unique(ab$name),
           in_performance = name %in% unique(performance$name),
           in_absenteeism_2015 = name %in% unique(ab$name[ab$year == 2015]),
           in_absenteeism_2016 = name %in% unique(ab$name[ab$year == 2016]),
           in_performance_2015 = name %in% unique(performance$name[performance$year == 2015]),
           in_performance_2016 = name %in% unique(performance$name[performance$year == 2016]))
  
  # Clean up a bit
  ab$school <- ab$school.x; ab$school.x <- ab$school.y <- NULL
  
  # Remove those rows of census with no students
  census <- 
    census %>%
    left_join(students %>%
                filter(in_census) %>%
                dplyr::select(census_name) %>%
                mutate(keep = TRUE) %>%
                filter(!duplicated(census_name)),
              by = 'census_name') %>%
    filter(!is.na(keep))
  
  # Get kilometers to school in students
  # and remove from other places
  ab$km_to_school <- NULL
  performance$km_to_school <- NULL
  students <-
    students %>%
    left_join(census %>%
                dplyr::select(census_name,
                              longitude,
                              latitude),
              by = 'census_name')
  # Get school too
  students <-
    students %>%
    left_join(ab %>%
                filter(!duplicated(name)) %>%
                dplyr::select(name,
                              school) %>%
                rename(school_ab = school),
              by = 'name') %>%
    left_join(performance %>%
                filter(!duplicated(name)) %>%
                dplyr::select(name,
                              school) %>%
                rename(school_performance = school),
              by = 'name') %>%
    mutate(school = ifelse(is.na(school_ab),
                           school_performance,
                           ifelse(is.na(school_performance),
                                  school_ab,
                                  school_ab))) %>%
    dplyr::select(-school_performance,
                  -school_ab)
  
  students$distance_to_school <- NA
  for (i in 1:nrow(students)){
    message(i)
    if(!is.na(students$in_census[i])){
      this_school <- students$school[i]
      this_school_location <- geo %>%
        filter(school == this_school)
      students$distance_to_school[i] <-
        geosphere::distVincentyEllipsoid(p1 = c(students$longitude[i], 
                                                students$latitude[i]),
                                         p2 = c(this_school_location$lng,
                                                this_school_location$lat))
    }
  }
  
  # Convert to km
  students <- 
    students %>%
    mutate(km_to_school = distance_to_school / 1000) %>%
    dplyr::select(-distance_to_school)
  
  # Get kilometers from nearest health facility
  us <- cism::us
  us <- us %>% filter(province == 'MAPUTO PROVINCIA',
                      district %in% c('MAGUDE', 'MANHICA'))
  students$km_to_health_facility <- NA
  students$name_of_health_facility <- NA
  
  for (i in 1:nrow(students)){
    message(i)
    distances <- rep(NA, nrow(us))
    for(j in 1:nrow(us)){
      distances[j] <- 
        geosphere::distVincentyEllipsoid(p1 = c(students$longitude[i], 
                                                students$latitude[i]),
                                         p2 = c(us$longitude[j],
                                                us$latitude[j]))
    }
    # Get the minimum
    us_index <- which.min(distances)
    us_index <- us_index[1]
    # Get the name and distance
    students$km_to_health_facility[i] <- distances[us_index] / 1000
    students$name_of_health_facility[i] <- us$name[us_index]
  }
  
  # Get the district too
  students <-
    students %>%
    left_join(ab %>%
                filter(!duplicated(name)) %>%
                dplyr::select(name,
                              district) %>%
                rename(district_ab = district),
              by = 'name') %>%
    left_join(performance %>%
                filter(!duplicated(name)) %>%
                dplyr::select(name,
                              district) %>%
                rename(district_performance = district),
              by = 'name') %>%
    mutate(district = ifelse(is.na(district_ab),
                             district_performance,
                             ifelse(is.na(district_performance),
                                    district_ab,
                                    district_ab))) %>%
    dplyr::select(-district_performance,
                  -district_ab)
  
  # Get the distance to other district
  students$km_to_other_district_student <- NA
  students$km_to_other_district_school <- NA
  mag2 <- cism::mag2
  man2 <- cism::man2
  
  for (i in 1:nrow(students)){
    message(i)
    
    # Get the school
    this_school <- students$school[i]
    this_geo <- geo %>% filter(school == this_school)
    
    # Get location of student
    location_student <- c(students$longitude[i],
                          students$latitude[i])
    
    # Get the location of the school
    location_school <- c(this_geo$lng, this_geo$lat)
    
    # Get the district
    this_district <- students$district[i]
    the_other_district <- ifelse(this_district == 'Magude',
                                 'Manhiça', 'Magude')
    if(the_other_district == 'Magude'){
      the_other_polygon <- mag2
    } else {
      the_other_polygon <- man2
    }
    
    # Get the distance to the line for the student
    if(all(!is.na(location_student))){
      distance_student <- 
        as.numeric(geosphere::dist2Line(p = location_student, 
                                        line = the_other_polygon)[,1]) / 1000
      
      students$km_to_other_district_student[i] <- distance_student
    }
    
    # Get the distance to the line for the school
    distance_school <- 
      as.numeric(geosphere::dist2Line(p = location_school, 
                                      line = the_other_polygon)[,1]) / 1000
    
    students$km_to_other_district_school[i] <- distance_school
  }
  
  # Bring id into the other datasets
  ab <-
    left_join(ab,
              students %>%
                dplyr::select(name, id),
              by = 'name')
  performance <-
    left_join(performance,
              students %>%
                dplyr::select(name, id),
              by = 'name')
  
  # Combine students and census (and just get rid of census)
  students <- 
    left_join(students,
              census %>%
                rename(district_census = district) %>%
                dplyr::select(-longitude,
                              -latitude),
              by = 'census_name')
  rm(census)
  
  # Having now matched, get the census for everyone
  census <- census_all
  rm(census_all)
  
  # Get agregado-level data for census
  census <- census %>%
    mutate(agregado = substr(perm_id, 1, 8))
  census <- census %>%
    left_join(census %>%
                group_by(agregado) %>%
                summarise(n_residents_of_agregado = n(),
                          jobs_in_agregado = paste0(sort(unique(job)), collapse = ', '),
                          n_adults_agregado = length(which(dob <= '1999-01-01')),
                          n_children_agregado = length(which(dob > '1999-01-01'))),
              by = 'agregado')
  
  
  dictionaries <- dir('data_dictionaries/')
  dictionaries <- dictionaries[grepl('dictionary.txt', dictionaries)]
  for (i in 1:length(dictionaries)){
    this_file <- dictionaries[i]
    this_name <- gsub('.txt', '', this_file)
    assign(this_name,
           read_delim(paste0('data_dictionaries/', this_file), 
                      delim = '\t'))
  }
  
  # Define the variables that need modifying
  # - Each of the 12 variables' answers have received a puntuation that can go from 0 to 10 (please, first check that out, any variable should be pointed more than 10!). 
  # - After we have assigned a different puntuation to each answer, we then need to sum all the points for each household (for all the 12 variables), but ponderate each of the variables according to the criteria defined in the document "weights". 
  variables <- gsub('_dictionary.txt', '', dictionaries)
  variables[variables == 'educ'] <- 'education'
  
  # Loop through each variable, creating a scored one
  # for (j in 1:length(variables)){
  for (j in 1:length(variables)){
    this_variable <- variables[j]
    these_data <- census %>%
      dplyr::select_(this_variable)
    this_dictionary <- get(paste0(this_variable, '_dictionary'))
    # Join with the dictionary
    out <- 
      left_join(these_data,
                this_dictionary) 
    out <- out[,2:ncol(out)]
    # Bind to census
    census <- cbind(census, out)
    rm(list = paste0(this_variable, '_dictionary'))
  }
  rm(these_data, out)
  
  # Read in the weights
  weights <- read_delim('data_dictionaries/weights.txt',
                        delim = '\t')
  
  # Create an ses_asset_score
  census <- census %>%
    mutate(ses_asset_score = 
             (ifelse(is.na(n_bikes), mean(n_bikes, na.rm = TRUE), n_bikes) * 80) +
             (ifelse(is.na(n_cars), mean(n_cars, na.rm = TRUE), n_cars) * 4000) +
             (ifelse(is.na(n_chickens), mean(n_chickens, na.rm = TRUE), n_chickens) * 7) +
             (ifelse(is.na(n_cows), mean(n_cows, na.rm = TRUE), n_cows) * 80) +
             (ifelse(is.na(n_ducks), mean(n_ducks, na.rm = TRUE), n_ducks) * 12) +
             (ifelse(is.na(has_freezer), mean(has_freezer == 'yes', na.rm = TRUE), has_freezer == 'yes') * 150) +
             (ifelse(is.na(has_glacier), mean(has_glacier == 'yes', na.rm = TRUE), has_glacier == 'yes') * 120) +
             (ifelse(is.na(n_goats), mean(n_goats, na.rm = TRUE), n_goats) * 30) +
             (ifelse(is.na(n_moto), mean(n_moto, na.rm = TRUE), n_moto) * 400) +
             (ifelse(is.na(n_pigs), mean(n_pigs, na.rm = TRUE), n_pigs) * 60) +
             (ifelse(is.na(has_tractor), mean(has_tractor == 'yes', na.rm = TRUE), has_tractor == 'yes') * 2000) +
             (ifelse(is.na(has_tv), mean(has_tv == 'yes', na.rm = TRUE), has_tv == 'yes') * 150))
  
  # Create an ses_conditions_score
  census <- 
    census %>%
    mutate(ses_conditions_score = 
             (ifelse(is.na(coverage_material_points),
                     mean(coverage_material_points, na.rm = TRUE),
                     coverage_material_points) *
                weights$weight[weights$variables_household_conditions == 'coverage_material']) +
             (ifelse(is.na(floor_material_points),
                     mean(floor_material_points, na.rm = TRUE),
                     floor_material_points) *
                weights$weight[weights$variables_household_conditions == 'floor_material']) +
             (ifelse(is.na(fuel_for_cooking_points),
                     mean(fuel_for_cooking_points, na.rm = TRUE),
                     fuel_for_cooking_points) *
                weights$weight[weights$variables_household_conditions == 'fuel_for_cooking']) +
             (ifelse(is.na(fuel_for_lighting_points),
                     mean(fuel_for_lighting_points, na.rm = TRUE),
                     fuel_for_lighting_points) *
                weights$weight[weights$variables_household_conditions == 'fuel_for_lighting']) +
             (ifelse(is.na(has_kitchen_points),
                     mean(has_kitchen_points, na.rm = TRUE),
                     has_kitchen_points) *
                weights$weight[weights$variables_household_conditions == 'has_kitchen']) +
             (ifelse(is.na(is_kitchen_covered_points),
                     mean(is_kitchen_covered_points, na.rm = TRUE),
                     is_kitchen_covered_points) *
                weights$weight[weights$variables_household_conditions == 'is_kitchen_covered']) +
             (ifelse(is.na(is_kitchen_inside_points),
                     mean(is_kitchen_inside_points, na.rm = TRUE),
                     is_kitchen_inside_points) *
                weights$weight[weights$variables_household_conditions == 'is_kitchen_inside']) +
             (ifelse(is.na(latrine_type_points),
                     mean(latrine_type_points, na.rm = TRUE),
                     latrine_type_points) *
                weights$weight[weights$variables_household_conditions == 'latrine_type']) +
             (ifelse(is.na(n_constructions_points),
                     mean(n_constructions_points, na.rm = TRUE),
                     n_constructions_points) *
                weights$weight[weights$variables_household_conditions == 'n_constructions']) +
             (ifelse(is.na(n_house_divisions_points),
                     mean(n_house_divisions_points, na.rm = TRUE),
                     n_house_divisions_points) *
                weights$weight[weights$variables_household_conditions == 'n_house_divisions']) +
             (ifelse(is.na(wall_material_points),
                     mean(wall_material_points, na.rm = TRUE),
                     wall_material_points) *
                weights$weight[weights$variables_household_conditions == 'wall_material']) +
             (ifelse(is.na(water_source_points),
                     mean(water_source_points, na.rm = TRUE),
                     water_source_points) *
                weights$weight[weights$variables_household_conditions == 'water_source']))
  
  # Make a human capital score (education + job)
  human_capital <-
    census %>%
    group_by(agregado) %>%
    summarise(education_points = max(as.numeric(education_points),
                                     na.rm = TRUE),
              job_points = max(as.numeric(job_points),
                               na.rm = TRUE)) %>%
    ungroup %>%
    mutate(education_points = ifelse(is.infinite(education_points),
                                     NA,
                                     education_points),
           job_points = ifelse(is.infinite(job_points),
                               NA,
                               job_points)) %>%
    mutate(job_points = ifelse(is.na(job_points),
                               mean(job_points, na.rm = TRUE),
                               job_points),
           education_points = ifelse(is.na(education_points),
                                     mean(education_points, na.rm = TRUE),
                                     education_points)) %>%
    rename(ses_occupation_household = job_points,
           ses_education_household = education_points)
  
  # Join to census
  census <- left_join(x = census,
                      y = human_capital,
                      by = 'agregado')
  
  # remove duplicates
  ab <- ab %>%
    mutate(dummy = 1) %>%
    arrange(date, name, district) %>%
    group_by(date, name, district) %>%
    mutate(cs_dummy = cumsum(dummy)) %>%
    ungroup %>%
    filter(cs_dummy == 1)
  ab <- ab %>% dplyr::select(-dummy, -cs_dummy)
  
  # Clean up name in census
  census <-
    census %>%
    rename(census_name = name)
  
  # Remove duplicates in students
  students <-
    students %>%
    filter(!duplicated(name))
  
  # Create a "turma" variable in performance
  performance$turma <-
    paste0(performance$number,
           '-',
           performance$letter)
  
  save(ab,
       census,
       geo,
       performance,
       students,
       file = 'data/prepared_data.RData') 
  
  # Write csv
  write_csv(ab, 'outputs/ab.csv')
  write_csv(census, 'outputs/census.csv')
  write_csv(geo, 'outputs/geo.csv')
  write_csv(performance, 'outputs/performance.csv')
  write_csv(students, 'outputs/students.csv')
}

# Define function for date truncation
date_truncate <- function(date_object, level = c("month", "quarter", "year")){
    if (is.null(level)) {
      stop("You must provide a level argument of either \"month\", \"quarter\" or \"year\".")
    }
    date_object <- as.Date(date_object)
    if (sum(!is.na(date_object)) == 0) {
      return(date_object)
    }
    if (level == "month") {
      return_object <- date_object
      return_object[!is.na(return_object)] <- as.Date(paste0(format(return_object[!is.na(return_object)], 
                                                                    "%Y-%m"), "-01"))
      return(return_object)
    }
    if (level == "quarter") {
      q_month <- (((((as.numeric(format(date_object, "%m"))) - 
                       1)%/%3) + 1) * 3) - 2
      return_object <- date_object
      return_object[!is.na(return_object)] <- as.Date(paste0(format(return_object[!is.na(return_object)], 
                                                                    "%Y"), ifelse(nchar(q_month[!is.na(return_object)]) == 
                                                                                    2, "-", "-0"), q_month, "-01"))
      return(return_object)
    }
    if (level == "year") {
      return_object <- date_object
      return_object[!is.na(return_object)] <- as.Date(paste0(format(return_object[!is.na(return_object)], 
                                                                    "%Y"), "-01-01"))
      return(return_object)
    }
  }
 