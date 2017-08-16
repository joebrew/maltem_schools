library(cism)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tidyverse)
library(readxl)
library(sp)
library(googlesheets)


# For most recent absenteeism and performance data: 
# Go to https://172.16.236.245:4443/OpenClinica/
# user:jbrew
# pass: normal


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

month_matcher <- data_frame(month_number = c(1:12),
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

if('prepared_data.RData' %in% dir('data')){
  load('data/prepared_data.RData')
} else {
  if(!'census_done.RData' %in% dir('data')){
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
    
    save(census,
         census_all,
         census_sp, 
         file = 'data/census_done.RData')
  } else {
    load('data/census_done.RData')
  }
  
  #######
  # PERFORMANCE
  #######
  performance <- read_csv('data/performance_7_june_extra_rows_removed.csv')
  
  # Manual corrections as indicated by Laia
  performance$`Study Subject ID` <-
    gsub('3 DE FEVEREIRO_2015_2_E',
         '3 DE FEVEREIRO_2016_2_E',
         performance$`Study Subject ID`)
  performance <-
    performance %>%
    filter(`Study Subject ID` != '3 DE FEVEREIRO_2015_4_C_3') %>%
    mutate(year_E2_C4 = ifelse(`Study Subject ID` %in% c("3 DE FEVEREIRO_2016_1_F_",
                                                         "3 DE FEVEREIRO_2016_2_D_",
                                                         "3 DE FEVEREIRO_2016_2_E_",
                                                         "3 DE FEVEREIRO_2016_3_A_",
                                                         "EP GRACA MACHEL_2016_5_B_",
                                                         "EP MARAGRA_2016_1_E_",
                                                         "GRACA MACHEL_2016_3_C_",
                                                         "ILHA JOSINA_2016_2_C_",
                                                         "MAGUIGUANA_2016_1_C_",
                                                         "MAGUIGUANA_2016_5_C_",
                                                         "MARAGRA_2016_1_D_"),
                               2016,
                               year_E2_C4)) %>% 
    mutate(year_E2_C4 = ifelse(`Study Subject ID` %in% c("3 DE FEVEREIRO_2015_3_B_",
                                                         "GRACA  MACHEL_2015_1_A_",
                                                         "ILHA JOSINA_2015_1_D",
                                                         "MARAGRA_2015_3_A_",
                                                         "MARAGRA_2015_5_F_"),
                               2015,
                               year_E2_C4))

  # Drop some observations that don't exist
  performance <-
    performance %>%
    filter(!`Study Subject ID` %in% c("3 DE FEVEREIRO_2015_3_B_12", 
                                     "3 DE FEVEREIRO_2015_4_A_14", 
                                     "ILHA JOSINA_2015_1_D_", 
                                     "ILHA JOSINA_2016_1_B_3", 
                                     "ILHA JOSINA_2016_2_C_3", 
                                     "ILHA JOSINA_2016_2_C_23", 
                                     "3 DE FEVEREIRO_2015_3_B_25", 
                                     "3 DE FEVEREIRO_2015_4_A_33"))
  
  # For those that have indeed the same name and belong to different turmas, 
  # we change the name of one of them. we first create a variable"
  performance$nome_E2_C4[performance$`Study Subject ID` == 'GRACA MACHEL_2015_2_C_1'] <- 'ADERITO S COSSA 2 (duplicate name, but truly different person)' 
  performance$nome_E2_C4[performance$`Study Subject ID` == 'MAGUIGUANA_2016_3_B_'] <- 'ANTONIO FERNANDO COSSA 2 - sounds the same, but different' 
  performance$nome_E2_C4[performance$`Study Subject ID` == 'MARAGRA_2015_2_C_11'] <- 'CARLOS ALBERTO MATUSSE (the other one, similar name)' 

  
  # Clean up
  performance$school <- performance$school_E2_C4
  performance$school <- performance$`Study Subject ID`
  # unlist(lapply(strsplit(performance$`Study Subject ID`, '_'),
  #               function(x){
  #                 x[1]
  #               }))
  
  # Standardize school names
  performance <-
    performance %>%
    mutate(school = ifelse(grepl('GRACA|GRAÇA|GRCA', school), 'GRACA MACHEL',
                           ifelse(grepl('JOSI|ILHA|J MACHEL| J. MACHEL', school), 'ILHA JOSINA',
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
    mutate(year = year_E2_C4,
           year_from_id = get_info(x = `Study Subject ID`,
                           info = 'year'),
           number = classe_E2_C4, 
           number_from_id = get_info(x = `Study Subject ID`,
                             info = 'grade'),
           letter = turma_E2_C4, 
           letter_from_id = get_info(x = `Study Subject ID`,
                             info = 'turma'))#,
           # roster_number = get_info(x = `Study Subject ID`,
           #                          info = 'number'))
  
  # More manual cleaning as indicated by Laia
  performance <- performance %>%
    mutate(school = ifelse(school == 'MARAGRA' & 
                             year == 2016 &
                             number == 1 &
                             letter == 'B',
                           '3 DE FEV',
                           school),
           letter = ifelse(school == 'DUCO' &
                             year == 2015 &
                             number == 2,
                           'U',
                           letter),
           letter = ifelse(school == 'GRACA MACHEL' &
                             year == 2016 &
                             number == 5 &
                             letter == 'D',
                           'C',
                           letter),
           letter = ifelse(school == 'ILHA JOSINA' &
                             year == 2016 &
                             number == 5 &
                             letter == 'D',
                           'B',
                           letter),
           number = ifelse(school == 'MARAGRA' &
                             year == 2016 &
                             number == 3 &
                             letter == 'F',
                           2,
                           number),
           letter = ifelse(school == 'MOINE' &
                             year == 2016 &
                             number == 3 &
                             letter == 'B',
                           'U',
                           letter),
           letter = ifelse(school == 'SIMBE' &
                             year == 2015 &
                             number == 5 &
                             letter == 'A',
                           'U',
                           letter),
           letter = ifelse(school == 'MOINE' &
                             year == 2015 &
                             number == 5 &
                             letter == 'A',
                           'U',
                           letter))

  performance <-
    performance %>%
    mutate(flag_year = year != year_from_id,
           flag_number = number != number_from_id,
           flag_letter = letter != letter_from_id) %>%
    mutate(flag = flag_year | flag_number | flag_letter) %>%
    filter(!flag)
  
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
                                                 'Magude',
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
    rename(serial_number = serialno_E2_C4) %>%
    dplyr::select(`Study Subject ID`,
                  serial_number,
                  name, 
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
  performance$letter <- toupper(performance$letter)
  performance <-
    performance %>%
    mutate(letter = ifelse(letter == 'UNICA', 'U',
                           ifelse(grepl('B', letter), 'B',
                                  letter))) %>%
    mutate(letter = ifelse(letter == 'U', 'A', letter))
  
  performance <- 
    performance %>%
    mutate(number = as.numeric(number)) %>%
    filter(!is.na(number)) %>%
    mutate(number = ifelse(number > 5, NA, number)) %>%
    filter(!is.na(number),
           !is.na(letter))
  
  
  ########
  # ABSENTEEISM
  ########
  
  ab <- read_csv('data/absenteeism_7_june_extra_rows_removed.csv')
  
  # Clean up EPC
  ab$`Study Subject ID` <-
    gsub('EPC_|EP_',
         'EPC ',
         ab$`Study Subject ID`)
  
  # Clean up EPC
  ab$`Study Subject ID` <-
    gsub('JOSINA_MACHEL',
         'JOSINA MACHEL',
         ab$`Study Subject ID`)
  
  # Manual corrections as indicated by Laia
  ab$`Study Subject ID` <-
    gsub('3 DE FEVEREIRO_2015_2_E',
         '3 DE FEVEREIRO_2016_2_E',
         ab$`Study Subject ID`)
  
  # Remove those with no name
  ab <- ab[,!is.na(names(ab))]
  
  # Clean up
  ab$school <- 
    unlist(lapply(strsplit(ab$`Study Subject ID`, '_'),
                  function(x){
                    x[1]
                  }))
  
  # ab$year <-
  #   as.numeric(unlist(lapply(strsplit(ab$`Study Subject ID`, '_'),
  #                            function(x){
  #                              x[2]
  #                            })))
  # ab$year <-
  #   ifelse(grepl('2015_', ab$`Study Subject ID`), 2015,
  #          ifelse(grepl('2016_', ab$`Study Subject ID`), 2016,
  #                 NA))
  ab$year <- ab$year_E1_C1
  
  ab$number <- 
    as.numeric(unlist(lapply(strsplit(ab$`Study Subject ID`, '_'),
                             function(x){
                               x[3]
                             })))
  
  ab$letter <- ab$turma_E1_C1
    # unlist(lapply(strsplit(ab$`Study Subject ID`, '_'),
    #               function(x){
    #                 x[4]
    #               }))
  ab$letter <- toupper(ab$letter)
  ab$letter <- ifelse(ab$letter == 'UNICA', 'U',
                      ifelse(nchar(ab$letter) > 1, NA, 
                             ab$letter)) 
  ab <- ab %>%
    mutate(letter = ifelse(letter == 'U', 'A', letter))

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
  month_matcher <- data_frame(month_number = c(1:12),
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
  # rm(month_matcher)
  
  # Get day number
  ab$day_number <- 
    unlist(lapply(strsplit(ab$key, '_'), function(x){
      z <- x[1]
      if(grepl('maio', z)){
        as.numeric(substr(z, 8, nchar(z)))
      } else {
        as.numeric(substr(z, 7, nchar(z)))
      }
      
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
  
  # Remove those with non-sensical dates
  # such as feb 29 2015
  ab <- ab %>%
    filter(!is.na(date))
  
  # # Remove those which took place on weekends
  # ab <- ab %>%
  #   filter(! dow %in% c('Saturday', 'Sunday'))
  
  ab <-
    ab %>%
    mutate(school = ifelse(grepl('GRACA|GRAÇA', school), 'GRACA MACHEL',
                           ifelse(grepl('JOSI|ILHA|J MAC', school), 'ILHA JOSINA',
                                  ifelse(grepl('FAV|FEV|REIR', school), '3 DE FEV',
                                         ifelse(grepl('XINA|SIV', school), 'XINAVANE',
                                                ifelse(grepl('MAGUI|MAGGU', school), 'MAGUIGUANA',
                                                       ifelse(grepl('MARAG', school), 'MARAGRA',
                                                              ifelse(grepl('SIMB', school), 'SIMBE',
                                                                     ifelse(grepl('MOIN', school), 'MOINE', 
                                                                            ifelse(grepl('MAGU', school), 'GRACA MACHEL', 
                                                                                   ifelse(grepl('DUCO|DUCA', school), 'DUCO', 
                                                                                          ifelse(grepl('MAGUDE', school), 'GRACA MACHEL', NA))))))))))))
  
  # Define geography
  # # cat(paste0('"', sort(unique(ab$school)), '"', collapse = ',\n'))
  geography <- data_frame(school = toupper(c("3 de Fev",
                                     "Duco",
                                     "Graca Machel",
                                     "Maguiguana",
                                     "Maragra",
                                     "Moine",
                                     "Simbe",
                                     "Xinavane",
                                     "Ilha Josina")),
                          district = c('Manhiça',
                                       'Magude',
                                       'Magude',
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
  
  # Remove those with no school/district
  ab <- ab %>%
    filter(!is.na(school),
           !is.na(district))
  
  # Cut down to only necessary variables
  ab <- ab %>%
    rename(serial_number = serialno_E1_C1) %>%
    dplyr::select(`Study Subject ID`,
                  serial_number,
                  name,
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
  
  # # Clean up oddities # For now, leaving these
  # ab <-
  #   ab %>%
  #   mutate(letter = ifelse(letter %in% c('1', '6', '7', '8', '9', 'T'), NA,
  #                          ifelse(letter == 'UNICA', 'U',
  #                                 letter))) %>%
  #   mutate(letter = ifelse(letter == 'U', 'A', letter))
  # ab <- ab %>%
  #   mutate(number = ifelse(number > 5, NA, number)) %>%
  #   filter(!is.na(number),
  #          !is.na(letter))
  
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
  
  save.image('/home/joebrew/Desktop/temp.RData')
  
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
  
  # Remove periods
  df_names$name <- gsub('.', '', df_names$name, fixed = TRUE)
  
  # Remove double spaces
  df_names$name <- gsub('  |   ', '', df_names$name)
  
  # Remove trailing and leading white space
  df_names$name <- trimws(df_names$name)
  
  # Get the number of words
  df_names$n_words <- unlist(lapply(strsplit(df_names$name, split = ' '), function(x){ length(x)}))
  
  # # Write the names to a google sheet 
  # # Identify the sheet
  # this_sheet <- gs_ls("school names matching - do not edit by hand")
  # 
  # # Register the sheet
  # this_sheet_registered <- gs_key(x = this_sheet$sheet_key)
  # 
  # gs_edit_cells(ss = this_sheet_registered,
  #               ws = 1,
  #               input = df_names,
  #               anchor = 'A1',
  #               col_names = TRUE,
  #               trim = TRUE)
  # 
  # 
  # # Write a csv for use with open refine
  # write_csv(df_names %>% dplyr::select(name, school), 
  #           'open_refine_names.csv')
  # 
  # # Upload data to open refine
  # refine_upload(file = "open_refine_names.csv", 
  #               project.name = "school names cleaning", 
  #               open.browser = TRUE)
  # 
  # # Read in refine names
  # refined <- read_csv('~/Desktop/refined.csv')
  # names(refined)[1] <- 'new_name'
  # refined <- refined %>%
  #   dplyr::select(new_name)
  # 
  # # Join with old names
  # df_names <-
  #   cbind(df_names, refined)
  # 
  # df_names <- df_names %>%
  #   dplyr::select(school, name, new_name)
  # 
  # # Get numbers
  # df_names <- 
  #   df_names %>%
  #   mutate(number = as.numeric(factor(new_name)))
  # 
  # # Write a csv for further manual cleaning
  # write_csv(df_names, '~/Desktop/midway.csv')
  # 
  # # After refining, read back in data
  # df_fuzzy <- refine_export(project.name = "school names cleaning")
  # 
  
  # Read in the refined (manually and algorithmically) names
  library(gsheet)
  df_fuzzy <- gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1CgF122GCSkkIJWjwEyXtRZTm4p_PyXZXf_LT1Bq3k4Y/edit?usp=sharing')

  # if('fuzzy_names.RData' %in% dir()){
  #   load('fuzzy_names.RData')
  # } else {
  #   df_fuzzy <- cism::fuzzy_match(x = df_names$name)
  #   save(df_fuzzy, file = 'fuzzy_names.RData')
  # }
  # 
  # # Loop through each name, changing it if similar enough to another
  # df_names$new_name <- df_names$name
  # df_names$already_changed <- FALSE
  # df_names$master <- FALSE
  # df_names$copies <- 0
  # df_names$index <- 1:nrow(df_names)
  # threshold <- 0.1
  # n_names <- nrow(df_names)
  # for (i in 1:length(df_names$name)){
  #   message(paste0(i, ' of ', n_names))
  #   # Only change names for those which haven't been modified yet
  #   if(!df_names$already_changed[i]){
  #     this_name <- df_names$name[i]
  #     this_school <- df_names$school[i]
  #     # Scores
  #     scores <- df_fuzzy[i,]
  #     scores_df <- df_names %>%
  #       mutate(score = scores)
  #     
  #     # Remove from scores_df the identical rows
  #     scores_df <-
  #       scores_df %>%
  #       filter(score > 0)
  #     
  #     # Identify if there is anything below the threshold
  #     scores_df <- scores_df %>% filter(score < threshold)
  #     
  #     # Keep only those matches with the same school
  #     scores_df <- 
  #       scores_df %>%
  #       filter(school == this_school)
  #     
  #     # If there are names below the threshold, change them all
  #     if(nrow(scores_df) > 0){
  #       # Make the change to the name
  #       df_names$new_name[df_names$index %in% scores_df$index] <-
  #         this_name
  #       # DON'T revisit this name (ie, it's already been changed)
  #       df_names$already_changed[df_names$index %in% scores_df$index] <- TRUE
  #       # Flag this as being the "master" name
  #       df_names$master[i] <- TRUE
  #       # Designate how many copies it took
  #       df_names$copies[i] <- nrow(scores_df)
  #     } 
  #   }
  # }
  # 
  # # Use df_names to create id numbers
  # id_numbers <-
  #   df_names %>%
  #   group_by(name,
  #            new_name,
  #            school) %>%
  #   tally %>%
  #   ungroup %>%
  #   dplyr::select(-n) %>%
  #   mutate(name_match_type = ifelse(name == new_name, 'perfect',
  #                                   'fuzzy')) %>%
  #   mutate(perfect_dummy = ifelse(name_match_type == 'perfect', 1, 0),
  #          fuzzy_dummy = ifelse(name_match_type == 'fuzzy', 1, 0)) %>%
  #   mutate(perfect_dummy = cumsum(perfect_dummy),
  #          fuzzy_dummy =cumsum(fuzzy_dummy)) %>%
  #   mutate(fuzzy_dummy = fuzzy_dummy + 100000) %>%
  #   mutate(id = ifelse(name_match_type == 'perfect', perfect_dummy,
  #                      fuzzy_dummy)) %>%
  #   rename(original_name = name) %>%
  #   dplyr::select(original_name,
  #                 school,
  #                 id)
  # 
  
  df_names <- df_fuzzy
  
  # Fix the names in performance and ab to be standardized
  ab <- 
    ab %>%
    left_join(df_names %>%
                dplyr::select(name, 
                              new_name),
              by = 'name') %>%
    mutate(original_name = name) %>%
    mutate(name = new_name) %>%
    dplyr::select(-new_name)
  
  performance <- 
    performance %>%
    left_join(df_names %>%
                dplyr::select(name, 
                              new_name),
              by = 'name') %>%
    mutate(original_name = name) %>%
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
  
  # # Remov unecessary objects
  # rm(best_index,
  #    best_name,
  #    census_names,
  #    i,
  #    n_names,
  #    scores,
  #    this_name,
  #    this_school,
  #    threshold,
  #    df_fuzzy,
  #    df_names,
  #    fuzzy_census,
  #    scores_df,
  #    j,
  #    new_name,
  #    this_column)
  
  # Rename name in census
  census <-
    census %>%
    rename(census_name = name)
  
  # Create a dataset of unique student name / ids
  students <- 
    data_frame(name = sort(unique(c(performance$name,
                                    ab$name))))

  # Create an id number
  
  # No longer using id
  # students$id <- 1:nrow(students)

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
    try({
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
    })
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
    try({
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
    })
  }
  
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
    arrange(date, original_name, district) %>%
    group_by(date, original_name, district) %>%
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
  
  # MANUAL CHANGES  
  ab <- ab %>% filter(serial_number != 100872)
  ab <- ab %>%
    filter(serial_number != 478557) %>%
    mutate(letter = ifelse(grepl('3 DE FEVEREIRO_2015_5_E', `Study Subject ID`), 'A', letter),
           turma = ifelse(grepl('3 DE FEVEREIRO_2015_5_E', `Study Subject ID`), '5-A', turma)) %>%
    filter(!grepl('DUCA_2016_4_A', `Study Subject ID`))
#   

  ab <- ab %>%
    mutate(flag = 
             ifelse(school=="GRACA MACHEL"&year==2015&turma=="1-C" |
                      school=="GRACA MACHEL"&year==2015&turma=="2-A" |
                      school=="GRACA MACHEL"&year==2015&turma=="3-A"&month_name=="aug" |
                      school=="3 DE FEV"&year==2016&turma=="1-A"&month_name=="set"|month_name=="out"|
                      school=="3 DE FEV"&year==2015&turma=="3-A"|
                      school=="3 DE FEV"&year==2015&turma=="3-B"&month_name=="aug"|
                      school=="MARAGRA"&year==2015&turma=="1-D",
                    TRUE, FALSE)) %>%
    filter(!flag) %>%
    dplyr::select(-flag)

#   **** correcting some years (2015 and 2016)
#   replace year=2016 if regexm(studysubjectid, "DUCO_2016_4_A_")
  ab <- ab %>%
    mutate(year = ifelse(grepl('DUCO_2016_4_A_', `Study Subject ID`),
                         2016, year))
  
  ab <- ab %>%
    filter(!grepl("3 DE FEVEREIRO_2015_5_A_", `Study Subject ID`))

  # Apply the manual fixes / corrections emailed by Laia
  ab <-
    ab %>%
    filter(!(school == '3 DE FEV' &
               number == 3 &
               letter == 'A' &
               year == 2016))
  performance <-
    performance %>%
    mutate(letter = ifelse(school == 'DUCO' &
                             year == 2016 &
                             letter == 'A',
                           'U', 
                           letter))
  ab <-
    ab %>%
    mutate(letter = ifelse(school == 'DUCO' &
                             year == 2016 &
                             letter == 'A',
                           'U', 
                           letter))
  ab <-
    ab %>%
    mutate(school = ifelse(school == 'MAGUDE',
                           'GRACA MACHEL',
                           school))
  ab <-
    ab %>%
    filter(!(school == 'MAGUIGUANA' &
               number == 1 &
               letter == 'A' &
               year == 2016))
  ab <-
    ab %>%
    mutate(year = ifelse(year == 2015 &
                           school == '3 DE FEV' &
                           number == 2 &
                           letter == 'E',
                         2016,
                         year)) %>%
    filter(!(year == 2016 &
               number == 3 &
               letter == 'A' &
               school == '3 DE FEV')) #%>%
    # mutate(letter = ifelse(letter == 'B' &
    #                          number == 3 &
    #                          year == 2015 &
    #                          school == 'MAGUIGUANA',
    #                        'A',
    #                        letter))
  performance <-
    performance %>%
    mutate(school = ifelse(school == 'MAGUDE',
                           'GRACA MACHEL',
                           school))
  
  # Still need to run ------------------------
  performance$letter <- ifelse(performance$letter == 'U', 'A',
                               performance$letter)
  ab$letter <- ifelse(ab$letter == 'U', 'A',
                               ab$letter)
  
  # Create a "turma" variable in performance
  performance$turma <-
    paste0(performance$number,
           '-',
           performance$letter)
  
  # Create a "turma" variable in absenteeism
  ab$turma <-
    paste0(ab$number,
           '-',
           ab$letter)
  
  # Remove those with no id
  ab <- ab %>% filter(!is.na(id), !is.na(name))
  performance <- performance %>% filter(!is.na(id), !is.na(name))
  students <- students %>% filter(!is.na(id), !is.na(name))
  
  # Remove all duplicates
  ab_dups <-
    ab %>%
    group_by(name, district, year, term, date,
             number, letter, turma) %>%
    tally %>%
    ungroup %>%
    mutate(flag = n > 1) 
  ab <- ab %>%
    left_join(ab_dups) %>%
    filter(!flag) %>%
    dplyr::select(-flag)
  performance_dups <- 
    performance %>%
    group_by(name, district, school, year, trimester, number,
             letter, subject) %>%
    tally %>%
    ungroup %>%
    mutate(flag = n > 1) 
  performance <-
    performance %>%
    left_join(performance_dups) %>%
    filter(!flag) %>%
    dplyr::select(-flag)

  
  # Create variable defining the level of matching
  ab$match_type <-
    ifelse(paste0(ab$original_name, ab$school) %in% 
             paste0(performance$original_name,
                    performance$school),
           'perfect',
           ifelse(paste0(ab$name, ab$school) %in%
                    paste0(performance$name, performance$school),
                  'fuzzy',
                  'none'))
  performance$match_type <-
    ifelse(paste0(performance$original_name,
                  performance$school) %in% 
             paste0(ab$original_name,
                    ab$school),
           'perfect',
           ifelse(paste0(performance$name, performance$school) %in%
                    paste0(ab$name, ab$school),
                  'fuzzy',
                  'none'))
  
  # Remove old id from everywhere
  ab$id <- NULL
  performance$id <- NULL
  students$id <- NULL

  # Get match types into students
  match_types <-
    ab %>%
    group_by(name, school, match_type) %>%
    tally %>%
    ungroup %>%
    bind_rows(
      performance %>%
        group_by(name, school, match_type) %>%
        tally %>%
        ungroup
    ) %>%
    group_by(name,
             school,
             match_type) %>%
    tally %>%
    ungroup %>%
    dplyr::select(name,
                  school,
                  match_type) %>%
    arrange(name) %>%
    mutate(dummy = 1) %>%
    mutate(is_none = match_type == 'none') %>%
    group_by(is_none) %>%
    mutate(id = cumsum(dummy)) %>%
    ungroup %>%
    dplyr::select(-dummy, -is_none) %>%
    mutate(id = ifelse(match_type == 'none',
                       id + 100000,
                       id))
  
  # Bring ids and match types into every where
  ab <-
    ab %>%
    left_join(match_types)
  performance <-
    performance %>%
    left_join(match_types)
  students <- 
    students %>%
    left_join(match_types)
  
  # Remove duplicates
  students <- 
    students %>%
    filter(!duplicated(paste0(name, school)))
  ab <- ab %>%
    filter(!duplicated(paste0(name, school, date)))
  performance <-
    performance %>%
    filter(!duplicated(paste0(name, school,
                              year,
                              trimester,
                              subject)))

  # Final cleaning to absenteeism -----------------------------
  ab$day <- as.numeric(format(ab$date, '%d'))

  ab$date <- as.Date(ab$date, origin = '1970-01-01')
  ab <- ab %>%
    # Regenerate all the date-related variables
    mutate(year = as.numeric(format(date, '%Y'))) %>%
    mutate(year_term = paste0(year, '-', term)) %>%
    mutate(month = date_truncate(date, 'month')) %>%
    dplyr::select(-month_number) %>%
    mutate(month_number = as.numeric(format(date, '%m'))) %>%
    left_join(month_matcher) %>%
    mutate(dow = weekdays(date)) %>%
    mutate(day = as.numeric(format(date, '%d')))

  
  ab <- ab %>%
    mutate(year = ifelse(grepl('DUCO_2016_4_A_', `Study Subject ID`),
                         2016, year))
  
  # IMPLEMENT MANUAL CORRECTIONS SENT BY LAIA AUGUST 2 2017
  # * School absenteeism - data cleaning commands, 2nd august 2017
  # 
  # * You might have already implemented these commands. But please, double check (important to follow this order) 
  ab <- 
    ab %>%
    filter(id != 100872) %>%
    filter(!(school=="GRACA MACHEL"&year==2015&turma=="1-C"),
           !(school=="GRACA MACHEL"&year==2015&turma=="2-A"),
           !(school=="MARAGRA"&year==2015&turma=="1-D"),
           !(grepl("DUCA_2016_4_A_", `Study Subject ID`)),
           !(grepl("3 DE FEVEREIRO_2015_5_A_", `Study Subject ID`)))
  
  ab <- ab %>%
    mutate(year = ifelse(grepl("DUCO_2016_4_A_", `Study Subject ID`), 
                         2016, 
                         year))
  # If the year changes, does everything else?
  
  # Define a function for reverse grepl (object first, string second)
  rev_grepl <- function(a, b){
    grepl(b, a)
  }

  ab <- ab %>%
    mutate(turma = ifelse(rev_grepl(`Study Subject ID`, "3 DE FEVEREIRO_2015_5_E_"),
                          '5-A',
                          turma)) %>%
    mutate(year = ifelse(school=="3 DE FEV"&year==2016&turma=="2-E", 2015, year))
 
  ab <- ab %>%
    filter(!(school=="MAGUIGUANA"&year==2016&turma=="1-C"),
           !(school=="GRACA MACHEL"&year==2015&turma=="3-A"&month_name=="ago"),
           !(school=="3 DE FEV"&year==2015&turma=="3-B"&month_name=="ago"))

  # *3 DE FEV
  ab <- ab %>%
    filter(!(school=="3 DE FEV"&year==2015&month_name=="mar"&turma=="2-D"&day==28),
           !(school=="3 DE FEV"&year==2015&month_name=="mar"&turma=="2-D"&day==29)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2015&month_name=="mai"&turma=="2-D"&day>15&day<28,
                        day + 2,
                        day)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2015&month_name=="mar"&turma=="2-E"&day==1,
                        day + 1,
                        day))
  ab <- ab %>%
    filter(!(school=="3 DE FEV"&year==2015&(month_name=="abr"|month_name=="ago"|month_name=="oct"|month_name=="nov")&turma=="3-B"&day==29),
           !(school=="3 DE FEV"&year==2015&month_name=="mai"&turma=="3-B"&day>0&day<17))
  ab <- ab %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2015&month_name=="mar"&turma=="4-B"&day>4&day<31, day - 3,
                        day))
    
  ab <- ab %>%
    filter(!(school=="3 DE FEV"&year==2015&month_name=="abr"&turma=="4-B"&day==2),
           !(school=="3 DE FEV"&year==2015&month_name=="abr"&turma=="4-B"&day==3))

  ab <- ab %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2015&month_name=="abr"&turma=="4-B"&day>3&day<31,
                        day - 3,
                        day))
  ab <- ab %>%
    filter(!(school=="3 DE FEV"&year==2015&month_name=="mai"&turma=="4-B"&day==2),
           !(school=="3 DE FEV"&year==2015&month_name=="mai"&turma=="4-B"&day==3)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2015&month_name=="mai"&turma=="4-B"&day>3&day<32, 
                        day-3,
                        day),
           day = ifelse(school=="3 DE FEV"&year==2015&month_name=="jun"&turma=="4-B"&day>3&day<30,
                        day - 3,
                        day)) %>%
    filter(!(school=="3 DE FEV"&year==2015&month_name=="jul"&turma=="4-B"&day==2),
           !(school=="3 DE FEV"&year==2015&month_name=="jul"&turma=="4-B"&day==3)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2015&month_name=="jul"&turma=="4-B"&day>3&day<32,
                        day - 3,
                        day)) %>%
    filter(!(school=="3 DE FEV"&year==2015&month_name=="ago"&turma=="4-B"&day==1),
           !(school=="3 DE FEV"&year==2015&month_name=="ago"&turma=="4-B"&day==2)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2015&month_name=="ago"&turma=="4-B"&day>2&day<32, 
                        day - 3,
                        day)) %>%
    filter(!(school=="3 DE FEV"&year==2015&month_name=="set"&turma=="4-B"&day==3)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2015&month_name=="set"&turma=="4-B"&day>2&day<7,
                         day - 2,
                         day )) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2015&month_name=="set"&turma=="4-B"&day>9&day<29,
                         day - 3,
                         day)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2015&month_name=="mai"&turma=="4-C"&day>1&day<6,
                        day + 2,
                        day)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2015&month_name=="ago"&turma=="4-E"&day>0&day<4,
                        day + 2,
                        day)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2016&month_name=="abr"&turma=="2-A"&day>19&day<31,
                         day - 2,
                         day)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2016&month_name=="mai"&turma=="2-A"&day>3&day<30,
                         day - 2,
                         day)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2016&month_name=="jun"&turma=="2-A"&day>7&day<13,
                        day - 2,
                        day)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2016&month_name=="jul"&turma=="2-A"&day>5&day<31,
                  day - 2,
                  day)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2016&month_name=="ago"&turma=="2-A"&day>2&day<15,
                        day - 2,
                        day)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2016&month_name=="mar"&turma=="3-C"&day>26&day<29,
                        day + 1,
                        day)) %>%
    mutate(day = ifelse(school=="3 DE FEV"&year==2016&month_name=="mar"&turma=="5-B"&day>15&day<21,
                        day - 2,
                        day))

  # *DUCO
  ab <- ab %>%
    mutate(day = ifelse(school=="DUCO"&year==2015&month_name=="abr"&turma=="2-A",
                        day + 1, 
                        day)) %>%
    filter(!(school=="DUCO"&year==2015&month_name=="set"&turma=="2-A"&day==13)) %>%
    mutate(day = ifelse(school=="DUCO"&year==2015&month_name=="mai"&turma=="3-A"&day>5&day<10,
                        day - 1,
                        day)) %>%
    mutate(day = ifelse(school=="DUCO"&year==2015&month_name=="mai"&turma=="3-A"&day>16&day<19,
                        day + 1,
                        day)) %>%
    mutate(day = ifelse(school=="DUCO"&year==2015&month_name=="jun"&turma=="3-A"&day>26&day<29,
                        day - 2,
                        day)) %>%
    mutate(day = ifelse(school=="DUCO"&year==2016&month_name=="abr"&turma=="5-A"&day>2&day<7,
                        day + 1,
                        day))

  # *GRACA MACHEL
  ab <- 
    ab %>%
    mutate(day = ifelse(school=="GRACA MACHEL"&year==2015&month_name=="jun"&turma=="3-A"&day>20&day<25,
                        day + 1,
                        day)) %>%
    mutate(day = 
             ifelse(school=="GRACA MACHEL"&year==2015&month_name=="abr"&turma=="3-C"&day>4&day<7,
                    day + 1,
                    day)) %>%
    mutate(day = ifelse(school=="GRACA MACHEL"&year==2016&month_name=="abr"&turma=="1-A"&day>23&day<29, 
                        day + 1,
                        day)) %>%
    mutate(day = ifelse(school=="GRACA MACHEL"&year==2016&month_name=="jun"&turma=="1-B"&day>17&day<22,
                        day + 2,
                        day)) %>%
    filter(!(school=="GRACA MACHEL"&year==2016&month_name=="abr"&turma=="2-C"&day==24)) %>%
    mutate(day = ifelse(school=="GRACA MACHEL"&year==2016&month_name=="ago"&turma=="5-B"&day>27&day<32,
                        day - 5,
                        day))

  # *ILHA JOSINA
  ab <- 
    ab %>%
    mutate(day = ifelse(school=="ILHA JOSINA"&year==2015&month_name=="abr"&turma=="1-A"&day>4&day<7,
                        day + 1,
                        day)) %>%
    filter(!(school=="ILHA JOSINA"&year==2015&month_name=="jul"&turma=="1-A"&(day==2|day==3))) %>%
    mutate(day = ifelse(school=="ILHA JOSINA"&year==2015&month_name=="jul"&turma=="1-A"&day>3&day<32,
                        day - 3,
                        day)) %>%
    filter(!(school=="ILHA JOSINA"&year==2015&month_name=="set"&turma=="2-A"&day==29)) %>%
    mutate(day = ifelse(school=="ILHA JOSINA"&year==2015&month_name=="set"&turma=="2-A"&day>25&day<29,
                        day + 2,
                        day)) %>%
    mutate(day = ifelse(school=="ILHA JOSINA"&year==2015&month_name=="set"&turma=="2-B"&day==26,
                        day - 1,
                        day)) %>%
    mutate(day = ifelse(school=="ILHA JOSINA"&year==2015&month_name=="set"&turma=="2-B"&day>26&day<30,
                        day + 1,
                        day)) %>%
    filter(!(school=="ILHA JOSINA"&year==2015&month_name=="jun"&turma=="4-C"&(day==30|day==31))) %>%
    mutate(day = ifelse(school=="ILHA JOSINA"&year==2015&month_name=="jun"&turma=="4-C"&day>0&day<30,
                        day + 2,
                        day)) %>%
    filter(!(school=="ILHA JOSINA"&year==2015&month_name=="jul"&turma=="4-C"&(day==11|day==12))) %>%
    filter(!(school=="ILHA JOSINA"&year==2015&month_name=="set"&turma=="5-A"&day==31)) %>%
    mutate(day = ifelse(school=="ILHA JOSINA"&year==2015&month_name=="set"&turma=="5-A"&day>12&day<31,
                        day + 1,
                        day)) %>%
    filter(!(school=="ILHA JOSINA"&year==2016&month_name=="mai"&turma=="1-C"&(day==1|day==7|day==8|day==14|day==15|day==21|day==22|day==28|day==29))) %>%
    mutate(day = ifelse(school=="ILHA JOSINA"&year==2016&month_name=="jul"&turma=="5-C"&day>23&day<29,
                        day + 1,
                        day)) %>%
    filter(!(`Study Subject ID`=="ILHA JOSINA_2016_3_B_2")) %>%
    filter(!(`Study Subject ID`=="ILHA JOSINA_2016_3_B_1"))

  # *MAGUIGUANA
  # * IMPORTANT: Joe, please, before runing the below commands, cancel the previous change that we 
  # * told you to do about changing Maguiguana 2015 3-B to 3-A. Do this instead:
  ab <- ab %>%
    mutate(turma = ifelse(serial_number == 479041,
                          '3-A',
                          turma),
           turma = ifelse(serial_number==479052,
                          '3-B',
                          turma)) %>%
    mutate(day = ifelse(school=="MAGUIGUANA"&year==2015&month_name=="mar"&turma=="1-C"&(day==8|day==9|day==10|day==11),
                        day + 1,
                        day)) %>%
    filter(!(school=="MAGUIGUANA"&year==2015&month_name=="mar"&turma=="1-C"&(day==14|day==15|day==21|day==22|day==28|day==29))) %>%
    mutate(day = ifelse(school=="MAGUIGUANA"&year==2015&month_name=="mai"&turma=="2-B"&day>23&day<26,
                        day + 1,
                        day)) %>%
    mutate(day = ifelse(school=="MAGUIGUANA"&year==2015&month_name=="set"&turma=="3-B"&(day==16|day==17|day==18|day==19|day==20),
                        day - 2,
                        day)) %>%
    mutate(day = ifelse(school=="MAGUIGUANA"&year==2015&month_name=="set"&turma=="3-B"&(day==26|day==27),
                        day + 2,
                        day)) %>%
    mutate(day = ifelse(school=="MAGUIGUANA"&year==2015&month_name=="ago"&turma=="4-B"&day==16,
                        day + 1,
                        day)) %>%
    mutate(day = ifelse(school=="MAGUIGUANA"&year==2015&month_name=="abr"&turma=="5-B"&(day==19|day==20),
                        day + 1,
                        day)) %>%
    mutate(day = ifelse(school=="MAGUIGUANA"&year==2015&month_name=="mai"&turma=="5-B"&day==24,
                        day + 1,
                        day)) %>%
    filter(!(school=="MAGUIGUANA"&year==2015&month_name=="mai"&turma=="5-C"&(day==29|day==30))) %>%
    mutate(day = ifelse(school=="MAGUIGUANA"&year==2015&month_name=="mai"&turma=="5-C"&day>1&day<27,
                        day + 3,
                        day)) %>%
    filter(!(school=="MAGUIGUANA"&year==2016&month_name=="mar"&turma=="1-B"&day==27),
           !(school=="MAGUIGUANA"&year==2016&month_name=="mai"&turma=="1-B"&(day==28|day==29|day==31)),
           !(school=="MAGUIGUANA"&year==2016&month_name=="ago"&turma=="1-C")) %>%
    mutate(year = ifelse(`Study Subject ID`=="EP MAGUIGUANA_2015_3_B_9",
                         2015,
                         year)) %>%
    filter(!(school=="MAGUIGUANA"&year==2016&month_name=="mai"&turma=="2-D"&(day==22))) %>%
    mutate(day = ifelse(school=="MAGUIGUANA"&year==2016&month_name=="set"&turma=="3-B"&day>9&day<12,
                        day + 3,
                        day))

  # *MARAGRA
  ab <- ab %>%
    mutate(turma = ifelse(serial_number==478804,
                          '5-J',
                          turma)) %>%
    mutate(year = ifelse(serial_number==478804,
                         2015,
                         year)) %>%
    filter(!(`Study Subject ID`=="MARAGRA_2015_2_D_34"),
           !(serial_number==478210),
           !(serial_number==478203),
           !(`Study Subject ID`=="MARAGRA_2015_1_C_1")) %>%
    mutate(day = ifelse(school=="MARAGRA"&year==2015&month_name=="mar"&turma=="2-B"&day==1,
                        day + 1,
                        day)) %>%
    mutate(day = ifelse(school=="MARAGRA"&year==2015&month_name=="ago"&turma=="2-C"&(day==1|day==2),
                        day + 2,
                        day)) %>%
    filter(!(school=="MARAGRA"&year==2015&month_name=="mar"&turma=="2-E"&day==22),
           !(school=="MARAGRA"&year==2015&month_name=="jul"&turma=="2-F"&day==26),
           !(school=="MARAGRA"&year==2015&month_name=="nov"&turma=="2-F"&day==8)) %>%
    mutate(day = ifelse(school=="MARAGRA"&year==2015&month_name=="jul"&turma=="5-F"&day>4&day<9,
                        day + 1,
                        day)) %>%
    filter(!(school=="MARAGRA"&year==2016&month_name=="mar"&turma=="3-D"&day==6),
           !(school=="MARAGRA"&year==2016&month_name=="jun"&turma=="3-D"&day==26)) %>%
    mutate(day = ifelse(school=="MARAGRA"&year==2016&month_name=="mai"&turma=="4-A"&day==15,
                        day + 1,
                        day)) %>%
    filter(!(school=="MARAGRA"&year==2016&month_name=="abr"&turma=="5-A"&day==10),
           !(school=="MARAGRA"&year==2016&month_name=="jul"&turma=="5-C"&day==24)) %>%
    mutate(day = ifelse(school=="MARAGRA"&year==2016&month_name=="abr"&turma=="5-F"&day==24,
                        day + 1,
                        day)) %>%
    filter(!(school=="MARAGRA"&year==2016&month_name=="mai"&turma=="5-F"&day==29))

  ab <- ab %>%
    mutate(day= ifelse(school=="MOINE"&year==2015&month_name=="jun"&turma=="5-A"&(day==21|day==22),
                       day + 1,
                       day)) %>%
    filter(!(school=="MOINE"&year==2016&month_name=="mar"&turma=="4-A"&(day==26|day==27)))

  # *SIMBE
  ab <- ab %>%
    filter(!(school=="SIMBE"&year==2016&month_name=="set"&turma=="1-A"&day==16)) %>%
    mutate(day = ifelse(school=="SIMBE"&year==2016&month_name=="set"&turma=="1-A"&day==18,
                        day - 2,
                        day)) %>%
    mutate(day = ifelse(school=="SIMBE"&year==2016&month_name=="mar"&turma=="2-A"&day>5&day<12,
                        day + 1,
                        day)) %>%
    filter(!(school=="SIMBE"&year==2016&month_name=="mai"&turma=="5-A"&(day==7|day==8)))

  # *XINAVANE
  ab <- ab %>%
    mutate(year = ifelse(serial_number==478684,
                         2016,
                         year)) %>%
    mutate(day = ifelse(school=="XINAVANE"&year==2015&month_name=="mai"&turma=="3-A"&day>15&day<19,
                        day - 2,
                        day)) %>%
    filter(!(school=="XINAVANE"&year==2015&month_name=="jul"&turma=="3-A"&(day==4|day==5))) %>%
    mutate(day = ifelse(school=="XINAVANE"&year==2015&month_name=="jul"&turma=="3-A"&day>7&day<20,
                        day - 2,
                        day)) %>%
    filter(!(school=="XINAVANE"&year==2015&month_name=="mar"&turma=="5-C"&(day==14|day==15))) %>%
    mutate(day = ifelse(school=="XINAVANE"&year==2015&month_name=="mar"&turma=="5-C"&day>16&day<29,
                        day - 1,
                        day)) %>%
    filter(!(school=="XINAVANE"&year==2015&month_name=="jul"&turma=="5-C"&(day==18|day==19))) %>%
    mutate(day = ifelse(school=="XINAVANE"&year==2015&month_name=="jul"&turma=="5-C"&day>7&day<13,
                        day - 2,
                        day)) %>%
    filter(!(school=="XINAVANE"&year==2016&month_name=="abr"&turma=="1-A")) %>%
    mutate(day = ifelse(school=="XINAVANE"&year==2016&month_name=="jun"&turma=="1-A"&day>4&day<10,
                        day + 1,
                        day)) %>%
    filter(!(school=="XINAVANE"&year==2016&month_name=="mai"&turma=="1-B"&day==22)) %>%
    filter(!(school=="XINAVANE"&year==2016&month_name=="jul"&turma=="2-A"&day==3)) %>%
    mutate(day = ifelse(school=="XINAVANE"&year==2016&month_name=="mai"&turma=="2-B"&day>7&day<13,
                        day + 1,
                        day)) %>%
    filter(!(school=="XINAVANE"&year==2016&month_name=="mar"&turma=="2-D"&(day==5|day==6)),
           !(school=="XINAVANE"&year==2016&month_name=="jun"&turma=="3-A"&day==26),
           !(school=="XINAVANE"&year==2016&month_name=="mar"&turma=="4-A"&day==6),
           !(school=="XINAVANE"&year==2016&month_name=="mar"&turma=="4-B"&day==27)) %>%
    mutate(day = ifelse(school=="XINAVANE"&year==2016&month_name=="ago"&turma=="4-B"&(day==28|day==29),
                        day + 1,
                        day)) %>%
    filter(!(school=="XINAVANE"&year==2016&month_name=="apr"&turma=="4-C"&day==17))
  
  
  # Change all date contingencies,
  # first as a function of year, then day
  ab$date <- as.Date(paste0(ab$year,
                            '-',
                            ab$month_number,
                            '-',
                            ab$day))
  ab$dow <- weekdays(ab$date)
  ab$dow <- factor(ab$dow,
                   levels = c('Monday',
                              'Tuesday',
                              'Wednesday',
                              'Thursday',
                              'Friday',
                              'Saturday',
                              'Sunday'))
  ab$month <- date_truncate(ab$date, level = 'month')
  ab$month_name <- NULL
  ab$month_number <- as.numeric(format(ab$date, '%m'))
  
  # Change all numbers/letters, as a function of turma
  ab$number <- as.numeric(unlist(lapply(strsplit(ab$turma, '-'), function(x){x[1]})))
  ab$letter <- unlist(lapply(strsplit(ab$turma, '-'), function(x){x[2]}))
  
  # Create a year_term variable
  ab$term <-
    ifelse(as.numeric(format(ab$month, '%m')) %in% 2:4, '1',
           ifelse(as.numeric(format(ab$month, '%m')) %in% 5:7, '2',
                  ifelse(as.numeric(format(ab$month, '%m')) %in% 8:10, '3',
                         NA)))
  ab$year_term <- paste0(ab$year,
                         '-',
                         ab$term)
  
  ab$n <- NULL
  # Re-check for duplicates
   ab_dups <-
     ab %>%
     group_by(name, district, year, term, date,
              number, letter, turma) %>%
     tally %>%
     ungroup %>%
     mutate(flag = n > 1) 
   ab <- ab %>%
     left_join(ab_dups) %>%
     filter(!flag) %>%
     dplyr::select(-flag)
   
   # Remove NA subjects
   performance <-
     performance %>%
     filter(!is.na(subject))
   

   
   # Define function for entry into status
   entry <- function(x,
                     force_first = FALSE){
     # Ensure that x is boolean
     if(!is.logical(x)){
       stop('x must be a logical/boolean vector.')
     }
     # Define whether there is a change in status
     change_vec <- abs(as.numeric(x) - as.numeric(lag(x)))
     # Flag days which are both a change
     # and are TRUE (ie, entry into lateness or entry into default)
     entry_days <- which(x & change_vec == 1)
     # Return a boolean of entry into lateness
     return_vec <- rep(FALSE, length(x))
     return_vec[entry_days] <- TRUE
     # Force the first value to be copied if applicable
     if(force_first){
       return_vec[1] <- x[1]
     }
     return(return_vec)
   }
   
   # Get the number of students (max) for each turma
   # and drop those dates with very few observations
   ab <- ab %>%
     group_by(school, turma, month) %>%
     mutate(max_students = length(unique(id))) %>%
     ungroup %>%
     group_by(school, turma, year) %>%
     mutate(max_students_year = max(max_students, na.rm = TRUE)) %>%
     ungroup %>%
     filter(max_students_year >= 8) %>%
     group_by(school, turma, date) %>%
     mutate(n_eligibles = length(unique(id))) %>%
     mutate(drop = n_eligibles < (0.5 * max_students_year)) %>%
     ungroup %>%
     filter(!drop) %>%
     dplyr::select(-drop)
   
   # Drop weekends
   ab <- ab %>%
     filter(! dow %in% c('Saturday', 'Sunday'))
   
   # Identify those unlikely chains of presences
   x <- ab %>%
     arrange(school, turma, date) %>%
     group_by(school, turma, date) %>%
     summarise(presences = length(which(!absent)),
               absences = length(which(absent))) %>%
     ungroup %>%
     mutate(any_absence = absences > 0,
            any_presence = presences > 0) %>%
     mutate(change = any_absence != dplyr::lag(any_absence, n = 1, default = FALSE)) %>%
     group_by(school, turma) %>%
     mutate(chain = cumsum(change) + 1) %>%
     ungroup %>%
     group_by(school, turma, chain) %>%
     mutate(cs_presences = cumsum(presences)) %>%
     mutate(cs_absences = cumsum(absences)) %>%
     ungroup %>%
     mutate(chain_type = ifelse(any_absence & any_presence, 
                                'mix',
                                ifelse(any_absence & !any_presence,
                                       'absence',
                                       ifelse(!any_absence & any_presence,
                                              'presence', NA)))) %>%
     dplyr::select(-change) %>%
     mutate(chain_length = ifelse(chain_type == 'mix', 0,
                                  ifelse(chain_type == 'absence',
                                         cs_absences,
                                         ifelse(chain_type == 'presence',
                                                cs_presences, NA))))
   
   # Define thresholds
   thresholds <-
     data_frame(chain_type = c('mix', 'absence', 'presence'),
                threshold = c(Inf, 15, 70))
   
   # Join thresholds to turma info
   x <- 
     left_join(x,
               thresholds,
               by = 'chain_type')
   
   # Get maximum chain length for each chain
   x <- x %>%
     group_by(school, turma, chain) %>%
     mutate(maximum_chain_length = max(chain_length)) %>%
     ungroup
   
   # Flag invalid chains
   x <- x %>%
     mutate(flag = maximum_chain_length > threshold)
   
   # Join back to absenteeism database
   ab <- 
     ab %>%
     left_join(x %>%
                 dplyr::select(school, turma, date, flag))
   
   # Remove the invalid chains
   ab <- ab %>%
     filter(!flag)
  
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


