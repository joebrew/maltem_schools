library(dplyr)
library(sp)

# Magude  ----------------------------------------------------------
load('../census/maltem/2016-07-15_HOUSEHOLD.RData')
locations_magude <- HOUSEHOLD; rm(HOUSEHOLD)
locations_magude$lng <-
  locations_magude$longitude <-
  locations_magude$x <-
  locations_magude$lon <-
  locations_magude$HOUSEHOLD_HEAD_GPS_LNG
locations_magude$lat <-
  locations_magude$latitude <-
  locations_magude$y <-
  locations_magude$HOUSEHOLD_HEAD_GPS_LAT
locations_magude$geo <- 'Magude'
locations_magude$permid <- locations_magude$HOUSEHOLD_HEAD_PERM_ID
locations_magude$house_number <- locations_magude$HOUSEHOLD_HEAD_AGREG_NUM
locations_magude <-
  locations_magude %>% dplyr::select(house_number,
                                     lng,
                                     longitude,
                                     x,
                                     lon,
                                     lat,
                                     latitude,
                                     y,
                                     geo)
# Also get birthday
load('../census/maltem/2016-07-15_MEMBER.RData')
magude_member <- MEMBER; rm(MEMBER)
magude_member <-
  magude_member %>%
  dplyr::select(PERM_ID_MEMBER,
                BIRTH_MEMBER,
                MEMBER_GENDER,
                MEMBER_NAME,
                HOUSEHOLD_NUMBER) %>%
  rename(permid = PERM_ID_MEMBER,
         dob = BIRTH_MEMBER,
         gender = MEMBER_GENDER,
         name = MEMBER_NAME,
         house_number = HOUSEHOLD_NUMBER) %>%
  mutate(dob = as.Date(substr(dob, 1, 10)))
# Join
census_magude <-
  left_join(x = magude_member,
            y = locations_magude,
            by = 'house_number') %>%
  mutate(geo = 'Magude')
rm(magude_member)
# Recode gender
census_magude$gender <-
  ifelse(census_magude$gender == '1',
         'male',
         ifelse(census_magude$gender == '2', 
                'female',
                NA))

# Manhica ----------------------------------------------------------
load('../census/openhds/2016-07-15_individual.RData')
individual$dob <- as.Date(individual$dob)
individual$house_number <- substr(individual$lastName, 1, 8)
individual$name <- individual$firstName
individual$permid <- individual$lastName
individual <- individual %>%
  dplyr::select(permid, name, house_number, dob, gender)
# Read in coordinates (emailed from Charfudin)
coords <- readr::read_csv('../census/openhds/Coordenadas.csv')
names(coords) <- c('house_number', 'region', 'lat', 'lng')
coords$region <- NULL
# Combine
census_manhica <- left_join(individual,
                            coords,
                            by = 'house_number')
rm(individual, coords)

# Convert census_manhica to lat/lng
census_manhica_location <- census_manhica %>% filter(!is.na(lat) & !is.na(lng))
census_manhica_no_location <- census_manhica %>% filter(is.na(lat) | is.na(lng))
sp::coordinates(census_manhica_location) <- ~lng+lat
proj4string(census_manhica_location) <- CRS("+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
census_manhica_location <- spTransform(census_manhica_location, CRS('+proj=longlat'))
# Extract the coordinates
ll <- coordinates(census_manhica_location)
# Add back into the original dataframe
census_manhica_location$x <- ll[,1]
census_manhica_location$y <- ll[,2]
census_manhica_location <- data.frame(census_manhica_location@data)
# change names
census_manhica_location <-
  census_manhica_location %>%
  rename(lng = x,
         lat = y)
# Combine all of manhica back together
census_manhica <-
  rbind(census_manhica_location,
        census_manhica_no_location)
rm(census_manhica_location, census_manhica_no_location)

# Expand coordinates
census_manhica$longitude <-
  census_manhica$x <-
  census_manhica$lon <-
  census_manhica$lng
census_manhica$latitude <-
  census_manhica$y <-
  census_manhica$lat
census_manhica$geo <- 'Manhiça'
census_manhica <-
  census_manhica %>%
  dplyr::select(permid,
                dob,
                gender,
                name,
                house_number,
                lng,
                longitude,
                x,
                lon,
                lat,
                latitude,
                y,
                geo)
# Recode gender
census_manhica$gender <-
  ifelse(census_manhica$gender == 'F',
         'female',
         ifelse(census_manhica$gender == 'M',
                'male', 
                NA))


# Join the censuses ----------------------------------------
census_magude <- data.frame(census_magude)
census_manhica <- data.frame(census_manhica)
for (j in 1:ncol(census_manhica)){
  if(class(census_manhica[,j]) == 'factor'){
    census_manhica[,j] <- as.character(census_manhica[,j])
  }
  if(class(census_magude[,j]) == 'factor'){
    census_magude[,j] <- as.character(census_magude[,j])
  }
}
census <- rbind(census_manhica, census_magude)
rm(census_manhica, census_magude)

census$sex <- census$gender
