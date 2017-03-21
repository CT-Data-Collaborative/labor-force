library(dplyr)
library(datapkg)
library(readxl)

##################################################################
#
# Processing Script for Labor-Force
# Created by Jenna Daly
# On 03/10/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location))
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

###Create subsets

##Town Data######################################################################################################################

#grabs town xls files
town_path <- file.path(path, "town")
town_files <- dir(town_path, pattern = "town")

all_towns <- data.frame(stringsAsFactors = F)
for (j in 1:length(town_files)) {
  current_file <- (read_excel(paste0(town_path, "/", town_files[j]), sheet=1, skip=4))
  colnames(current_file) <- c("Town/County", "Measure", "January", "February", "March", "April", "May", "June", 
                              "July", "August", "September", "October", "November", "December", "Annual Average")
  #remove blank rows
  current_file <- current_file[rowSums(is.na(current_file)) != ncol(current_file),]
  #remove first row
  current_file <- current_file[-c(1), ]
  #populate blank town lines with corresponding town
  currentTown = current_file[1,1]
  for (i in 1:nrow(current_file)) {
    if(is.na(current_file[i,1])) {
      current_file[i,1] <- currentTown
    } else {
      currentTown <- current_file[i,1]
    }
  }
  #assign year
  get_year <- unique(as.numeric(unlist(gsub("[^0-9]", "", unlist(town_files[j])), "")))
  current_file$Year <- get_year
  #trim white space from `Town/County` column
  current_file$`Town/County` <- trim(current_file$`Town/County`)
  #bind together
  all_towns <- rbind(all_towns, current_file) 
}

#Add FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
town_fips <- (town_fips_dp$data[[1]])

all_towns <- merge(all_towns, town_fips, by.x = "Town/County", by.y = "Town", all=T)
#remove "Connecticut"
all_towns <- all_towns[ grep("Connecticut", all_towns$`Town/County`, invert = TRUE) , ]

##County and State Data##########################################################################################################

#grabs county xls files
county_path <- file.path(path, "county")
county_files <- dir(county_path, pattern = "cty")

all_counties <- data.frame(stringsAsFactors = F)
for (j in 1:length(county_files)) {
  current_file <- (read_excel(paste0(county_path, "/", county_files[j]), sheet=1, skip=1))
  colnames(current_file) <- c("Town/County", "Measure", "January", "February", "March", "April", "May", "June", 
                              "July", "August", "September", "October", "November", "December", "Annual Average")
  #delete blank rows
  current_file <- current_file[rowSums(is.na(current_file)) != ncol(current_file),]
  #delete blank columns
  drops <- c(NA)
  current_file <- current_file[ , !(names(current_file) %in% drops)]
  #populate blank town lines with corresponding town
  currentCounty = current_file[1,1]
  for (i in 1:nrow(current_file)) {
    if(is.na(current_file[i,1])) {
      current_file[i,1] <- currentCounty
    } else {
      currentCounty <- current_file[i,1]
    }
  } 
  #now, only take rows we need to processing, 
  #based on whenever the "Town/County" column is equal to a county
  blankFilter <- logical()
  for(i in 1:nrow(current_file)) {
    blankFilter <- append(blankFilter, all(is.na(current_file[i,])))
  }
  counties <- c("FAIRFIELD COUNTY", "HARTFORD COUNTY", "LITCHFIELD COUNTY", "MIDDLESEX COUNTY", 
                "NEW HAVEN COUNTY", "NEW LONDON COUNTY", "TOLLAND COUNTY", "WINDHAM COUNTY", "STATEWIDE") 
  current_file <- current_file[!blankFilter & current_file$`Town/County` %in% counties,]

  #assign year
  get_year <- unique(as.numeric(unlist(gsub("[^0-9]", "", unlist(county_files[j])), "")))
  current_file$Year <- get_year
  #bind together
  all_counties <- rbind(all_counties, current_file) 
}

#set Town/County to correct case
all_counties$`Town/County` <- gsub("\\b([a-z])([a-z]+)", "\\U\\1\\E\\2", tolower(all_counties$`Town/County`), perl=TRUE)

#relabel statewide to CT
all_counties <- within(all_counties, `Town/County`[`Town/County` == 'Statewide'] <- 'Connecticut')

#Add FIPS
county_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-county-list/master/datapackage.json'
county_fips_dp <- datapkg_read(path = county_fips_dp_URL)
county_fips <- (county_fips_dp$data[[1]])

all_counties <- merge(all_counties, county_fips, by.x = "Town/County", by.y = "County", all=T)

#Combine town and county/state data
all_geogs <- rbind(all_towns, all_counties)

#merge in measure type crosswalk
fixed_meas <- read.csv(paste0(path, "/", "fixed_measures.csv"), stringsAsFactors=F, header=T)
all_geogs_meas <- merge(all_geogs, fixed_meas, by = "Measure")
all_geogs_meas$Measure <- all_geogs_meas$fixedMeasure
all_geogs_meas$fixedMeasure <- NULL
names(all_geogs_meas)[names(all_geogs_meas) == 'Measure.Type'] <- 'Measure Type'

#Stack months
#convert to long format
cols_to_stack <- c("Annual Average", "January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")

long_row_count = nrow(all_geogs_meas) * length(cols_to_stack)

all_geogs_long <- reshape(all_geogs_meas, 
                          varying = cols_to_stack, 
                          v.names = "Value", 
                          timevar = "Month", 
                          times = cols_to_stack, 
                          new.row.names = 1:long_row_count,
                          direction = "long"
)

all_geogs_long$id <- NULL
all_geogs_long$Variable <- "Labor Force"

#Reorder columns
all_geogs_long <- all_geogs_long %>% 
  select(`Town/County`, `FIPS`, `Year`, `Measure`, `Month`, `Measure Type`, `Variable`, `Value`)

#Set sigfigs in Value column (trim trailing zeros)
all_geogs_long$Value <- as.numeric(all_geogs_long$Value)

# Write to File
write.table(
  all_geogs_long,
  file.path(getwd(), "data", "labor_force_2008-2015.csv"),
  sep = ",",
  row.names = F
)