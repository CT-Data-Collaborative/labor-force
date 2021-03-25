library(dplyr)
library(datapkg)
library(readxl)
library(tidyr)

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

#read in entire xls file (all sheets)
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

###Create subsets

##Town Data######################################################################################################################

#grabs town xls files
town_path <- file.path(path, "town")
town_files <- dir(town_path, pattern = "xls")

for (i in 1:length(town_files)) {
  mysheets <- read_excel_allsheets(paste0(town_path, "/", town_files[i]))
  town_sheet_index <- grep("Towns", names(mysheets))
  town_sheet <- mysheets[[town_sheet_index]]
  get_year <- substr((unlist(gsub("[^0-9]", "", unlist(town_files[i])), "")), 1, 4)
  assign(paste0(get_year, "-towns"), town_sheet)
}

#Concatenate all DFs
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
all_town_dfs <- grep("towns", dfs, value=T)

#Combine all geog DFs into one data frame
all_towns <- data.frame(stringsAsFactors = F)
for (j in 1:length(all_town_dfs)) {
  current_file <- get(all_town_dfs[j])
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
  get_year <- substr((unlist(gsub("[^0-9]", "", unlist(town_files[j])), "")), 1, 4)
  current_file$Year <- get_year
  #trim white space from `Town/County` column
  current_file$`Town/County` <- trim(current_file$`Town/County`)
  #bind together
  all_towns <- rbind(all_towns, current_file) 
}

#Fix Union
all_towns$`Town/County` <- gsub("\\*", "", all_towns$`Town/County`)

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
county_files <- dir(county_path, pattern = "xls")

for (i in 1:length(county_files)) {
  mysheets <- read_excel_allsheets(paste0(county_path, "/", county_files[i]))
  county_sheets <- c("County", "COUNTIES")
  county_sheet_index <- unique (grep(paste(county_sheets, collapse="|"), names(mysheets))) 
  county_sheet <- mysheets[[county_sheet_index]]
  get_year <- substr((unlist(gsub("[^0-9]", "", unlist(county_files[i])), "")), 1, 4)
  assign(paste0(get_year, "-counties"), county_sheet)
}

#Concatenate all DFs
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
all_county_dfs <- grep("counties", dfs, value=T)

all_counties <- data.frame(stringsAsFactors = F)
for (j in 1:length(all_county_dfs)) {
  current_file <- get(all_county_dfs[j])
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
  get_year <- substr((unlist(gsub("[^0-9]", "", unlist(county_files[j])), "")), 1, 4)
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

#convert wide to long format
all_geogs_long <- gather(all_geogs_meas, Month, Value, 3:15, factor_key=F)

#Setting "Annual Average" as the first level, so the Month column doesn't get converted to a date in CKAN datastore
all_geogs_long$Month <- factor(all_geogs_long$Month, 
                               levels = c("Annual Average", "January", "February", "March", "April", "May", "June", 
                                          "July", "August", "September", "October", "November", "December"))

all_geogs_long$Variable <- "Labor Force"

#Reorder columns
all_geogs_long <- all_geogs_long %>% 
  select(`Town/County`, FIPS, Year, Measure, Month, `Measure Type`, Variable, Value) %>% 
  arrange(`Town/County`, Year, Month)

#Set sigfigs in Value column (trim trailing zeros)
all_geogs_long$Value <- round(as.numeric(all_geogs_long$Value), 1)

# For some reason Counties and Connecticut are doubule-written in the final frame,
# once with the proper year and once with year=NA. So filter out NAs. - added by Ilya on May 21, 2019
all_geogs_long <- drop_na(all_geogs_long, 'Year')

# Write to File
write.table(
  all_geogs_long,
  file.path(getwd(), "data", "labor_force_2005-2020.csv"),
  sep = ",",
  row.names = F
)
