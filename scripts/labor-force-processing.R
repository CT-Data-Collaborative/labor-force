library(dplyr)
library(datapkg)
library(readxl)
library(xlsx)

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
  current_file <- current_file[rowSums(is.na(current_file)) != ncol(current_file),]
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
  #bind together
  all_towns <- rbind(all_towns, current_file) 
}

##County Data####################################################################################################################

#grabs county xls files
county_path <- file.path(path, "county")
county_files <- dir(county_path, pattern = "cty")

all_counties <- data.frame(stringsAsFactors = F)
for (j in 1:length(county_files)) {
  current_file <- (read_excel(paste0(county_path, "/", county_files[1]), sheet=1, skip=1))
  colnames(current_file) <- c("Town/County", "Measure", "January", "February", "March", "April", "May", "June", 
                              "July", "August", "September", "October", "November", "December", "Annual Average")
  current_file <- current_file[rowSums(is.na(current_file)) != ncol(current_file),]
  current_file <- current_file[-c(1), ]
  #populate blank town lines with corresponding town
  currentCounty = current_file[1,1]
  for (i in 1:nrow(current_file)) {
    if(is.na(current_file[i,1])) {
      current_file[i,1] <- currentCounty
    } else {
      currentCounty <- current_file[i,1]
    }
  }
  #assign year
  get_year <- unique(as.numeric(unlist(gsub("[^0-9]", "", unlist(county_files[j])), "")))
  current_file$Year <- get_year
  #bind together
  all_towns <- rbind(all_towns, current_file) 
}

  county_file <- file.path(getwd(), "raw", "county", county_files[1])

counties <- read.xlsx(county_file, 1)
  
  # ,
  # rowIndex = 10:48,
  # colIndex = 1:15,
  # header = F
)





Measure	fixedMeasure	    Measure Type
L.F	    Labor Force	      Number
LF	    Labor Force	      Number
EMP	    Employment	      Number
EM	    Employment	      Number
UNEMP	  Unemployment	    Number
UN	    Unemployment	    Number
%	      Unemployment Rate	Percent
RT	    Unemployment Rate	Percent

   


