source("library.R")

ca_households <- read.csv("psam_h06.csv",header=TRUE)
# 5-year household PUMS sample 2015-2019
# (csv is too large for repository, download yourself into folder)
# Documentation: https://www.census.gov/programs-surveys/acs/microdata.html
# Download and unzip: https://www2.census.gov/programs-surveys/acs/data/pums/2019/5-Year/csv_hca.zip

# Reduce dataset to Public Use Microdata Areas (PUMAs) of interest
# California PUMAs list: https://www.census.gov/geographies/reference-maps/2010/geo/2010-pumas/california.html
# For Greater Oakland, uncomment:
PUMAs_of_interest <- c(101:105) # Greater Oakland
geog_label <- "GreaterOakland"
# For Berkeley+Albany only (smaller sample size), uncomment:
#PUMAs_of_interest <- c(101) # Berkeley, Albany
#geog_label <- "BerkAlb"
# For San Francisco, uncomment: 
#PUMAs_of_interest <- c(7501:7507)
#geog_label <- "SanFrancisco"

households_set <- ca_households[ca_households$PUMA %in% PUMAs_of_interest,]

# data cleaning and labeling:

# adjust household income data to 2019 dollars w/ ADJINC (6 implied decimal places)
households_set$Adjusted_Income <- (as.double(households_set$HINCP) * 
                                     as.double(households_set$ADJINC))/1000000

# collapse building type categories to fewer to work with
households_set %<>% mutate(bldtype = case_when(
  BLD == 2 ~ "Single-family detached",
  BLD == 3  ~ "Single-family attached",
  BLD %in% 4:5 ~ "2-4 units",
  BLD %in% 6:7 ~ "5-19 units",
  BLD == 8 ~ "20-49 units",
  BLD == 9 ~ "50+ units",
  BLD %in% c(1,10) ~ "Other",
  TRUE ~ "N/A"
))

table_bld_types <- c("Single-family detached","Single-family attached",
                   "2-4 units", "5-19 units", "20-49 units","50+ units")


# collapse building age categories
households_set %<>% mutate(bldage = case_when(
  YBL %in% 1:2 ~ "Pre-1950",
  YBL %in% 3:4 ~ "1950-1969",
  YBL %in% 5:6 ~ "1970-1989",
  YBL %in% 7:8 ~ "1990-2004",
  YBL %in% 9:23 ~ "2005-2019",
  TRUE ~ "N/A"
))

table_bld_ages<- c("Pre-1950","1950-1969",
                   "1970-1989", "1990-2004", "2005-2019")

# add tenure type, collapse owned into one category (from free and clear / mortgage) 
households_set %<>% mutate(tenure = case_when(
  TEN %in% 1:2 ~ "Owned",
  TEN == 3 ~ "Rented",
  TEN == 4 ~ "Occupied w/o rent"
))


# blank results table to populate
row <- rep(-666,6)
resultsblank <- cbind(row,row,row,row,row)
colnames(resultsblank) <- table_bld_ages
rownames(resultsblank) <- table_bld_types

#compute 5X6 table of weighted median income in each cell

results <- resultsblank

for(i in 1:length(table_bld_types)) {
  
  for(j in 1:length(table_bld_ages)) {
    
  iter_set <- households_set[households_set$bldtype == 
                                      table_bld_types[i] & 
                                 households_set$bldage == 
                                      table_bld_ages[j] &
                               !is.na(households_set$Adjusted_Income),]
  if(sum(iter_set$WGTP>0,na.rm=TRUE)<10) { #write * in if the cell has  household sample size <10
   results[i,j] <- "*" 
  } else {
    results[i,j] <- weighted.median(iter_set$Adjusted_Income,iter_set$WGTP) #add weighted median value to cell
    }
  }
  }

filename <- paste("age_type_median_incs_",geog_label,"_2019.csv",sep="")
write.csv(results,filename)

# same for renters only

results <- resultsblank

for(i in 1:length(table_bld_types)) {
  
  for(j in 1:length(table_bld_ages)) {
    
    iter_set <- households_set[households_set$bldtype == 
                                 table_bld_types[i] & 
                                 households_set$bldage == 
                                 table_bld_ages[j] &
                                 !is.na(households_set$Adjusted_Income) &
                                 households_set$TEN==3,]
    if(sum(iter_set$WGTP>0,na.rm=TRUE)<10) {
      results[i,j] <- "*" 
    } else {
      results[i,j] <- weighted.median(iter_set$Adjusted_Income,iter_set$WGTP)
    }
  }
}

filename <- paste("age_type_median_incs_renters",geog_label,"_2019.csv",sep="")
write.csv(results,filename)

#same for owner-occupiers only

results <- resultsblank

for(i in 1:length(table_bld_types)) {
  
  for(j in 1:length(table_bld_ages)) {
    
    iter_set <- households_set[households_set$bldtype == 
                                 table_bld_types[i] & 
                                 households_set$bldage == 
                                 table_bld_ages[j] &
                                 !is.na(households_set$Adjusted_Income) &
                                 households_set$TEN %in% c(1,2),]

    if(sum(iter_set$WGTP>0,na.rm=TRUE)<10) {
      results[i,j] <- "*" 
    } else {
      results[i,j] <- weighted.median(iter_set$Adjusted_Income,iter_set$WGTP)
    }
  }
  
  }

filename <- paste("age_type_median_incs_owners",geog_label,"_2019.csv",sep="")
write.csv(results,filename)



# output the number of households Census estimated in each cell
# that were available for median income analysis
# (WGTP = households weights; people in group quarters like dorms, nursing 
# homes have WGTP 0 since designated as non-households)


results <- resultsblank

for(i in 1:length(table_bld_types)) {
  
  for(j in 1:length(table_bld_ages)) {
    
    iter_set <- households_set[households_set$bldtype == 
                                 table_bld_types[i] & 
                                 households_set$bldage == 
                                 table_bld_ages[j] &
                                 !is.na(households_set$Adjusted_Income),]
    results[i,j] <- sum(iter_set$WGTP,na.rm=TRUE)
  }
}

filename <- paste("age_type_HHs_count_",geog_label,"_2019.csv",sep="")
write.csv(results,filename)

# count households of renters only


results <- resultsblank

for(i in 1:length(table_bld_types)) {
  
  for(j in 1:length(table_bld_ages)) {
    
    iter_set <- households_set[households_set$bldtype == 
                                 table_bld_types[i] & 
                                 households_set$bldage == 
                                 table_bld_ages[j] &
                                 !is.na(households_set$Adjusted_Income) & 
                                 households_set$tenure=="Rented",]
    results[i,j] <- sum(iter_set$WGTP,na.rm=TRUE)
  }
}

filename <- paste("age_type_HHs_count_",geog_label,"_rented2019.csv",sep="")
write.csv(results,filename)


# count households of owner-occupiers


results <- resultsblank

for(i in 1:length(table_bld_types)) {
  
  for(j in 1:length(table_bld_ages)) {
    
    iter_set <- households_set[households_set$bldtype == 
                                 table_bld_types[i] & 
                                 households_set$bldage == 
                                 table_bld_ages[j] &
                                 !is.na(households_set$Adjusted_Income) &
                                 households_set$tenure=="Owned",]
    results[i,j] <- sum(iter_set$WGTP,na.rm=TRUE)
  }
}

filename <- paste("age_type_HHs_count_",geog_label,"_owned2019.csv",sep="")
write.csv(results,filename)

# count households occupying without paying rent only

results <- resultsblank

for(i in 1:length(table_bld_types)) {
  
  for(j in 1:length(table_bld_ages)) {
    
    iter_set <- households_set[households_set$bldtype == 
                                 table_bld_types[i] & 
                                 households_set$bldage == 
                                 table_bld_ages[j] &
                                 !is.na(households_set$Adjusted_Income) &
                                 households_set$tenure=="Occupied w/o rent",]
    results[i,j] <- sum(iter_set$WGTP,na.rm=TRUE)
  }
}

filename <- paste("age_type_HHs_count_",geog_label,"_occnorent2019.csv",sep="")
write.csv(results,filename)


# count UNWEIGHTED survey respondents (households set)

results <- resultsblank

for(i in 1:length(table_bld_types)) {
  
  for(j in 1:length(table_bld_ages)) {
    
    iter_set <- households_set[households_set$bldtype == 
                                 table_bld_types[i] & 
                                 households_set$bldage == 
                                 table_bld_ages[j] &
                                 !is.na(households_set$Adjusted_Income) &
                                 households_set$WGTP > 0,] #discard WGTP==0 group quarters not in median analysis
    results[i,j] <- nrow(iter_set)
  }
}

filename <- paste("age_type_unweighteds_count_",geog_label,"_2019.csv",sep="")
write.csv(results,filename)
