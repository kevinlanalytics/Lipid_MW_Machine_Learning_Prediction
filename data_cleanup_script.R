#import file and na.string enable to mark "blank" indices "NA" 
fileimport <- read.csv("/Users/sonicboy66/Documents/Research 2020 Spring/ML/Data Cleaning/stds_county_year_sex.csv", na.strings=c("","NA"))

#Rate with NA
Rate=fileimport$Rate

#NA percentage in the list
b=b="%"
c=mean(is.na(Rate))*100
sprintf(" the percentage of NA in the data list is %s %s", c,b)

#shows all "NA" indices 
NAindices = which(is.na(Rate))
NonNAindices = which(!is.na(Rate))

# (trial. 3) - find the smallest difference between non NA and each NA index. The 2 smallest difference will be the nearest indices 
# create the automation of this (current progress ) - this process takes 27 min to process the current data 
meanV<- c()
difference_NA <- c()

for(j in 1:length(NAindices)){
  for(i in 1:length(NonNAindices)){
    
    #find out the value distance between the non-NA indices and NA index
    difference_NA[i] <- NonNAindices[i] - NAindices[j]
    
    #closest index below the NA index (generalized)
    Neg <- NonNAindices[which.max(difference_NA[difference_NA<0])]
    
    #closest index above the NA index (generalized)
    Pos <- difference_NA[which.min(difference_NA[difference_NA>0])+which.max(difference_NA[difference_NA<0])] + NAindices[j]
  }
  #calc the mean of the two and save into a list meanV 
  meanL <- c(Rate[Neg], Rate[Pos])
  meanV[j] <- mean(meanL)
}

# replace new mean value to the NA indices 
for(z in 1:length(NAindices)){
  fileimport$Rate[NAindices[z]] <-meanV[z]
}
