myName <- 'Dongkai Wu'
 
## The purpose of this R script is to get you started on the
## midterm project. 

library(tidyverse)
library(magrittr)
library(readxl)

## Start by reading the data
strawb <- read_excel("Desktop/615/strawberries-2022oct30-a.xlsx",col_names = TRUE)

# data cleaning and rearrange
## Get the column names and index them
cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

## Set T as an indicator
T <- NULL

## Collect number of unique rows in each column
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}

## Use T to select columns to drop -- 
drop_cols <- cnames[which(T == 1)]

## Now, drop the columns with only one unique value.
strawb %<>% select(!all_of(drop_cols))

## Let's arrange the data frame by year and state.
strawb %<>% arrange(Year, State)

## now look at the `Data Item` column

temp1 <- strawb %>% select(`Data Item`) %>% 
         distinct()

strawb %<>% separate(col=`Data Item`,
                    into = c("Strawberries", "type", "items", "units"),
                    sep = ",",
                    fill = "right")


# Question 1
# LB = 100 x CWT
Number_lb <- 100 * 285
print(Number_lb)


# Question 2
# Margin of error (parameter) = Critical value x Standard deviation for the population
# population mean = 231304956 and CV = 13.7, So we could find the SD by mean x CV
mean = 231304956
SD <- 231304956 * 0.137
Up_bound <- mean + 1.96 * SD
low_bound <- mean - 1.96 * SD
print(Up_bound)
print(low_bound)

# Question 3 
# Answer is "NA" Because the column contains "(NA)", "(D)", and "(Z)"


# Question 4
totchemical <- strawb %>% filter(`Domain`!= "ORGANIC STATUS" & `Domain`!= "TOTAL")
unique_chem <-unique(totchemical$`Domain Category`)
length(unique(totchemical$`Domain Category`))

cc  <- grep("TOTAL", 
            totchemical$`Domain Category`, 
            ignore.case = T)

# Total unique chemicals equals unique Domain Category - Chemical name contains 'TOTAL' because this is the sum of the chemicals, not as a unique chemical. 
total_uniqchem <- length(unique_chem) - length(cc)
print(total_uniqchem)


# Question 5 

# total chemicals for cali

Calichemical <- strawb %>% filter(`Domain`!= "ORGANIC STATUS" & `Domain`!= "TOTAL" & State == "CALIFORNIA")
unique_chem <-unique(Calichemical$`Domain Category`)
length(unique(Calichemical$`Domain Category`))

cc  <- grep("TOTAL", 
            strawb$`Domain Category`, 
            ignore.case = T)

# Total unique chemicals equals unique Domain Category - Chemical name contains 'TOTAL' because this is the sum of the chemicals, not as a unique chemical. 
cali_uniqchem <- length(unique_chem) - length(cc)
print(cali_uniqchem)


# total chemicals for florida
floridachemical <- strawb %>% filter(`Domain`!= "ORGANIC STATUS" & `Domain`!= "TOTAL" & State == "FLORIDA")
unique_chem <-unique(floridachemical$`Domain Category`)
length(unique(floridachemical$`Domain Category`))

cc  <- grep("TOTAL", 
            strawb$`Domain Category`, 
            ignore.case = T)

# Total unique chemicals equals unique Domain Category - Chemical name contains 'TOTAL' because this is the sum of the chemicals, not as a unique chemical. 
florida_uniqchem <- length(unique_chem) - length(cc)
print(florida_uniqchem)

# number of chemicals used in cali - number of chemicals used in florida
cali_uniqchem - florida_uniqchem
