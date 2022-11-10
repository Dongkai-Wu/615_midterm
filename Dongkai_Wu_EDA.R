myName <- 'Dongkai Wu'
 
## The purpose of this R script is to get you started on the
## midterm project. 

library(tidyverse)
library(magrittr)
library(readxl)
library(ggplot2)

## Start by reading the data
strawb <- read_excel("~/Desktop/615/strawberries-2022oct30-a.xlsx", col_names = TRUE)

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

# Data cleaning - take out NA, Z, and D. 
chemical <- strawb %>% filter(`Value`!= "(NA)" & `Value`!= "(D)" & `Value`!= "(Z)" & `Domain`!= "TOTAL")

# Data Cleaning - take out rows units in $ and CWT and with Domain ORGANIC STATUS
surveydta <- chemical %>% filter(`Program`!= 'CENSUS' & `Domain`!= "ORGANIC STATUS")

# Data Cleaning - take out rows with Domain Category that contains (TOTAL) so the rest would be the unique chemicals
surveydta <- surveydta %>% filter(`Program`!= 'CENSUS' & `Domain Category`!= 'CHEMICAL, FUNGICIDE: (TOTAL)' & `Domain Category` != 'CHEMICAL, HERBICIDE: (TOTAL)' & `Domain Category` != 'CHEMICAL, INSECTICIDE: (TOTAL)' & `Domain Category` != 'CHEMICAL, OTHER: (TOTAL)')

cc  <- grep("OTHER", 
            strawb_chem$`chem_types`, 
            ignore.case = T)
length(cc)
chem_other <- surveydta %>% slice(cc)


length(strawb_chem$Year)
unique(strawb_chem$chem_types)

clean_chem <- strawb_chem %>% filter(`Value`!= "(NA)" & `Value`!= "(D)" & `Value`!= "(Z)"& `chem_name` != ' TOTAL')
clean_chem$Value <-as.numeric(clean_chem$Value)



othertop5 <- clean_chem %>% filter(`chem_types`== 'OTHER' & `chem_name` != ' TOTAL')
othertop5 <- othertop5[order(othertop5$Value, decreasing = TRUE), ]



Bifenthrin <- clean_chem %>% filter(`chem_name` == ' BIFENTHRIN ' & `State` == 'CALIFORNIA')

Bifenthrin_lb <- Bifenthrin %>% filter(`units` == ' LB')


library(ggplot2)

ggplot(Bifenthrin_lb) +
 aes(x = Year, y = Value) +
 geom_point(shape = "circle", size = 1.5, colour = "#112446") +
 theme_minimal()+
  geom_smooth()

# Simple Bar Plot
counts <- table(Bifenthrin_lb$Value)
barplot(counts, main="Car Distribution",
        xlab="Number of Gears")



clean_chem$Value <-as.numeric(clean_chem$Value)


ggplot(Bifenthrin_lb, aes(Year, Value)) + 
  geom_bar(stat = "identity")



