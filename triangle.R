library(tidyverse)
library(CASdatasets)
data(freclaimset2motor)

# Filtering to only include settled claims and columns of interest.

claims <- filter(tibble(freclaimset2motor$claimset), ClaimStatus == "fully closed")

claims <- subset(claims, select = c(ClaimID, OccurYear, ManagYear, PaidAmount))

# claims <- claims[,c(1:3,5)]

# Create a column of number of years taken to settle.

claims$difference = claims$ManagYear - claims$OccurYear

# Select only claims that occurred in 2009 or later.

claims <- filter(claims, OccurYear >= 2009)

# Create a new column for the total amount paid to policyholders for each distinct year
# of the claims and time taken to settle after claim. 

year <- claims %>% group_by(difference, OccurYear) %>% summarise(total = sum(PaidAmount))

# Compute cumulative sums.

cumul <- year %>% group_by(OccurYear) %>% mutate(Cumulative = cumsum(total))

cumul <- subset(cumul, select = c(difference, OccurYear, Cumulative))

# Show data in loss development triangle. 

triangle <- pivot_wider(cumul, names_from = difference, values_from = Cumulative)

