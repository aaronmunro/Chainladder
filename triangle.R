library(tidyverse)
library(CASdatasets)
library(reticulate)
data(freclaimset2motor)

# Filtering to only include settled claims and columns of interest.

claims <- filter(tibble(freclaimset2motor$claimset), ClaimStatus == "fully closed")

claims <- subset(claims, select = c(ClaimID, OccurYear, ManagYear, PaidAmount))

# Create a column of number of years taken to settle.

claims$difference = claims$ManagYear - claims$OccurYear

# Select only claims that occurred in 2009 or later.

claims <- filter(claims, OccurYear >= 2009)

# Create a new column for the total amount paid to policyholders for each distinct year
# of the claims and time taken to settle after claim. 

year <- claims %>% group_by(difference, OccurYear) %>% summarise(total = sum(PaidAmount))

# Compute cumulative sums.

cumulation <- year %>% group_by(OccurYear) %>% mutate(Cumulative = cumsum(total))

simplified_cumulation <- subset(cumulation, select = c(difference, OccurYear, Cumulative))

# Show data in loss development triangle. 

triangle <- pivot_wider(simplified_cumulation, names_from = difference, values_from = Cumulative)

# 2. Age to age factors.

# Calculate the age to age factors by adding a new column detailing the year on year increase for 
# each iteration of year of cumulative claims.

factors <- cumulation %>% group_by(difference, OccurYear) %>% mutate(increase = Cumulative/(Cumulative - total))

# Clean and remove infinite values caused by dividing by 0 in first iteration of development from 0-1 years

factors <- factors[is.finite(rowSums(factors)),]

# Subset relevant columns for triangle. 

factors <- subset(factors, select = c(difference, OccurYear, increase))

# Show data in triangle. 

factors_triangle <- pivot_wider(factors, names_from = difference, values_from = increase)

# 3. Create vector of simple averages of age-to-age factors

averages <- as.list.data.frame(colMeans(factors_triangle[,2:ncol(factors_triangle)], na.rm = TRUE))

## Next up will be calculating cumulative development factors and projecting ultimate losses. 

CDF <- character()
for (j in 1:length(averages)-1) {
  CDF <- append(CDF, prod(averages[if (j==1) c(1:length(averages)) else (-c(1:j))]))
}

source_python("averages.py")
CDF_2 <- tibble(cumulativemeans(averages))
CDF_2 <- as.list.data.frame(cumulativemeans(averages))



