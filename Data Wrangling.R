# Install and load necessary packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tsibble")) install.packages("tsibble")

library(tidyverse)
library(tsibble)

# Load the datasets manually (assuming you have saved them in your working directory)
carroll <- read.csv("HC01ESTVC1713045.csv")
cobb <- read.csv("HC01ESTVC1713067.csv")
coweta <- read.csv("HC01ESTVC1713077.csv")
douglas <- read.csv("HC01ESTVC1713097.csv")
paulding <- read.csv("HC01ESTVC1713223.csv")


# Create 'county' column and rename time series to 'degree attainment'
carroll <- carroll %>% 
  rename(`degree attainment` = HC01ESTVC1713045) %>% 
  mutate(county = "Carroll")

cobb <- cobb %>% 
  rename(`degree attainment` = HC01ESTVC1713067) %>% 
  mutate(county = "Cobb")

coweta <- coweta %>% 
  rename(`degree attainment` = HC01ESTVC1713077) %>% 
  mutate(county = "Coweta")

douglas <- douglas %>% 
  rename(`degree attainment` = HC01ESTVC1713097) %>% 
  mutate(county = "Douglas")

paulding <- paulding %>% 
  rename(`degree attainment` = HC01ESTVC1713223) %>% 
  mutate(county = "Paulding")

# Combine datasets
degreeattainment <- bind_rows(carroll, cobb, coweta, douglas, paulding)

# Check column names
colnames(degreeattainment)

# Rename the 'DATE' column to 'date'
degreeattainment <- degreeattainment %>%
  rename(date = DATE)

# Convert the 'date' column to Date format (assuming it's in a standard format like YYYY-MM-DD)
degreeattainment$date <- as.Date(degreeattainment$date)

# Now, create a tsibble object with 'date' as the index and 'county' as the key
degreeattainment_tsibble <- degreeattainment %>%
  as_tsibble(index = date, key = county)

# View the tsibble to ensure it was created correctly
degreeattainment_tsibble



# Question (a): Which county had the highest degree attainment in 2022?
highest_2022 <- degreeattainment_tsibble %>%
  filter(year(date) == 2022) %>%
  filter(`degree attainment` == max(`degree attainment`))

highest_2022

# Question (b): Which counties demonstrate positive trends in degree attainment from 2019-2022?

# Calculate percentage change
percentage_change <- data_2019_2022 %>%
  group_by(county) %>%
  reframe(
    degree_2019 = max(`degree attainment`[date == as.Date("2019-01-01")], na.rm = TRUE),
    degree_2022 = max(`degree attainment`[date == as.Date("2022-01-01")], na.rm = TRUE),
    percentage_change = ifelse(
      !is.na(degree_2019) & !is.na(degree_2022),
      ((degree_2022 - degree_2019) / degree_2019) * 100,
      NA
    )
  ) %>%
  arrange(desc(percentage_change))  # Sort by percentage change

# Print the results
print(percentage_change)

# Question (c): Do the numbers surprise you?
# Answer with comments in the code:
# No, because these are in line with the population distribution and education funding patterns.

# Create a line graph for all counties
degreeattainment_tsibble %>%
  ggplot(aes(x = date, y = `degree attainment`, color = county)) +
  geom_line() +
  labs(title = "Degree Attainment in GA Counties", x = "Year", y = "Degree Attainment (%)") +
  theme_minimal()


# Question 2


# Import CPI data

CPI <- read.csv("CPIAUCSL.csv")

# Convert DATE to YearMonth format and create a tsibble
CPI_tsibble <- CPI %>%
  mutate(YearMonth = yearmonth(DATE)) %>%  # Convert DATE to yearmonth format
  as_tsibble(index = YearMonth)  # Create tsibble using YearMonth as the index

# View the tsibble to ensure it's created correctly
CPI_tsibble


#### OPTION 1


# Create 10 period autocorrelation function for CPI data

# Check if 'feasts' package is installed; if not, install it

if (!require("feasts")) install.packages("feasts")

# Load the 'feasts' package
library(feasts)

# Calculate the autocorrelation using the ACF() function from feasts
acf_data <- CPI_tsibble %>%
  ACF(CPIAUCSL, lag_max = 10)

# View the ACF values
acf_data

# Plot the ACF
autoplot(acf_data)

acf_data <- ACF(CPI_tsibble, CPIAUCSL, lag_max = 10)

# Conduct Ljung-Box test

lb_test <- Box.test(CPI_tsibble$CPIAUCSL, lag = 10, type = "Ljung-Box")

cat("Ljung-Box test results:\n")

print(lb_test)

#### OPTION 2

# Check for stationarity and difference if needed
CPI_tsibble <- CPI_tsibble %>%
  mutate(differenced = difference(CPIAUCSL))

# Create 10 period autocorrelation function for differenced data
acf_data <- ACF(CPI_tsibble, differenced, lag_max = 10)

# Graph the ACF
autoplot(acf_data) +
  labs(title = "Autocorrelation Function for Differenced CPI", x = "Lag", y = "ACF") +
  theme_minimal()

# Conduct Ljung-Box test on differenced data
lb_test <- Box.test(CPI_tsibble$differenced, lag = 10, type = "Ljung-Box")
print(lb_test)