# Load packages
install.packages("readr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("forcats")
install.packages("skimr")
install.packages("janitor")
install.packages("ggplot2")

library(readr)
library(dplyr)
library(lubridate)
library(forcats)
library(ggplot2)

# Preprocess data
  # Consistent column names
  library(janitor)
  sales_data <- sales_data %>% clean_names(case = "snake")
  # Missing values
  anyNA(sales_data)   
  # Dataframe view
  glimpse(sales_data) 
  # Mutation
    # Date/Time variables
    sales_data <- sales_data %>% mutate(date = mdy(date), time = hms(time),
                                        weekday = wday(date),
                                        weekend = if_else(between(weekday, 2, 6), FALSE, TRUE),
                                        morning = am(time))
    # Continuous variables
    sales_data <- sales_data %>% mutate(price = unit_price * 1.05,
                                        cost = cogs / quantity)
    # Factor variables
    sales_data <- sales_data %>% mutate(customer_type = as.factor(customer_type),
                                        gender = as.factor(gender),
                                        product_line = as.factor(product_line),
                                        payment = as.factor(payment),
                                        weekday = as.factor(weekday),
                                        weekend = as.factor(weekend),
                                        morning = as.factor(morning))
    
# Explore data
  # Count: Factor variables
    # Aggregated level: Count the total observations/proportion for each factor variable
    # Product line
    sales_data %>% 
      count(product_line, sort = TRUE) %>%
      mutate(prop = n / sum(n)) 
    # Customer type
    sales_data %>% 
      count(customer_type, sort = TRUE) %>%
      mutate(prop = n / sum(n))
    # Gender
    sales_data %>% 
      count(gender, sort = TRUE) %>%
      mutate(prop = n / sum(n))
    # Payment
    sales_data %>% 
      count(payment, sort = TRUE) %>%
      mutate(prop = n / sum(n))
    # Date/Time
    sales_data %>% 
      count(weekday, morning) %>%
      mutate(prop = n / sum(n))
    
    # Group level: Count the total observations/fraction for each combination 
    # Product line + Customer type
    prod_customer <- sales_data %>% 
      count(product_line, customer_type) %>% 
      group_by(product_line) %>%
      mutate(fraction = n / sum(n)) 
    # Product line + Gender
    prod_gender <- sales_data %>% 
      count(product_line, gender) %>%
      group_by(product_line) %>%
      mutate(fraction = n / sum(n))
    # Product line + Payment
    prod_payment <- sales_data %>% 
      count(product_line, payment) %>%
      group_by(product_line) %>%
      mutate(fraction = n / sum(n))
    # Product line + Date 
    prod_date <- sales_data %>%
      count(product_line, weekday) %>%
      group_by(product_line) %>%
      mutate(fraction = n / sum(n)) 
    # Product line + Time
    prod_time <- sales_data %>%
      count(product_line, morning) %>%
      group_by(product_line) %>%
      mutate(fraction = n / sum(n))
    
  # Visualize: Factor variables
    # Group level: Visualize the fraction for each combination
    # Fraction of customer type for each product line
    ggplot(prod_customer, aes(x = customer_type, y = fraction)) +
      geom_col() +
      facet_wrap(~ product_line)
    # Fraction of gender for each product line
    ggplot(prod_gender, aes(x = gender, y = fraction)) +
      geom_col() +
      facet_wrap(~ product_line)
    # Fraction of payment for each product line
    ggplot(prod_payment, aes(x = payment, y = fraction)) +
      geom_col() +
      facet_wrap(~ product_line)
    # Fraction of weekday purchases for each product line 
    ggplot(prod_date, aes(x = weekday, y = fraction)) +
      geom_col() +
      facet_wrap(~ product_line)
    # Fraction of morning purchases for each product line
    ggplot(prod_time, aes(x = morning, y = fraction)) +
      geom_col() +
      facet_wrap(~ product_line)
      
  # Summarize: Continuous variables
    # Summary statistics: For each product line, calculate its price, quantity, and cost summary statistics
    sales_data %>% 
      select(product_line, price, quantity, cost) %>%
      group_by(product_line) %>%
      summarise(mean_p = mean(price), sd_p = sd(price),
                mean_q = mean(quantity), sd_q = sd(quantity),
                mean_c = mean(cost), sd_c = sd(cost))
    # Distributions
      # Price points
      sales_data %>% 
        # Round prices
        mutate(price = round(price)) %>% 
        # Aggregate quantity for a given price
        count(product_line, price, wt = quantity) %>% 
        # Plot price-quantity distributions
        ggplot(aes(x = price, y = n)) + 
          geom_point(position = "jitter") + 
          facet_wrap(~ product_line)
      # Price ranges
        # Price interval = 10
        sales_data %>%
          mutate(price = round(price)) %>% 
          mutate(price_range = case_when(between(price, 10, 20) ~ 1,
                                         between(price, 21, 30) ~ 2,
                                         between(price, 31, 40) ~ 3,
                                         between(price, 41, 50) ~ 4,
                                         between(price, 51, 60) ~ 5,
                                         between(price, 61, 70) ~ 6,
                                         between(price, 71, 80) ~ 7,
                                         between(price, 81, 90) ~ 8,
                                         between(price, 91, 100) ~ 9,
                                         TRUE ~ 10)) %>%
          count(product_line, price_range, wt = quantity) %>%
          ggplot(aes(x = price_range, y = n)) + 
            geom_point()  + 
            facet_wrap(~ product_line)
        # Price quantiles
          # Density plot
          sales_data %>%
            mutate(price = round(price)) %>%
            ggplot(aes(price)) + geom_histogram() + facet_wrap(~ product_line)
          # Statistics
          price_quantiles <- sales_data %>%
            mutate(price = round(price)) %>%
            group_by(product_line) %>%
            mutate(q1 = quantile(price, 1/4),
                   q2 = quantile(price, 2/4),
                   q3 = quantile(price, 3/4)) %>%
            select(product_line, price, quantity, q1, q2, q3)
          
          price_quantiles %>%
            mutate(price_range = case_when(price <= q1 ~ 1,
                                           price > q1 & price <= q2 ~ 2,
                                           price > q2 & price <= q3 ~ 3,
                                           price > q3 ~ 4)) %>%
            count(product_line, price_range, wt = quantity) %>%
            ggplot(aes(x = price_range, y = n)) + 
              geom_point() + 
              facet_wrap(~ product_line)
              
              