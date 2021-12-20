library(dplyr)

depth_measurements <- readr::read_csv(
    "input/day-01_depth-measurements.txt", 
    col_names = c("depth")
)

depth_increases <- depth_measurements %>%
    mutate(
        depth_increase = depth > lag(depth)
    )

# 01.01 How many measurements are larger than the previous measurement?

n_depth_increase <- sum(depth_increases$depth_increase, na.rm = TRUE)

n_depth_increase # 1559

depth_lag_sums <- depth_measurements %>%
    mutate(
        lag_sum = depth + lag(depth) + lag(depth, n = 2L), 
        lag_sum_increase = lag_sum > lag(lag_sum)
    )

# 01.02 How many 3 measurement windows are larger than the previous 3 measurement window?

n_lag_sum_increase <- sum(depth_lag_sums$lag_sum_increase, na.rm = TRUE)

n_lag_sum_increase # 1600
