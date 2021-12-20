pacman::p_load(
    "dplyr"
)

readings <- readr::read_fwf(
    "input/day-03_diagnostic-readings.txt",    
    col_positions = readr::fwf_widths(rep(1, 12))
)

digit_means <- colMeans(readings) %>%
    unname() 

gamma_bits <- round(digit_means) 
epsilon_bits <- round(1 - digit_means)

bits <- gamma_bits

evaluate_binary <- function(bits) {

    res <- sum(rev(bits) * (2 ^ (seq_along(bits) - 1)))

    return(res)

}

gamma_dec <- evaluate_binary(gamma_bits)
epsilon_dec <- evaluate_binary(epsilon_bits)

# 03.01 product of gamma and epsilon

gamma_x_epsilon <- gamma_dec * epsilon_dec

gamma_x_epsilon # 3901196

commonality_cascade <- function(readings, dir = c("common", "rare"), digit_index = 1, browse = FALSE) {

    if(browse) browser()

    if(nrow(readings) == 1) {
        digits <- unname(unlist(readings))
        return(digits)
    }

    target_digit <- readings[[digit_index]]

    if(mean(target_digit) == 0.5) {
        target_val <- case_when(
            dir[1] == "common" ~ 1, 
            dir[1] == "rare" ~ 0
        )
    } else {
        target_val <- case_when(
            dir[1] == "common" ~ round(mean(target_digit)), 
            dir[1] == "rare" ~ round(1 - mean(target_digit))
        )
    }

    readings <- readings %>%
        filter(target_digit == target_val)

    commonality_cascade(readings, dir = dir, digit_index = digit_index + 1, browse = browse)

}

oxygen_rating <- evaluate_binary(commonality_cascade(readings, "common"))
scrubber_rating <- evaluate_binary(commonality_cascade(readings, "rare"))

# 03.02 What is the product of the oxygen and scrubber ratings

life_support_rating <- oxygen_rating * scrubber_rating

life_support_rating # 4412188
