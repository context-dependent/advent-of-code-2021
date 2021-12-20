pacman::p_load(
    "dplyr"
)

read_school <- function(path) {
    res <- readLines(path, n = 1L) %>%
        stringr::str_split(",", simplify = TRUE) %>%
        as.numeric()
    
    return(res)
}


pass_day <- function(old_school) {

    ind_zeros <- old_school == 0

    new_school <- old_school

    new_school[!ind_zeros] <- new_school[!ind_zeros] - 1
    new_school[ind_zeros] <- 6

    baby_fish <- rep(8, sum(ind_zeros))

    new_school <- c(new_school, baby_fish)

    return(new_school)
}

sim_school <- function(starting_school, n_days) {

    current_school <- starting_school

    for(i in seq_len(n_days)) {
        current_school <- pass_day(current_school)
    }

    return(current_school)
}



example_school <- read_school("input/day-06_lantern-fish_example.txt")

day_18 <- sim_school(example_school, 18)
length(day_18)

start_school <- read_school("input/day-06_lantern-fish.txt")

sim_80_days <- sim_school(start_school, 80)

# 06.01 How big is the school after 80 days?

length(sim_80_days) # 350605

# My computer can handle a vector ~350k numbers long with ease. 
# For the second star, however, we're going to have to be 
# a bit more intentional, as the same vector will be 
# orders of magnitude larger after 256 days.

make_school <- function(fish) {
    actual_days <- 

        tibble::tibble(
            days_left = fish
        ) %>%

        count(days_left)

    possible_days <- 
    
        tibble::tibble(
            days_left = 0:8
        )

    school <- possible_days %>%

        left_join(actual_days) %>%
        mutate(
            cohort_size = coalesce(n, 0)
        ) %>%
        select(-n)

    return(school)
    
}

pass_day_lite <- function(old_school_lite) {

    new_fish <- 

        tibble::tibble(
            days_left = c(6, 8), 
            cohort_size = old_school_lite[old_school_lite$days_left == 0, ][["cohort_size"]]
        )

    old_fish <- old_school_lite %>%

        filter(days_left > 0) %>%
        mutate(days_left = days_left - 1)

    new_school_lite <- old_fish %>%
        bind_rows(new_fish) %>%
        group_by(days_left) %>%
        summarize(cohort_size = sum(cohort_size)) %>%
        ungroup()

    return(new_school_lite)
}

sim_school_lite <- function(starting_school, n_days) {

    current_school <- starting_school

    for(i in seq_len(n_days)) {
        current_school <- pass_day_lite(current_school)
    }

    return(current_school)
}

test_school_lite <- make_school(example_school)

pass_day_lite(test_school_lite)

start_school_lite <- make_school(start_school)

sim_80_days_lite <- start_school_lite %>% sim_school_lite(80)

sim_256_days_lite <- start_school_lite %>% sim_school_lite(256)

# 06.02 How many lanternfish would there be after 256 days?

sim_256_days_lite$cohort_size  %>% sum() %>% as.character()
