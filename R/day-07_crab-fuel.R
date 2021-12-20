pacman::p_load(
    "dplyr", 
    "purrr", 
    "tibble"
)

read_crabs <- function(path) {
    txt <- readLines(path)
    crab_positions <- stringr::str_split(txt, ",", simplify = TRUE) %>% as.numeric()
    crab_dat <- tibble(
        position = crab_positions
    ) %>%

    count(position)

    return(crab_dat)
}

calculate_fuel_cost <- function(meeting_position, crab_dat) {
    fuel_used <- sum(abs(meeting_position - crab_dat[["position"]]) * crab_dat[["n"]])
    return(fuel_used)
}

generate_test_grid <- function(crab_dat) {
    test_grid <- tibble(
        position = seq_len(max(crab_dat[["position"]]))
    )
    return(test_grid)
}

cost_meeting_spots <- function(crab_dat) {
    test_grid <- generate_test_grid(crab_dat)
    cost_grid <- test_grid %>%
        mutate(
            fuel_cost = position %>%
                map_dbl(calculate_fuel_cost, crab_dat)
        )

    return(cost_grid)
}

test_crabs <- read_crabs("input/day-07_crab-fuel_example.txt")

test_fuel_cost <- calculate_fuel_cost(test_crabs, 2)

cost_meeting_spots(test_crabs)

crabs <- read_crabs("input/day-07_crab-fuel.txt")

fuel_costs <- cost_meeting_spots(crabs)

# 07.01 Lowest fuel cost to align crabs

fuel_costs %>%
    arrange(fuel_cost)

calculate_fuel_cost_alt <- function(meeting_position, crab_dat) {
    travel_distances <- abs(meeting_position - crab_dat[["position"]])
    fuel_costs_unit <- travel_distances %>%
        map_dbl(
            function(x) {
                sum(seq_len(x))
            }
        )
    fuel_cost_total <- sum(fuel_costs_unit * crab_dat$n)

    return(fuel_cost_total)
    
}

cost_meeting_spots_alt <- function(crab_dat) {
    test_grid <- generate_test_grid(crab_dat)
    cost_grid <- test_grid %>%
        mutate(
            fuel_cost = position %>%
                map_dbl(calculate_fuel_cost_alt, crab_dat)
        )

    return(cost_grid)
}

test_fuel_cost_alt <- calculate_fuel_cost_alt(5, test_crabs)

test_fuel_cost_alt

fuel_costs_alt <- cost_meeting_spots_alt(crabs)

# 07.02 Lowest fuel cost with increasing cost function

fuel_costs_alt %>% arrange(fuel_cost) # 90040997
