pacman::p_load(
    "dplyr"
)

sub_course <- readr::read_delim(
    "input/day-02_sub-movements.txt", 
    delim = " ", 
    col_names = c("direction", "distance")
)

sub_course %>% count(direction)

sub_course_position <- sub_course %>%

    mutate( 
        dimension = case_when(
            direction %in% c("down", "up") ~ "down", 
            direction %in% c("forward", "backward") ~ "out", 
            TRUE ~ NA_character_
        ), 
        position_change = case_when(
            direction %in% c("up", "backward") ~ -1 * distance, 
            TRUE ~ distance
        )
    ) %>%

    group_by(dimension) %>%

    mutate(position = cumsum(position_change)) 

# 02.01 What is the product of the sub's final depth and horizontal position?

final_position <- sub_course_position %>%
    group_by(dimension) %>%
    slice_tail() %>%
    ungroup() %>%
    select(dimension, position) %>%
    tidyr::pivot_wider(
        names_from = dimension, 
        values_from = position
    ) %>%
    mutate(product = down * out)

final_position # 2073315

sub_course_angle <- sub_course %>%

    mutate( 
        index = row_number(), 
        change = case_when(
            direction %in% c("down", "up") ~ "angle", 
            direction %in% c("forward") ~ "position", 
            TRUE ~ NA_character_
        ), 
        position_change = case_when(
            direction %in% c("up") ~ -1 * distance, 
            TRUE ~ distance
        )
    ) 

sub_index_angle <- sub_course_angle %>%
    filter(change == "angle") %>%
    mutate(current_angle = cumsum(position_change)) %>%
    select(index, current_angle)

sub_course_angle <- sub_course_angle %>%
    left_join(sub_index_angle) %>%
    tidyr::fill(current_angle) %>%
    mutate(
        current_angle = case_when(
            is.na(current_angle) ~ 0, 
            TRUE ~ current_angle
        )
    )

sub_position_angle <- sub_course_angle %>%
    filter(change == "position") %>%
    mutate(
        down = distance * current_angle, 
        out = distance, 
        depth = cumsum(down), 
        horizontal_position = cumsum(out), 
        product = depth * horizontal_position
    )

# 02.02 What is the product of depth and horizontal position 
#       with the new method of course calculation?

sub_position_last <- sub_position_angle %>%
    slice_tail()

sub_position_last # 1840311528
