pacman::p_load(
    "dplyr", 
    "readr", 
    "stringr", 
    "tidyr", 
    "purrr"
)


load_elevations <- function(path) {
    raw <- readLines(path)
    trix <- raw |> str_split("", simplify = TRUE)
    class(trix) <- "numeric"

    return(trix)
}

get_ring <- function(elevations, x, y) {
    xmin <- max(x - 1, 1)
    xmax <- min(x + 1, ncol(elevations))
    ymin <- max(y - 1, 1)
    ymax <- min(y + 1, nrow(elevations))

    ring_coords <- expand_grid(y_pos = ymin:ymax, x_pos = xmin:xmax) |>
        filter(!(x_pos == x & y_pos == y)) |>
        as.matrix()

    ring_vals <- elevations[ring_coords]

    return(ring_vals)

}

is_low_spot <- function(x, y, elevations) {
    ring_vals <- get_ring(elevations, x, y)
    spot_val <- elevations[x, y]
    is_low <- all(spot_val < ring_vals)

    return(as.numeric(is_low))
}

find_low_spots <- function(elevations) {
    canvas <- matrix(0, nrow = nrow(elevations), ncol = ncol(elevations))
    low_spot_coords <- expand_grid(x = 1:ncol(elevations), y = 1:nrow(elevations)) |>
        mutate(
            is_low = map2_dbl(x, y, is_low_spot, elevations = elevations)
        ) |>
        filter(is_low) |>
        select(-is_low) |>
        as.matrix()

    canvas[low_spot_coords] <- 1

    return(canvas)
}

example_path <- "input/day-09_lava-tubes_example.txt"

elevations <- load_elevations(example_path)

test_ring <- get_ring(elevations, 1, 1)

test_low_spot_grid <- find_low_spots(elevations)

low_spot_coords |>
    mutate(
        m = map2_dbl(x, y, sum)
    )

is_low_spot_bin <- numeric(length = nrow(low_spot_coords))

for(i in seq_len(nrow(low_spot_coords))) {
    message(glue::glue("x: {low_spot_coords[['x']][i]} y: {low_spot_coords[['y']][i]}"))
    is_low_spot_bin[i] <- is_low_spot(low_spot_coords[["x"]][i], low_spot_coords[["y"]][i], elevations)

}
