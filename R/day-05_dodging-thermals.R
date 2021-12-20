pacman::p_load(
    "purrr", 
    "dplyr", 
    "tidyr", 
    "readr", 
    "tibble"
)

read_thermal_vectors <- function(path) {

    dat <- 
    
        tibble(
            raw = read_lines(path)
        ) %>%

        separate(
            raw, 
            into = c("x1", "y1", "x2", "y2")
        ) %>%

        mutate(
            across(
                .cols = everything(),
                as.numeric
            )
        ) 

    res <- (dat + 1) # Input vectors assume 0 indexing. Adding 1 makes our lives ezer

    res <- res %>%
        mutate(
            vec = res %>% as.list() %>% transpose()
        )
        
    return(res)
}

test_vecs <- read_thermal_vectors("input/day-05_dodging-thermals_example.txt")

drop_diag <- function(vecs) {
    res <- vecs %>%
        filter(x1 == x2 | y1 == y2)
    
    return(res)
}

test_vecs_vh <- test_vecs %>% drop_diag()

test_vecs_vh

generate_canvas <- function(vecs) {
    canvas <- matrix(0, nrow = max(c(vecs$y1, vecs$y2)), ncol = max(c(vecs$x1, vecs$x2)))

    return(canvas)
}

test_canvas <- generate_canvas(test_vecs_vh)


test_canvas %>% plot_vh_vec(vec = list(x1 = 1, x2 = 3, y1 = 1, y2 = 1))

plot_vh_vecs <- function(canvas, vecs) {

    marked_canvas <- canvas

    vecs %>%
        walk(
            function(vec) {
                marked_canvas[vec$y1:vec$y2, vec$x1:vec$x2] <<- 
                    marked_canvas[vec$y1:vec$y2, vec$x1:vec$x2] + 1
            }
        )

    return(marked_canvas)
}

test_matrix <- test_canvas %>% plot_vh_vecs(test_vecs_vh$vec)

test_risk <- test_matrix > 1

test_risk %>%sum()

create_risk_matrix_vh <- function(dat) {
    dat_vh <- dat %>% drop_diag()
    canvas <- generate_canvas(dat_vh)
    marked_canvas <- canvas %>% plot_vh_vecs(dat_vh$vec)
    risk_matrix <- marked_canvas > 1
    return(risk_matrix)
}

thermal_readings <- read_thermal_vectors("input/day-05_dodging-thermals.txt")

risk_matrix_vh <- create_risk_matrix_vh(thermal_readings)

# 05.01 Using only vertical and horizontal lines, what is the number of intersections?

sum(risk_matrix_vh) # 5167

drop_vh <- function(dat) {
    res <- dat %>%
        filter(x1 != x2 & y1 != y2)
    
    return(res)
}

plot_diag_vec <- function(canvas, vec) {
    indices <- matrix(c(vec$y1:vec$y2, vec$x1:vec$x2), ncol = 2)
    canvas[indices] <- canvas[indices] + 1

    return(canvas)
}

plot_diag_vecs <- function(canvas, vecs) {
    marked_canvas <- canvas

    vecs %>%
        walk(
            function(vec) {
                indices <- matrix(c(vec$y1:vec$y2, vec$x1:vec$x2), ncol = 2)
                marked_canvas[indices] <<- marked_canvas[indices] + 1
            }
        )

    return(marked_canvas)
}

test_vecs_diag <- test_vecs %>% drop_vh()

test_vecs_diag

test_canvas %>% plot_diag_vec(test_vecs_diag$vec[[1]])

test_canvas <- generate_canvas(test_vecs_diag)

test_diag_plot <- test_canvas %>% plot_diag_vecs(test_vecs_diag$vec)

test_diag_plot

plot_vecs <- function(dat) {
    canvas <- generate_canvas(dat)
    vh_vecs <- drop_diag(dat)$vec
    diag_vecs <- drop_vh(dat)$vec

    marked_canvas_vh <- canvas %>% plot_vh_vecs(vh_vecs)
    marked_canvas <- marked_canvas_vh %>% plot_diag_vecs(diag_vecs)

    return(marked_canvas)

}

create_risk_matrix <- function(dat) {
    vec_plot <- dat %>% plot_vecs()
    risk_matrix <- vec_plot > 1
    return(risk_matrix)
}

risk_matrix <- create_risk_matrix(thermal_readings)

# 05.02 use all the lines

sum(risk_matrix) # 17604
