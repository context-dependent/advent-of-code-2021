pacman::p_load(
    "purrr", 
    "dplyr"
)

raw_input <- readLines("input/day-04_bingo.txt")

draws <- raw_input[1] %>% 
    stringr::str_split(",", simplify = TRUE) %>% 
    as.numeric()

cards <- 

    tibble::tibble(
        line = raw_input[3:length(raw_input)], 
        card_index = cumsum(line == "") + 1
    ) %>%

    filter(line != "") %>%

    mutate(
        nums = stringr::str_split(line, " +") %>% map(as.numeric)
    ) %>%

    tidyr::unnest(nums) %>%

    filter(!is.na(nums)) %>%

    group_by(card_index) %>%

    summarize(card_nums = list(c(nums))) %>%
    ungroup() %>%
    mutate(
        card_squares = card_nums %>%
            map(
                function(x) {
                    matrix(x, ncol = 5, byrow = TRUE)
                }
            ), 
        card_marks = card_index %>%
            map(
                function(x) {
                    matrix(rep(0, 25), ncol = 5)
                }
            ),
        win = FALSE
    ) %>%
    select(
        index = card_index, 
        nums = card_squares, 
        marks = card_marks
    ) %>%

    as.list() %>%

    purrr::transpose() %>%
    walk(
        function(x) {
            x$draws <- numeric()
        }
    )


mark_draw <- function(cards, draw) {

    winning_card <- NULL

    res <- cards %>%
        map(
            function(x) {
                x$marks <- x$marks + as.integer(x$nums == draw)
                x$win <- check_win(x$marks)
                x$draws <- c(x$draws, draw)
                if(x$win) {
                    x$score <- sum(x$nums * (1 - x$marks)) * draw
                    winning_card <<- x
                }
                return(x)
            }
        )

    if(!is.null(winning_card)) {
        res$winning_card <- winning_card
    }

    return(res)

}

check_win <- function(marks) {
    win <- any(
        c(
            rowSums(marks), 
            colSums(marks)
        ) == 5
    )

    return(win)
}

run_game <- function(cards, draws, draw_index = 1, winning_card = NULL) {

    if(!is.null(cards$winning_card)) {
        return(cards$winning_card)
    }

    if(draw_index > length(draws)) {
        return(cards)
    }

    marked_cards <- mark_draw(cards, draws[draw_index])

    run_game(marked_cards, draws, draw_index + 1)

}

test_cards <- cards[1:3]

winning_card <- run_game(cards, draws)

# 04.01 What is the score of the first card to win?

winning_card$score # 8136


mark_draw_and_drop_wins <- function(cards, draw) {

    winning_card_indices <- numeric()

    marked_cards <- cards %>%
        imap(
            function(x, i) {
                x$marks <- x$marks + as.integer(x$nums == draw)
                x$win <- check_win(x$marks)
                x$draws <- c(x$draws, draw)
                if(x$win) {
                    x$score <- sum(x$nums * (1 - x$marks)) * draw
                    winning_card_indices <<- c(winning_card_indices, i)
                }
                return(x)
            }
        )

    if(!rlang::is_empty(winning_card_indices)) {
        res <- marked_cards[-1 * winning_card_indices]
    } else {
        res <- marked_cards
    }

    return(res)

}

find_last_winner <- function(cards, draws, draw_index = 1) {

    if(length(cards) == 1) {
        last_winner <- run_game(cards, draws, draw_index = draw_index + 1)
        return(last_winner)
    } 

    if(draw_index > length(draws)) {
        return(cards)
    }

    remaining_cards <- mark_draw_and_drop_wins(cards, draws[draw_index])

    find_last_winner(remaining_cards, draws, draw_index + 1)


}

test_last_winner <- find_last_winner(test_cards, draws)

last_winner <- find_last_winner(cards, draws)

# 04.02 What is the score of the card that would win last?

last_winner$score # 12738
