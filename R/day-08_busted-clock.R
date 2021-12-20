pacman::p_load(
    "tidyverse"
)


read_trailing_codes <- function(path) {
    raw <- readLines(path)
    trailing_codes <- raw %>%
        str_sub(start = 62L) %>%
        str_split(" ")

    return(trailing_codes)
}

read_signals <- function(path) {
    raw <- readLines(path)
    signals <- raw %>%
        str_sub(end = 58) %>%
        str_split(" ")

    return(signals)
}

trailing_codes <- read_trailing_codes("input/day-08_busted-clock.txt")

# 08.01 number of 1s, 4s, 7s, 8s

trailing_codes  %>% # 519
    unlist()  %>%
    nchar() %in% 
    c(2, 3, 4, 7) %>%
    sum()

harmonize_codes <- function(disordered_codes) {
    res <- letters[1:7] %>%
        map(
            ~ str_detect(disordered_codes, .x)
        ) %>%
        set_names(letters[1:7]) %>%
        as_tibble()
    
    return(res)
}

read_codex <- function(path) {
    dat <- 
        tibble(
            raw = readLines(path)
        ) %>%
        separate(
            raw, 
            into = c("code", "codex_digit"), 
            sep = ": "
        ) 

    orderless_codes <- harmonize_codes(dat$code)
    res <- dat %>% cbind(orderless_codes) %>%
        select(-code)

    return(res)

}

prep_trailing_codes <- function(trailing_codes) {
    dat <- 
        tibble(
            code = trailing_codes
        ) %>%

        mutate(entry_index= row_number()) %>%
        unnest(code) %>%
        group_by(entry_index) %>%
        mutate(digit_index = row_number()) %>%
        ungroup()  %>%
        mutate(n_signal = nchar(code))
    
    clean_codes <- harmonize_codes(dat$code)

    res <- bind_cols(dat, clean_codes)

    return(res)

}

pull_byte_codes <- function(dat_signal) {
    dat_coded <- dat_signal %>%
        filter(!is.na(digit)) %>%
        rowwise() %>%
        mutate(
            byte_code = list(c(c_across(a:g)))
        )

    byte_codes <- dat_coded %>%
        pull(byte_code) %>%
        set_names(dat_coded$digit)

    return(byte_codes) 
    
}

decode_signal_pattern <- function(signal_pattern) {
    dat <- 
        tibble(
            signal = signal_pattern
        ) %>%
        mutate(
            n_sig = nchar(signal),
            codes = harmonize_codes(signal)
        ) %>%
        unpack(codes)  %>%
        rowwise() %>%
        mutate(
            byte_code = list(c(c_across(a:g)))
        )
    

    dat_1478 <- dat %>%
        mutate(
            digit = case_when(
                n_sig == 2 ~ 1, 
                n_sig == 4 ~ 4, 
                n_sig == 3 ~ 7, 
                n_sig == 7 ~ 8, 
                TRUE ~ NA_real_
            )
        )
    
    byte_codes <- pull_byte_codes(dat_1478)

    dat_digits <- dat_1478 %>%
        mutate(
            digit = case_when(
                n_sig == 6 & sum(byte_code & byte_codes$`4`) == 4 ~ 9, 
                n_sig == 6 & sum(byte_code & byte_codes$`1`) == 2 ~ 0, 
                n_sig == 6 ~ 6, 
                n_sig == 5 & sum(byte_code & byte_codes$`1`) == 2 ~ 3, 
                n_sig == 5 & sum(byte_code & byte_codes$`4`) == 3 ~ 5, 
                n_sig == 5 & sum(byte_code & byte_codes$`4`) == 2 ~ 2,
                TRUE ~ digit
            )
        )
    
    codex <- dat_digits %>%
        select(
            a:g, digit
        )

    return(codex)
}

load_data <- function(path) {
    res <- tibble(
        signals = read_signals(path), 
        outputs = read_trailing_codes(path)
    ) %>%
    mutate(
        index = row_number()
    )

    return(res)
}


dat <- load_data("input/day-08_busted-clock.txt")

codex <- dat %>%
    mutate(
        codices = signals %>%
            map(decode_signal_pattern)
    ) %>%
    select(
        index, 
        codices
    ) %>%
    unnest(codices)

digits <- dat %>%
    select(
        index, code = outputs
    ) %>%
    unnest(code) %>%
    mutate(harmonized_code = harmonize_codes(code)) %>%
    unpack(harmonized_code) %>%
    left_join(codex)

outputs <- digits %>%
    group_by(index) %>%
    summarize(
        output = as.numeric(paste0(digit, collapse = ""))
    )

sum(outputs$output)
