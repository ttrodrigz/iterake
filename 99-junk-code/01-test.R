# step 0) set up ----
library(tidyverse)

beat <- read_csv("./data/wgt_data.csv") %>%
    set_names(tolower)

# step 1) current freqs and props ----
wgt_cat <- c("demographics", "ownership", "luxury", "ethnicity")

current <- beat %>%
    select(cqno,
           one_of(wgt_cat)) %>%
    gather(wgt_cat, value, -cqno) %>%
    group_by(wgt_cat, value) %>%
    tally() %>%
    group_by(wgt_cat) %>%
    mutate(uwgt_prop = n / sum(n)) %>%
    nest()


# step 2) target props ----
targets <- tibble(
    wgt_cat = wgt_cat,
    data = list(
        tribble(~value, ~targ_prop,
                1,  .013,
                2,  .042,
                3,  .045,
                4,  .142,
                5,  .111,
                6,  .315,
                7,  .173,
                8,  .006,
                9,  .005,
                10, .030,
                11, .021,
                12, .066,
                13, .031),
        tribble(~value, ~targ_prop,
                1,  .013,
                2,  .012,
                3,  .017,
                4,  .047,
                5,  .083,
                6,  .169,
                7,  .134,
                8,  .089,
                9,  .151,
                10, .260,
                11, .025),
        tribble(~value, ~targ_prop,
                1,  .013,
                2,  .776,
                3,  .061,
                4,  .052,
                5,  .098),
        tribble(~value, ~targ_prop,
                1,  .013,
                2,  .136,
                3,  .008,
                4,  .007,
                5,  .008,
                6,  .729,
                7,  .032,
                8,  .045,
                9,  .022)
    )
)

left_join(
    x = targets %>% unnest(data),
    y = current %>% unnest(data)
    ) %>%
    mutate(deviation = targ_prop - uwgt_prop) %>%
    ggplot(aes(x = factor(value),
               y = deviation)) +
    # geom_point() +
    geom_col() +
    geom_hline(yintercept = 0, lty = "longdash") +
    facet_wrap(~wgt_cat, scales = "free_x")

# function to make excel target file
xl_wgt_template <- function(wgt_cats, path) {
    
    n_cats <- length(wgt_cats)
    
    base_df <- function() {
        tibble(
            value = integer(),
            targ_prop = double()
        )
    }
    
    target_file <- replicate(
        n = n_cats, 
        expr = base_df(), 
        simplify = FALSE
    )
    
    names(target_file) <- wgt_cats
    
    writexl::write_xlsx(x = target_file, path = path)
}

excel_target(wgt_cats = c("Gender", "Age", "Income"),
             path = "test.xlsx")


targets_xl
targets_df
targets_list

card <- function(wgt_cat, wgt_values, wgt_targets) {
    
    list(
        tibble(value = wgt_values,
               )
    )
}
    






my_targets <- function(...)

my_targets(card(...), card(...))












