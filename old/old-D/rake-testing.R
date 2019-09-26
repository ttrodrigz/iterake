library(tidyverse)
library(crayon)

# set up pop.model
model_test <- pop_model(
    
    # age category
    wgt_cat(name = "age",
            value = c("18-34", "35-54", "55+"),
            targ_prop = c(0.320, 0.350, 0.330)),
    
    # gender category
    wgt_cat(name = "gender",
            value = c("Female", "Male"),
            targ_prop = c(0.54, 0.46)),
    
    # vehicle category
    wgt_cat(name = "vehicle",
            value = c("Car", "SUV", "Truck"),
            targ_prop = c(0.380, 0.470, 0.150))
    
)

# read in data and force an initial wgt value of 1
data_test <- read_rds("./data/test_data.RData") %>%
    mutate(wgt = 1)

iterake <- function(data, pop.model, wgt.name = "weight", id.name = "id", wgt.lim = 3, threshold = 1e-10, max.iter = 50)
    {
    # initialize things
    check <- 1
    count <- 0
    
    # check if id variable name exists - if not, make it
    if (!(id.name %in% names(data))) {
        data <- data %>%
            mutate(!!id.name := 1:nrow(.))
    }

    # do the loops until the threshold is reached
    while (check > threshold) {
        
        # iteration limit check
        if (count >= max.iter) {
            n <- nrow(data)
            data <- NULL
            break
        }
        
        # loop through each variable in pop.model$wgt_cat to generate weight
        for (i in 1:length(pop.model$wgt_cat)) {
            
            # merge data
            data <- merge(
                
                data,
                
                # with a merge of target proportion data
                merge(pop.model$data[[i]], 
                      
                      # and current weighted proportions data
                      data %>% 
                          group_by(get(pop.model$wgt_cat[[i]])) %>%
                          summarise(act_prop = sum(wgt) / nrow(.)) %>%
                          set_names("value", "act_prop"),
                      
                      by = "value") %>%
                    
                    # calculate weight needed based on targ and actual proportions
                    mutate(wgt_temp = ifelse(act_prop == 0, 0, targ_prop / act_prop)) %>%
                    
                    # only keep variable value and weight
                    select(value,
                           wgt_temp),
                
                by.x = pop.model$wgt_cat[[i]],
                by.y = "value") %>%
                
                # and then multiply the merged wgt_temp by orig weight
                mutate(wgt = wgt * wgt_temp,
                       
                       # and force them to be no larger than wgt.lim, no smaller than 1/wgt.lim
                       wgt = ifelse(wgt >= wgt.lim, wgt.lim, 
                                    ifelse(wgt <= 1/wgt.lim, 
                                           1/wgt.lim, wgt))) %>%
                
                # and remove wgt_temp
                select(-wgt_temp)
        }
        
        # reset/initialize check value
        check <- 0
        
        # loop through each to calculate discrepencies
        for (i in 1:length(pop.model$wgt_cat)) {
            
            # check is the sum of whatever check already is
            check <- check +
                
                # plus the sum of a pull from a merge of target proportions
                sum(merge(pop.model$data[[i]], 
                          
                          # and current weighted proportions data
                          data %>% 
                              group_by(get(pop.model$wgt_cat[[i]])) %>%
                              summarise(act_prop = sum(wgt) / nrow(.)) %>%
                              set_names("value", "act_prop"),
                          
                          by = "value") %>%
                        
                        # calculate diff between targ and actual
                        mutate(prop_diff = abs(targ_prop - act_prop)) %>%
                        
                        # pull for sum
                        pull(prop_diff))
        }
        
        count <- count + 1
    }

    # what to return
    if (is.null(data)) {
        
        out_bad <- red $ bold
        
        cat('-- ' %+% bold('iterake diagnostics') %+% ' -----------------\n')
        cat(' Convergence: ' %+% out_bad('Failed '%+% '\U2718') %+% '\n')
        cat('  Iterations: ' %+% out_bad(max.iter) %+% '\n\n')
        cat('Unweighted N: ' %+% out_bad(n) %+% '\n')
        cat(' Effective N: ' %+% out_bad('--') %+% '\n')
        cat('  Weighted N: ' %+% out_bad('--') %+% '\n')
        cat('  Efficiency: ' %+% out_bad('--') %+% '\n')
        cat('        Loss: ' %+% out_bad('--') %+% '\n')
        
        out <- NULL
        
    } else {
        
        out <- data %>%
            select(id.name, wgt) %>%
            arrange(get(id.name)) %>%
            as_tibble()
        
        wgt <- out$wgt
        n <- nrow(out)
        wgt_n <- sum(wgt)
        neff <- (sum(wgt) ^ 2) / sum(wgt ^ 2)
        loss <- round((n / neff) - 1, 3)

        efficiency <- (neff / n)

        out_good <- green $ bold
        cat('-- ' %+% bold('iterake diagnostics') %+% ' -----------------\n')
        cat(' Convergence: ' %+% out_good('Success '%+% '\U2714') %+% '\n')
        cat('  Iterations: ' %+% out_good(count) %+% '\n\n')
        cat('Unweighted N: ' %+% out_good(n) %+% '\n')
        cat(' Effective N: ' %+% out_good(round(neff, 2)) %+% '\n')
        cat('  Weighted N: ' %+% out_good(wgt_n) %+% '\n')
        cat('  Efficiency: ' %+% out_good(scales::percent(round(efficiency, 4))) %+% '\n')
        cat('        Loss: ' %+% out_good(loss) %+% '\n')

    }
    
    # assign id/weight variable names here
    out %>%
        set_names(id.name, wgt.name)
}

out <- iterake(data_test, model_test)
clean <- data_test %>%
    select(-wgt) %>%
    left_join(out)
