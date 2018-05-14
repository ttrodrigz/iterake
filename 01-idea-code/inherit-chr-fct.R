want <- factor(x = c("one", "two", "three"), 
               levels = c("one", "two", "three"),
               ordered = TRUE)

df <- tibble(
    a = c(1, 2, 3.4),
    x = c("one", "two", "three"),
    y = factor(x = c("one", "two", "three")),
    z = factor(x = c("one", "two", "three"),
               levels = c("three", "two", "one"),
               ordered = TRUE)
)

inherit_chr_fct <- function(to, from) {
    
    # to = character, from = factor
    if ((is.character(to) | is.factor(to)) & is.factor(from)) {
        
        to <- factor(x = to, 
                     levels = levels(from))
        
        if (is.ordered(from)) {
            
            to <- factor(to, ordered = TRUE)
            
        }
    }
    
    # to = factor, from = character
    if (is.factor(to) & is.character(from)) {
        
        to <- as.character(to)
    }
    
    to

}

inherit_chr_fct(df$x, want)
