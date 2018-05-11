good <- factor(x = c("one", "two", "three"), 
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

copy_chr_fct <- function(to, from) {
    
    # to = character, from = factor
    if (is.character(to) & is.factor(from)) {
        
        to <- as.factor(to)
        
        if (is.ordered(from)) {
            
            levels(to) <- levels(from)
            to <- factor(to, ordered = TRUE)
            
        }
    }
    
    # to = factor, from = character
    if (is.factor(to) & is.character(from)) {
        
        to <- as.character(to)
    }
    
    to

}

copy_chr_fct(df$x, good)
l