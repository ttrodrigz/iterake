
iterake <img src=logo/ITERAKE_LOGO_01.png width=140 height=140 align="right" />
===============================================================================

**NOTE**: This package is under active development and is not yet ready to be used.

Overview
--------

iterake performs an algorithm known as iterative raking to generate row/respondent level weights for a sample of data. Iterative raking (also known as rim weighting), is one of several methods used to correct the deviation between the *marginal* proportions in your sample from a known population for a given set of variables.

Three workhorse functions are inluded to help at every stage of the data weighting process:

-   `pre_rake()` compares the unweighted data to the population model.
-   `iterake()` performs the iterative raking.
-   `post_rake()` evaluates the effects of the weighting model.

Prior to weighting, two helper functions are used to construct the population model (your weighting targets):

-   `wgt_cat()` constructs an individual weighting category (e.g., gender).
-   `pop_model()` collects all of the individual weighting categories and prepares them for `iterake()`.

This package was built to play nicely with the [tidyverse](https://www.tidyverse.org/), so the API should feel somewhat familiar to users of tidy tools.

<!--
## Installation
```
# install.packages("devtools")
devtools::install_github("ttrodrigz/iterake")
```

## Motivating Example
Suppose you've sampled 200 individuals from your home town to perform a study on used car buyers. In our omniscience, we know characteristics about age, gender, and the vehicle type owned in the population from which we are sampling.


Table: Age

Value   Pop.    Sample 
------  ------  -------
18-34   32.0%   34.0%  
35-54   35.0%   33.5%  
55+     33.0%   32.5%  



Table: Gender

Value    Pop.    Sample 
-------  ------  -------
Female   54.0%   57.0%  
Male     46.0%   43.0%  



Table: Vehicle

Value   Pop.    Sample 
------  ------  -------
Car     38.0%   35.5%  
SUV     47.0%   51.5%  
Truck   15.0%   13.0%  

From the above tables, we can see there is a difference between what we collected in our sample and what is observed in the population. We can use the raking procedure to generate weights such that the weighted proportions of the sample will match (or come very close) to the population. 

Note: we do not care about the joint probabilites of our weighting categories (i.e., percent of sample who are female, own an SUV, and are over 55 years old). As mentioned earlier, raking takes only into consideration the marginal probabilities.

## Usage
This is where I realize we should be using RMarkdown/knitr so we can include code + output. Oops.
-->
