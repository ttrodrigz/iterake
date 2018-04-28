# iterake

## Overview
iterake performs a procedure known as iterative raking to generate row/respondent level weights. Iterative raking (also known as rim weighting), is one of several methods used to correct the deviation between the *marginal* proportions in your sample from a known population for a given set of variables.

Three workhorse functions are inluded to help at every stage of the data weighting process:
* `pre_weight()` compares the unweighted data to the population model.
* `iterake()` performs the iterative raking.
* `post_weight()` evaluates the effects of the weighting model.

This package was built to play nicely with the [tidyverse](https://www.tidyverse.org/), so the API should feel somewhat familiar to the user of tidy tools.

## Installation
```
# install.packages("devtools")
devtools::install_github("ttrodrigz/iterake")
```

## Motivating Example
Suppose you've sampled 200 individuals from your home town to perform a study on used car buyers. In our omniscience, we know characteristics about age, gender, and the vehicle type owned in the population from which we are sampling.

<table>
<tr><th>Age</th><th>Gender</th><th>Vehicle Owned</th></tr>
<tr><td valign="top">

| Value   | Pop.     | Sample   |
|---------|:--------:|:--------:|
| 18-34   | 32.0%    | 34.0%    |
| 35-54   | 35.0%    | 33.5%    |
| 55+     | 33.0%    | 32.5%    |

</td><td valign="top">

| Value   | Pop.     | Sample   |
|---------|:--------:|:--------:|
| Female  | 54.0%    | 57.0%    |
| Male    | 46.0%    | 43.0%    |

</td><td valign="top">

| Value   | Pop.     | Sample   |
|---------|:--------:|:--------:|
| Car     | 38.0%    | 35.5%    |
| SUV     | 47.0%    | 51.5%    |
| Truck   | 15.0%    | 13.0%    |



</td></tr> </table>