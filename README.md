
iterake <img src=logo/ITERAKE_LOGO_01.png width=140 height=140 align="right" />
===============================================================================

**NOTE**: This package is under active development and is not yet ready to be used.

Overview
--------

iterake's main utility is creating row-level weights using a process called iterative raking. Iterative raking (also known as rim weighting), is one of several methods used to correct the deviation between the *marginal* proportions in a sample from a known population, or, universe as it was first referred to (Deming & Stephan 1940) for a given set of variables.

The weighting process with `iterake` is fairly straightforward; we suggest the following workflow:

1.  Use `universe()` to build your known population. This is built with one or more weighting categories with the `marginal()` function.
2.  Compare the marginal proportions in your sample with the population with `compare_margins()` function.
3.  If needed, create weights for your data using `iterake()`.
4.  Verify that the weighted proportions in your sample now match the population. The `compare_margins()` function is again used here.
5.  Check the performance of the weighting model with `weight_stats()`.

Motivating Example
------------------

Suppose a car dealership is interested in surveying the opinions of people who purchased a new vehicle from their lot. This dealership has been around since 2015 and has kept accurate records of its sales since its inception. The dealership randomly samples 200 individuals from its sales database to complete a customer satisfaction survey.

In order to try and make the sample represent the known population in their database, they calculate the marginal proportions of their sample and population for the following three categories:

| Value | Pop. | Sample |
|:------|:-----|:-------|
| 18-34 | 12%  | 17%    |
| 35-54 | 58%  | 55%    |
| 55+   | 30%  | 28%    |

|  Value| Pop. | Sample |
|------:|:-----|:-------|
|   2015| 23%  | 20%    |
|   2016| 26%  | 23%    |
|   2017| 30%  | 34%    |
|   2018| 21%  | 23%    |

| Value | Pop. | Sample |
|:------|:-----|:-------|
| Car   | 38%  | 36%    |
| SUV   | 47%  | 52%    |
| Truck | 15%  | 12%    |

There are slight deviations between what was collected in the sample and the known population. These deviations are small enough
