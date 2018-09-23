
iterake <img src=logo/ITERAKE_LOGO_01.png width=140 height=140 align="right" />
===============================================================================

**NOTE**: This package is under active development and is not yet ready to be used.

Overview
--------

iterake's main utility is creating row-level weights using a process called iterative raking. Iterative raking (also known as rim weighting), is one of several methods used to correct the deviation between the *marginal* proportions in a sample and a known population, or, universe as it was first referred to (Deming & Stephan 1940) for a given set of variables.

The weighting process with `iterake` is fairly straightforward; we suggest the following workflow:

1.  Use the `universe()` function to build your population.
    1.  The univerise is constructed with one or more characteristics where the marginal probabilites are known. These characteristics are built with the `build_margin()` function.
2.  Compare the marginal proportions in your sample with the population with `compare_margins()` function.
3.  If needed, create weights for your data using `iterake()`.
4.  Verify that the weighted proportions in your sample now match the population. The `compare_margins()` function is again used here.
5.  Check the performance of the weighting model with `weight_stats()`.

Motivating Example
------------------

Suppose a car dealership is interested in surveying the opinions of people who purchased a new vehicle from their lot. This dealership has been around since 2015 and has kept accurate records of its sales since its inception. The dealership randomly samples 200 individuals from its sales database to complete a customer satisfaction survey.

In order to try and make the sample represent the known population in their database, they calculate the marginal proportions of their sample and population for the following three categories:

<table class="kable_wrapper">
<tbody>
<tr>
<td>
<table>
<thead>
<tr>
<th style="text-align:center;">
Age
</th>
<th style="text-align:center;">
Pop.
</th>
<th style="text-align:center;">
Sample
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
18-34
</td>
<td style="text-align:center;">
12%
</td>
<td style="text-align:center;">
17%
</td>
</tr>
<tr>
<td style="text-align:center;">
35-54
</td>
<td style="text-align:center;">
58%
</td>
<td style="text-align:center;">
55%
</td>
</tr>
<tr>
<td style="text-align:center;">
55+
</td>
<td style="text-align:center;">
30%
</td>
<td style="text-align:center;">
28%
</td>
</tr>
</tbody>
</table>
</td>
<td>
<table>
<thead>
<tr>
<th style="text-align:center;">
Year
</th>
<th style="text-align:center;">
Pop.
</th>
<th style="text-align:center;">
Sample
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
2015
</td>
<td style="text-align:center;">
23%
</td>
<td style="text-align:center;">
20%
</td>
</tr>
<tr>
<td style="text-align:center;">
2016
</td>
<td style="text-align:center;">
26%
</td>
<td style="text-align:center;">
23%
</td>
</tr>
<tr>
<td style="text-align:center;">
2017
</td>
<td style="text-align:center;">
30%
</td>
<td style="text-align:center;">
34%
</td>
</tr>
<tr>
<td style="text-align:center;">
2018
</td>
<td style="text-align:center;">
21%
</td>
<td style="text-align:center;">
23%
</td>
</tr>
</tbody>
</table>
</td>
<td>
<table>
<thead>
<tr>
<th style="text-align:center;">
Type
</th>
<th style="text-align:center;">
Pop.
</th>
<th style="text-align:center;">
Sample
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
Car
</td>
<td style="text-align:center;">
38%
</td>
<td style="text-align:center;">
36%
</td>
</tr>
<tr>
<td style="text-align:center;">
SUV
</td>
<td style="text-align:center;">
47%
</td>
<td style="text-align:center;">
52%
</td>
</tr>
<tr>
<td style="text-align:center;">
Truck
</td>
<td style="text-align:center;">
15%
</td>
<td style="text-align:center;">
12%
</td>
</tr>
</tbody>
</table>
</td>
</tr>
</tbody>
</table>
There are slight deviations between what was collected in the sample and the known population. These deviations are small enough where weighting can be performed to correct for these differences between the marginal proportions.
