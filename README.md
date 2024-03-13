Red-backed salamander and distribution in north-central Massachusetts
================
Joshua Harkness

## Summary

For my final project in Data Science II, I worked with a dataset from
the Harvard Forest Data Archive (link below) which included abundance
and morphometrics of eastern red-backed salamanders (Plethodon cinereus)
in north-central Massachusetts. Motivation for this project came from
previous studies at HF which examined changes in vegetation and
invertebrate communities in response experimental removal of eastern
hemlock (Tsuga canadensis) to simulate loss of this foundational conifer
to exotic insect pests. Little work had previously looked at amphibian
communities. The primary research question which the authors developed,
and which I chose to pursue, was whether red-backed salamander were more
or less abundant in hemlock versus mixed deciduous forests.

Data was collected in 2005 by Brooks Mathewson and Elizabeth Colburn on
properties owned by Harvard Forest and on state-owned land in
north-central Massachusetts, within a 20-mile radius of the main Harvard
Forest property in Petersham. Fifteen sites occupying a variety of
mid-elevation forested areas were selected. Two paired, fixed-radius df
(each 0.33ha in area) were located within each site using random
sampling stratified by forest type; one site of the pair being in a
hemlock-dominated stand, the other being in a mixed deciduous stand.
Hemlock-dominated sites were defined as having \>50% of total basal area
(BA) being hemlock, and mixed deciduous as \<25% of total BA being
hemlock. Each plot center was used as an intersection between two
perpendicular 50m transects, along which pairs of cover boards (ACO
stations) were placed at the transect ends and intersection. Cover
boards were sampled once every 2-3 weeks from June 13th until November
4th 2005. All amphibian and reptile species detected under cover boards
were recorded along with basic morphometrics; for P. cinereus, total
length, snout-vent length, weight, phase, and sex were recorded. For all
other herps, snout-vent length was recorded.

In addition to the original research question of whether red-backed
salamanders were more or less abundant in hemlock-dominated forests, I
also wanted to use a variety of environmental variables to predict
salamander abundance. Additionally, I made several morphometric
comparisons between sexes of salamanders, and between forest types, and
compared species richness for amphibians between the two forest types.

Data analysis proved to be long and difficult. Due to the way data was
collected/transcribed, I encountered a number of issues in data tidying
and joining files. The primary issue stems from comparing different
sampling units between files; each observation in one file is a sampling
event at a specific cover board (a zero-rich datafile useful for
modeling presence/absence), but another file treats each row as an
individual salamander. There is also a small mismatch in observations
between several files, which when initially joined, duplicated
observations in the count variable, producing an artificially inflated
count in my original analysis. This meant that as I reanalyzed the data,
I tried to do as much analysis on separate files as possible, rather
than join them into one large dataframe as I initially intended; or join
specific columns/parts of a dataframe when needed.

I spent a long time on exploratory visualization and modeling,
attempting to identify relationships between variables and select
appropriate statistical methods to analyze them. This ultimately led me
to using non-parametric tests for significance (Wilcoxon signed rank
test, Kruskal Wallis, and Dunn tests) to explore differences between
forest types and sampling sites. Morphometric variables were mostly
normally distributed, which allowed me to use t-tests here. An
interesting issue arose when comparing total body length of salamanders,
since this is somewhat left skewed (apparently due to individuals with
short, injured/regenerating tails) and begins to violate assumptions of
a t-test – i.e., the mean is starting to verge on no longer being a good
summary statistic. I considered using a non-parametric test here, but
since the distribution was still mostly normal and I used t-tests for my
other comparisons, decided it was safe to stick with the t-test. I
tested many models that never made it into the final analysis, including
quadratic models, and generalized linear models including poisson,
quasi-poisson, and negative binomial regression. I first used poisson
regression, but encountered issues with overdispersion. Quasi poisson
may have still been appropriate, but the negative binomial distribution
was closer to what I observed in the data. I ultimately settled on using
a hurdle model, which includes a binomial presence/absence component and
a count component, with a negative binomial distribution. I also did
some predictive modeling using logistic regression, but the end result
did not explain the data particularly well. I want to continue exploring
the `tidymodels` package to see if there is a way to link use a negative
binomial distribution to this predictive model, since I was unable to
find a solution to this before finishing my project.

Ultimately, I found that very few environmental variables explained
abundance even reasonably well (most had essentially no relationship
with abundance). The variables I expected to be significantly correlated
with abundance, such as volume of coarse woody debris, soil pH, basal
area, etc., ended up being very poor predictors of abundance, which may
be due to sampling design, which did not include surveys of natural
cover objects, and may have artificially inflated counts for sites that
have little natural cover by providing novel cover objects. Notably,
canopy cover and average snag diameter were the only two predictors that
I included in my final hurdle model after running stepwise AIC
comparisons. This suggests that future work should attempt to sample
sites with lower canopy cover, perhaps using some of HF’s experimental
hemlock-removal/logging plots. Snag diameter may also be a proxy for
size of coarse woody debris, since standing deadwood should have little
effect on salamander abundance.

In conclusion, a part of me almost regrets choosing this dataset due to
the issues with data collection/transcription which I can’t attempt to
answer. But I definitely enjoyed the challenge and learned a lot about
different models, particularly ones that are well-suited to count and
zero-inflated data, which is something I have never encountered in
previous data science/statistics classes, but is a very common, real
world problem for ecologists. Additionally, this project has also
sparked in me some interest for further exploring predictive modeling
and machine learning.

## Presentation

My presentation can be found
[here](https://docs.google.com/presentation/d/1Tna8kLxuJGHAdAQ17Df_38RzThAZglP16X3g7tyfDAg/edit#slide=id.p)

## Data

Data citation: Mathewson B, Colburn E. 2023. Eastern Redback Salamander
Abundance in North Central Massachusetts 2005. Harvard Forest Data
Archive: HF131 (v.14). Environmental Data Initiative:
<https://doi.org/10.6073/pasta/4107874d4233c79b05929c7a93a60ca4>.

## References

Data source: <https://harvardforest.fas.harvard.edu/data-archives>
