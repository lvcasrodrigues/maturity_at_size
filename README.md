# Size at maturity
Here I provide a short code to estimate the size-at-maturity.

Through a generalized linear model under Binomial distribution Bin(n, p) we can estimate the size in which 50% (or other) of population are mature. Such information is pivotal for fishery sciences.

The model need 3 informations:
1) the size class,  preferably with intervals of same amplitudes;
2) number of sampled individuals mature by size class;
3) number of total sampled individuals by size class.

The code also brings a plot (ggplot) and a way to save it in high quality.
