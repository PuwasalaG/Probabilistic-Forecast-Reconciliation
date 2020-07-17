# Report Results

Scripts for summarising results.

The mean score for the electrity data application. Base forecasts are generated either from a Gaussian density or by bootstrapping, assuming either independence or dependence. The reconciliation methods are:

- **Base**: Not a reconciliation method, just the base forecasts.
- **BottomUp**: Bottom up
- **BTTH**: Ben Taieb, Taylor Hyndman (2020). This is like bottom up but reorders a sample from
probabilistic forecast to match the empirical copula. Also the mean is adjusted to be the same as that
from MinT reconciliation.
- **JPP**: Jeon Panagiotelis Petropoulos (2019). This reorders a sample from the probabilistic forecast to be
perfectly dependent, i.e. it reconciles quantiles. Reconciliation is done by WLS (structural)
- **MinTSam**: MinT with the usual sample covariance estimator
- **MinTShr**: MinT with shrinkage covariance estimator
- **OLS**: OLS reconciliation
- **ScoreOptE**: Energy score Optimisation by stochastic gradient descent.
- **ScoreOptEIn**: Energy score Optimisation by stochastic gradient descent but with predicted values
(in-sample) used instead of rolling window forecasts.
- **ScoreOptV**: Variogram score Optimisation by stochastic gradient descent.
- **ScoreOptVIn**: Variogram score Optimisation by stochastic gradient descent but with predicted values
(in-sample) used instead of rolling window forecasts.
- **WLS**: Weighted least squares using structural scaling.

The .csv files contain mean energy score and mean variogram score. Nemenyi matrix plots are given as pdf files.
