# Scripts used to Generate Data in Simulation Study

- The script *generate_gaussian_nonstationary.R* generates from a seven variable hierarchy where the bottom level series come from ARIMA models with multivariate Gaussian innovations. The data is exported to the Data directory as a .csv file (wide format).

- The script *generate_nongaussian_nonstationary.R* generates from a seven variable hierarchy where the bottom level series come from ARIMA models. Innovations come from bivariate Gumbel meta-distributions with Beta margins. The data is exported to the Data directory as a .csv file (wide format).

- The script *generate_gaussian_stationary.R* generates from a seven variable hierarchy where the bottom level series come from ARIMA models with multivariate Gaussian innovations. The data is exported to the Data directory as a .csv file (wide format).

- The script *generate_nongaussian_stationary.R* generates from a seven variable hierarchy where the bottom level series come from ARIMA models. Innovations come from bivariate Gumbel meta-distributions with Beta margins. The data is exported to the Data directory as a .csv file (wide format).

The AR and MA coefficients are stored in .Rdata files where 'gn' in the filename denotes Gaussian Non-stationary, 'gs' in the filename denotes Gaussian Stationary etc.

