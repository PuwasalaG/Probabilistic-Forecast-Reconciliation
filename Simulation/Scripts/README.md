#Scripts used in Simulation Study

- The script *generate_Gaussian.R* generates from a seven variable hierarchy where the bottom level series come from ARIMA models with multivariate Gaussian innovations. The data is exported to the Data directory as a .csv file (wide format) and an .rds file (tsibble).

- The script *generate_nongaussian.R* generates from a seven variable hierarchy where the bottom level series come from ARIMA models. Innovations come from bivariate Gumbel meta-distributions with Beta margins. The data is exported to the Data directory as a .csv file (wide format) and an .rds file (tsibble).
