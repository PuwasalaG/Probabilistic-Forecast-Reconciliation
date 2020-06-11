# Results from reconciled forecasts

These files are large and are ignored in the github repository.  They can be generated using the scripts in the Reconciled Forecasts (in-sample) directory (either submit job.sh to a SLURM cluster or edit the R script)

The filename is made up of six parts

- Whether the *energy* score or *variogram* score is optimised.
- Whether the DGP is based on *gaussian* or *nongaussian* innovations.
- Whether the DGP includes some *nonstationary* series or whether all series are *stationary*.
- Whether the data are fit using *arima* or *ets* models.
- Whether the base forecasts are dependent or independent.
- Whether the base forecasts are Gaussian or bootstrap innovations.

Each file is an rds file containing a single R object.  These contain details of the SGA for training optimal weights.
