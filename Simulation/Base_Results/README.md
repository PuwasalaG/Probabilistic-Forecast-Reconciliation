# Results from base forecasts

These files are large and are ignored in the github repository.  They can be generated using the scripts in the Base Forecasts directory (either submit job.sh to a SLURM cluster or edit the R script)

The filename is made up of three parts

- Whether the DGP is based on *gaussian* or *nongaussian* innovations.
- Whether the DGP includes some *nonstationary* series or whether all series are *stationary*.
- Whether the data are fit using *arima* or *ets* models.

Each file is an rds file containing a single R object.  This is a list with R=1000 elements each corresponding to a window.  Each element contains 5 further elements

- The details of the models are included in the field  *mable*.
- The forecast means are included in the field *fc_mean*.
- The forecast standard deviations are included in the field *fc_sd*.
- The forecast covariance matrix (sample) in included in the field *fc_Sigma_sam*.
- The forecast covariance matrix (shrink) in included in the field *fc_Sigma_shr*.
- The residuals are included in the field *resid*.
