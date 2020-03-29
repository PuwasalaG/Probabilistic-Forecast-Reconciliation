# Results from base forecasts

These files are large and are ignored in the github repository.  They can be generated using the scripts in the Base Forecasts directory (either submit job.sh to a SLURM cluster or edit the R script)

The filename is made up of four parts

- Whether the DGP is based on *gaussian* or *nongaussian* innovations.
- Whether the DGP includes some *nonstationary* series or whether all series are *stationary*.
- Whether the data are fit using *arima* or *ets* models.
- Whether propbabilistic forecasts are produced using *gaussian* or *bootstrapped* innovations.

Each file is an rds file containing a single R object.  This is a list with R=1000 elements each corresponding to a window.  Each element contains 3 further elements

- The details of the models are included in the field  *mable*.
- The point forecasts are included in the field *forecast*.
- The proababilistic forecasts are included in the field *paths*.
