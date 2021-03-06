# Simulation Study

- The directory *Base_Forecasts* includes a script for producing base forecasts (including files for doing so on a cluster using SLURM).

- The directory *Base_Results* stores the output for base forecasts as .rds files.  These are very large so are not stored on github.

- The directory *Reconcile_Forecasts* includes a script for producing optimal forecast reconciliation by SGA (including files for doing so on a cluster using SLURM).

- The directory *Reconciled_Results* stores the output for optimal forecast reconciliation by SGA as .rds files.  These are very large so are not stored on github.

- The directory *Reconcile_Forecasts_in* includes a script for producing optimal forecast reconciliation by SGA (including files for doing so on a cluster using SLURM).  Instead of a rolling window being used to train reconciliation weights, in-sample predictions are used instead

- The directory *Reconciled_Results_in* stores the output for optimal forecast reconciliation by SGA (in-sample version) as .rds files.  These are very large so are not stored on github.

- The directory *Data* includes simulated data as .csv files.

- The directory *Generate_Data* includes R scripts that generate DGPs.

- The directory *Evaluate* contains files for evaluating many reconciliation methods using energy score and variogram score.

- The directory *Result_Reports* contains files for tabulating results.
