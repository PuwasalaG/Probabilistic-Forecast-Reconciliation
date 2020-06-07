# Scripts for producing optimal reconciled forecasts

- The file *reconcile_forecasts.R* is a script that finds optimal reconciled forecasts using SGA (in sample predictions used).
- The file *job.sh* can be submitted onto a SLURM cluster.  This allows 64 different scenarios to be run in parallel.
- The file *simtable.R* constructs a table lining up each simulation scenario with a number from 1-64 that can be matched to the slurm task id in job.sh.
- The file *SimulationTable.csv* is the table produces by simtable.R.


