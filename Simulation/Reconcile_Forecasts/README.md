# Scripts for producing reconciled forecasts

- The file *reconcile_forecasts.R* is a script that produces all reconciled forecasts and evaluates.
- The file *job.sh* can be submitted onto a SLURM cluster.  This allows 32 different simulation scenarios to be run in parallel.
- The file *simtable.R* constructs a table lining up each simulation scenario with a number from 1-32 that can be matched to the slurm task id in job.sh.
- The file *SimulationTable.csv* is the table produces by simtable.R.


