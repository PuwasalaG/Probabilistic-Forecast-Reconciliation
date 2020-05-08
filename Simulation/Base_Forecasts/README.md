# Scripts for producing base forecasts

- The file *base_forecasts.R* is a script that produces all base forecasts and retains important information.
- The file *job.sh* can be submitted onto a SLURM cluster.  This allows 8 different simulation scenarios to be run in parallel.
- The file *simtable.R* constructs a table lining up each simulation scenario with a number from 1-8 that can be matched to the slurm task id in job.sh.
- The file *SimulationTable.csv* is the table produces by simtable.R.


