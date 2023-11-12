#!/bin/bash
#SBATCH --time=3-00:00:00 # days-hours:minutes:seconds (or just minutes)
#SBATCH --nodes=1
#SBATCH -o /scratch/general/vast/Zahn/Marine_Fungi_SRA/slurm.output/slurm-%j.out-%N
#SBATCH -e /scratch/general/vast/Zahn/Marine_Fungi_SRA/slurm.output/slurm-%j.err-%N
#SBATCH --ntasks=16
#SBATCH --account=uvu
#SBATCH --partition=kingspeak
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=zahn.geoff@gmail.com
#SBATCH --job-name=SRA_MG_Marine

#

# load appropriate modules
module load sra-toolkit

# set up file paths
acc_list=/scratch/general/vast/Zahn/Marine_Fungi_SRA/download/SRR_Acc_List_Marine_MG.txt
out=/scratch/general/vast/Zahn/Marine_Fungi_SRA/sra_data

# run download script
while read line;
do
fasterq-dump $line --threads 16 --split-3 --outdir $out;
done < $acc_list

