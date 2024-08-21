#!/bin/bash
#SBATCH --time=14-00:00:00 # days-hours:minutes:seconds (or just minutes)
#SBATCH --nodes=1
#SBATCH -o /scratch/general/vast/Zahn/Marine_Fungi_SRA/slurm.output/slurm-%j.out-%N
#SBATCH -e /scratch/general/vast/Zahn/Marine_Fungi_SRA/slurm.output/slurm-%j.err-%N
#SBATCH --ntasks=64
#SBATCH --account=uvucos-np
#SBATCH --partition=uvucos-np
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=zahn.geoff@gmail.com
#SBATCH --job-name=MarineTest


#### OPTIONS ####

export OMP_NUM_THREADS=64
module load sra-toolkit spades seqtk fastp samtools bowtie2 python3 bedtools


metaeuk=/uufs/chpc.utah.edu/common/home/u6033249/programs/metaeuk/build/bin/metaeuk
mmseqs=/uufs/chpc.utah.edu/common/home/u6033249/programs/mmseqs/bin/mmseqs
annotation_db=/scratch/general/vast/Zahn/Databases/swissprot
temp=/scratch/general/vast/Zahn/Marine_Fungi_SRA/tmp # tmp dir for metaeuk
main_dir=/scratch/general/vast/Zahn/Marine_Fungi_SRA

cd $main_dir


threads=64


#------------------------------------------------------------------------------#
#### SETUP LIST OF PROJECTS AND RUNS ####
#------------------------------------------------------------------------------#

# list of SRA projects that passed metadata checks (change to complete list once bugs worked out)
project_list=/scratch/general/vast/Zahn/Marine_Fungi_SRA/metadata/passing_projects1.csv
run_list=/scratch/general/vast/Zahn/Marine_Fungi_SRA/metadata/passing_project-run_table.csv


#------------------------------------------------------------------------------#
#### BIG WHILE_LOOP ####
#------------------------------------------------------------------------------#

while read project
do

acc=$project
dl_dir=/scratch/general/vast/Zahn/Marine_Fungi_SRA/sra_data
raw_dir=$dl_dir/$acc
qc_dir=$raw_dir/QC
ass_dir=/scratch/general/vast/Zahn/Marine_Fungi_SRA/assemblies/$acc
annot_dir=/scratch/general/vast/Zahn/Marine_Fungi_SRA/annotations/$acc
mkdir -p $qc_dir $raw_dir $ass_dir $annot_dir

# enter main download directory
cd $raw_dir


# find runs associated with that project
runs=$raw_dir/$acc.runlist.txt
grep "$acc" $run_list | cut -d "," -f2 > $runs

# download each one, one run at a time
    while read run
    do
    
    #if file already downloaded, skip download
        if [[ ! -e "$raw_dir/${run}_1.fastq" && -e "$raw_dir/${run}_2.fastq" ]]; then
        fasterq-dump $run --threads 64 --split-3 --outdir $raw_dir
        fi
    done < $runs

# get list of fwd and rev files
ls -1 $raw_dir | grep "_1.fastq" > $raw_dir/$acc.fwds
ls -1 $raw_dir | grep "_2.fastq" > $raw_dir/$acc.revs

#------------------------------------------------------------------------------#
#### RUN QC ON EVERY RUN FROM PROJECT ####
    while read line;
    do
    F=$line
    R=${line/_1/_2}
    Fout=${line%%.fastq}.qc.fq
    Rout=${R%%.fastq}.qc.fq
    Mout=${F%%_1.fastq}.merged.fq
    # QC (merge here or later?)
        # test if file exists already. skip QC if TRUE    
        if [ ! -f "$Fout" ]; then
        
        echo "Performing QC on $Fout and $Rout"
        
        fastp -i $F -I $R -o $Fout -O $Rout -V --detect_adapter_for_pe \
        --cut_front --cut_mean_quality 20 --n_base_limit 0 \
        --average_qual 20 --correction --json /dev/null --html /dev/null
        fi
    
    done < $raw_dir/$acc.fwds

# move successful qc reads to new directory
mv *.qc.fq $qc_dir
# remove raw fastq files (will use QC reads for re-mapping)
rm $raw_dir/*.fastq

cd $main_dir

#------------------------------------------------------------------------------#
#### ASSEMBLE ####

for run in $(ls -1 $qc_dir/*1.qc.fq);
do
cleanup_header=$(basename ${run%%_1.qc.fq})
out=$ass_dir/$cleanup_header
mkdir $out
qc_fwd=$run
qc_rev=${run/_1.qc.fq/_2.qc.fq}
old_contigs=$out/contigs.fasta
new_contigs=$ass_dir/$cleanup_header.contigs.fasta

    # check if contigs already exist. Only assemble if FALSE
    if [ ! -f "$new_contigs" ]; then
    # run metaspades
    metaspades.py -t 64 -1 $qc_fwd -2 $qc_rev -o $out

    # move contigs to main assembly directory
    cat $old_contigs | seqtk seq -A > $new_contigs
    fi
    
# get easy variable
current_contigs=$new_contigs

#------------------------------------------------------------------------------#
#### COVERAGE ####

# remove all the spades extra stuff (just keeping contigs)
rm -r $out/


# map reads to contigs to get coverage estimates
	# map $qc_dir files to each contig file for that SRA run
	# index contigs
current_index=$annot_dir/$cleanup_header.index
current_sam=$annot_dir/$cleanup_header.sam
current_bam=$annot_dir/$cleanup_header.bam
sorted_bam=${current_bam/.bam/.sorted.bam}
current_coverage=$annot_dir/$cleanup_header.coverage.txt

bowtie2-build --threads $threads $current_contigs $current_index
bowtie2 --threads $threads -x $current_index -1 $qc_fwd -2 $qc_rev -S $current_sam
	# convert sam to bam
samtools view --threads $threads -S -b $current_sam > $current_bam
	# sort and index bam
samtools sort --threads $threads $current_bam -o $sorted_bam
samtools index -@ $threads $sorted_bam
	# calculate coverage
samtools depth --threads $threads $sorted_bam > $current_coverage

#------------------------------------------------------------------------------#
#### ANNOTATE ####


# annotate contigs
preds_out=$annot_dir/$cleanup_header.preds
$metaeuk easy-predict $current_contigs $annotation_db $preds_out metaeuk_tmp


#------------------------------------------------------------------------------#
#### LINK FEATURE COVERAGE ####

# Convert GFF to BED
awk 'BEGIN {OFS="\t"} {if ($0 !~ /^#/ && $3 != "region") {print $1, $4-1, $5, $3, ".", $7}}' $preds_out.gff > $preds_out.bed

#Convert coverage.txt to BED-like format
awk 'BEGIN {OFS="\t"} {print $1, $2-1, $2, $3}' $current_coverage > ${current_coverage/txt/bed}

# sort
bedtools sort -i $preds_out.bed > $preds_out.sorted.bed
bedtools sort -i ${current_coverage/txt/bed} > ${current_coverage/txt/sorted.bed}


#Calculate Coverage per Feature:
bedtools map -a $preds_out.sorted.bed -b ${current_coverage/txt/sorted.bed} -c 4 -o mean > $preds_out.coverage.bed




#------------------------------------------------------------------------------#
#### PREDICT TAXONOMY ####

$metaeuk taxtocontig --threads $threads $temp/latest/contigs $preds_out.fas $preds_out.headersMap.tsv $annotation_db $annot_dir/$cleanup_header. tax_tmp --majority 0.5 --tax-lineage 1 --lca-mode 2

# pull fungal stuff out
    # taxonomy assignments per contig
cat $annot_dir/$cleanup_header._tax_per_contig.tsv | grep -i "fungi" > $annot_dir/$cleanup_header.fungal_taxa.tsv
    # taxonomy assignments per predicted gene
cat $annot_dir/$cleanup_header._tax_per_pred.tsv | grep -i "fungi" > $annot_dir/$cleanup_header.fungal_taxa_per_pred.tsv
    # contigs (nucleotide)
grep -A1 -Fwf <(cut -f1 $annot_dir/$cleanup_header.fungal_taxa.tsv | sort -u) $new_contigs | grep -v "^--$" > $new_contigs.fungi
    # gff file
grep -Fwf <(cut -f1 $annot_dir/$cleanup_header.fungal_taxa.tsv | sort -u) $annot_dir/$cleanup_header.preds.gff | grep "CDS" > $annot_dir/$cleanup_header.preds.gff.fungi
    # nucleotide sequences
grep -A1 -Fwf <(head $annot_dir/$cleanup_header.preds.gff.fungi | cut -d "=" -f2 | cut -d ";" -f1) $annot_dir/$cleanup_header.preds.fas | grep -v "^--$" > $annot_dir/$cleanup_header.preds.faa.fungi
    # predicted genes coverage
 grep -Fwf <(cut -f1 $annot_dir/$cleanup_header.fungal_taxa.tsv | sort -u) $annot_dir/$cleanup_header.preds.coverage.bed | grep "CDS" > $annot_dir/$cleanup_header.preds.coverage.bed.fungi
    # rename
cp $annot_dir/$cleanup_header.fungal_taxa.tsv $annot_dir/$cleanup_header.taxonomy.fungi
grep -Fwf <(cut -f1 $annot_dir/$cleanup_header.fungal_taxa.tsv | sort -u) $annot_dir/$cleanup_header.preds.gff | grep "CDS" > $annot_dir/$cleanup_header.preds.gff.fungi


# delete all non-fungal files (gotta save space)
cd $annot_dir
ls -1 $annot_dir | grep -v ".fungi$" | xargs rm -r
cd $main_dir

# compress files to save space
gzip -f $ass_dir/$cleanup_header.contigs*
gzip -f $annot_dir/*.fungi


done  # done with current SRA RUN


#------------------------------------------------------------------------------#
#### CLEANUP FROM BIOPROJECT ####


# update project list before moving to next BioProject
cat $project_list | grep -v "$acc" | grep -v "^--$" > updated_project_list
cat $updated_project_list > $project_list

# remove SRA seq files
rm -r $raw_dir/
rm -r $qc_dir/

#------------------------------------------------------------------------------#
#### INPUT FILE LIST ####
#------------------------------------------------------------------------------#

done < $project_list  # done with current SRA BioProject
