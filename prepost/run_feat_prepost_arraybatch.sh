#!/bin/bash
# Script written in May 2022 by Ceci Westbrook

# Batch options
# Inputs ----------------------------------
#SBATCH -J feat_prepost
#SBATCH --time=12:00:00
#SBATCH --cluster=smp
#SBATCH -N 1
#SBATCH --cpus-per-task=2
#SBATCH --array=13-13
# Outputs ----------------------------------
#SBATCH -o log/%x-%A-%a.out
#SBATCH -e log/%x-%A-%a.err
# ------------------------------------------

# creates an array of all subjects from directories in processed dir
subs=()
for dir in `ls -d /ix/cladouceur/westbrook-data/processed/*`; do
	rawname=`basename $dir`
	subject=`echo ${rawname} | grep -Eo '[0-9]{4}'`
	subs+=($subject)
done

# if running a subset
subs=(2050 2058 2074 2126 2237 2284 2317 2349 2424 2443)

# chooses a subset of #x subjects starting at index #n
n=$SLURM_ARRAY_TASK_ID # index of subject to start at
x=1 # number of subjects to run
uniq_subs=($(printf "%s\n" ${subs[@]} | sort -u))
subject="${uniq_subs[@]:${n}:${x}}"

fsfdir=/ix/cladouceur/westbrook-data/Scripts/prepost/prepost_fsf_files/Feb2024_newcontrast
filedir=/ix/cladouceur/westbrook-data/Scripts/prepost/make_fsfs/Feb2024_newcontrast
module load fsl

#for cope in cope1 cope2 cope3 cope4 cope5 cope6 cope7 cope8 cope9 cope10 cope11 cope12 cope13 cope14 cope15; do
for cope in cope14 cope15; do
	echo "running $subject $cope"
	# check if both pre and post copes exist
	for session in pre post ; do
		if [[ $subject == `grep -R $subject "$filedir/${cope}${session}run1run2.txt"` || $subject == `grep -R $subject "$filedir/${cope}${session}run1run1.txt"` || $subject == `grep -R $subject "$filedir/${cope}${session}run2run2.txt"` ]] ; then
		eval ${session}_ok=1
		echo "$session found"
		else
			eval ${session}_ok=0
			echo "$session not found"
		fi
	done

		# run feat if both pre and post are usable
		if [[ $pre_ok = 1 && $post_ok = 1 ]] ; then

				# Make directories if needed
				if [[ -d "/ix/cladouceur/westbrook-data/processed/sub-${subject}/prepost" ]] ; then
					mkdir "/ix/cladouceur/westbrook-data/processed/sub-${subject}/prepost"
				fi

				# Remove old feat directories if re-running
				if [[ -d "/ix/cladouceur/westbrook-data/processed/sub-${subject}/prepost/${cope}_nofixation_newcontrast.gfeat" ]] ; then
				echo "found old feat"
				rm -r /ix/cladouceur/westbrook-data/processed/sub-${subject}/prepost/${cope}_nofixation_newcontrast*.gfeat
				fi

			for featfile in `ls "$fsfdir/sub-${subject}_prepost_${cope}.fsf"` ; do
				feat $featfile
			done
		else
			echo "no usable prepost feat"
		fi

done
