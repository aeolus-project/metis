#! /bin/bash

# Run solvers on a given list of instances.

# Absolute path of the output file, which will contain a list of records of 
# the form: ...|...
RESULTS="./results/metis_testA_dupl_results_NEW.log"

# Auxiliary files
TMP_PDDL='tmp_pddl_file'
TMP_RESULTS='tmp_results_file'
TMP='tmp_file'
OUT='out_tmp_file'
ERR='err_tmp_file'

# Errors log file.
ERRORS='errors_testA_dupl.log'

# Time-out
TIME_OUT=130

cmd_gen="python ./TestA_json_duplication.py"
cmd_plan="../../metis.native"

# Lower the priority of the i/o operations.
#renice -n 19 $$
#ionice -c 3 -p $$

#for i in {25..575..25} 
#for i in {5..15..5} 
#for i in {5..235..10} 
for i in {5..150..10} 
do
  cmd_gen_aux="$cmd_gen -s $i -o $TMP_PDDL"
  last_state=$(expr $i - 1)
  cmd_plan_aux="$cmd_plan -u $TMP_PDDL -c B -s q$last_state -o $TMP_RESULTS"
  echo "generate file ($cmd_gen_aux)"
  $cmd_gen_aux > $TMP_PDDL
  echo "solving ($cmd_plan_aux)"
  time -p (timeout $TIME_OUT $cmd_plan_aux 1>$OUT 2>$ERR) 2>$TMP
  ret=$?
  time_real=`cat $TMP | awk -v s=0.0 'NR==1 {print $2}'`
  time=`cat $TMP | awk -v s=0.0 'NR > 1 {s += $2} END {print s}'`
  if 
    [ $ret -ne 0 ]
  then
    if
      [ $ret -eq 124 ]
    then
      # If time-out expires.
      echo "|$i|124|timeout|$time|$time_real|" >> $RESULTS
    else
      # An error occurs
      echo "|$i|$ret|error|$time|$time_real|" >> $RESULTS
      echo "******** ERROR in processing $cmd_gen_aux ********" >> $ERRORS
      cat $ERR >> $ERRORS
    fi
  else
    echo "|$i|0|ok|$time|$time_real|" >> $RESULTS
  fi
done

rm $TMP_PDDL
rm $TMP
rm $OUT
rm $ERR
