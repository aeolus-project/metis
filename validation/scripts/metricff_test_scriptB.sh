#! /bin/bash

# Run solvers on a given list of instances.

# Absolute path of the input file, which contains the jobs to be executed; 
# each job has the form: solver_name|instance_qualified_name

# Absolute path of the output file, which will contain a list of records of 
# the form: solver|instance|host_name|return code|info|time.
RESULTS="./results/metricff_testB_results.log"

# Auxiliary files
TMP_PDDL='tmp_pddl_file'
TMP='tmp_file'
OUT='out_tmp_file'
ERR='err_tmp_file'

# Errors log file.
ERRORS='errors.log'

# Time-out
TIME_OUT=130

cmd_gen="python  ../encoding/generate_pddl_testB.py"
cmd_plan="../tools/plan_metric-ff -o ../encoding/aeolus.pddl -f $TMP_PDDL"
#cmd_plan="../tools/plan_mp ../encoding/aeolus.pddl $TMP_PDDL"


# Lower the priority of the i/o operations.
#renice -n 19 $$
#ionice -c 3 -p $$

#for i in {5..65..10} 
for i in {3..5..1}
do
  cmd_gen_aux="$cmd_gen $i"
  echo "generate file ($cmd_gen_aux)"
  $cmd_gen_aux > $TMP_PDDL
  echo "solving ($cmd_plan)"
  time -p (timeout $TIME_OUT $cmd_plan 1>$OUT 2>$ERR) 2>$TMP
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
