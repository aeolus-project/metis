#! /bin/bash

# Run solvers on a given list of instances.

# Absolute path of the output file, which will contain a list of records of 
# the form: ...|...
RESULTS="results.log"

# Auxiliary files
TMP_PDDL='tmp_pddl_file'
TMP='tmp_file'
OUT='out_tmp_file'
ERR='err_tmp_file'

# Errors log file.
ERRORS='errors.log'

# Time-out
TIME_OUT=130

cmd_gen="python  ../test_generator/TestA.py"
cmd_plan="plan_metric-ff aeolus.pddl $TMP_PDDL"
#cmd_plan="plan_mp aeolus.pddl $TMP_PDDL"


# Lower the priority of the i/o operations.
renice -n 19 $$
ionice -c 3 -p $$

for i in {4..7..1} 
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
