flags="-I100 -S -H100 -R100"
os="20 40 80 160 320"
cs="1 2 4 8 16 24"

for flag in $flags; do
    for o in $os; do
        for c in $cs; do
            mkdir run-$flag-$o-$c
            cd run-$flag-$o-$c
            echo ln -s ../*.gcode
            echo gun $flag -O$o -C$c $1 2\> errorLog.txt \> runLog.txt
            cd ..
            echo
        done
    done
done

#           echo -n "$flag $o $c " \>\> ../total-results.data
#           echo -n "Process"      \>\> iterations.data
#           echo -n "sedspace"     \>\> ../total-results.data
#           echo                   \>\> ../total-results.data
