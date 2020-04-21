#!/usr/bin/env fish
set work_time 30
set work_time_sec (math "60*$work_time")
set break_time 5
set break_time_sec (math "60*$break_time")

set work_start_time 0
set break_start_time (date +%s)
set break_end_time 0
set locked_time 0
set unlocked_time 0

xscreensaver-command -lock

xscreensaver-command -watch | while read -l event
  echo "Read: $event"
  if string match -q -r "^UNBLANK.*\$" $event
    set unlocked_time (date +%s)
    set locked_duration (math "($unlocked_time-$locked_time)/60")
    echo
    echo "Screen was locked for $locked_duration min."

    if test $unlocked_time -ge $break_start_time # if the break has started
        if test $unlocked_time -ge $break_end_time # if break is over
            # break done, start new work time cycle
            set work_start_time (date +%s)
            set break_start_time (math $work_start_time+$work_time_sec)
            set break_end_time (math $break_start_time+$break_time_sec)
            echo "================================"
            date
            echo "Break complete!"
            echo "Work for $work_time min, then break for $break_time min."
            sleep $work_time_sec
            xscreensaver-command -lock
        else if test $unlocked_time -lt $break_end_time # if break is not over yet
            set min_till_break_done (math "($break_end_time-$unlocked_time)/60")
            echo "Break time left: $min_till_break_done"
            # relock the screen until break is over
            sleep 5
            xscreensaver-command -lock
        else
            echo "shouldn't get here?"
        end
    else # work time still going (no break yet)
        set min_till_break_start (math "($break_start_time-$unlocked_time)/60")
        echo "Work time left: $min_till_break_start"
        sleep (math "60*$min_till_break_start")
        xscreensaver-command -lock
    end

  else if string match -q -r "^LOCK.*\$" $event
    set locked_time (date +%s)
  end
end
