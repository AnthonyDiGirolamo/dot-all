#!/bin/bash

# List all active panes in this tab. For example:
#
#   0 /dev/pts/4 (active)
#   1 /dev/pts/7
#   2 /dev/pts/6
#   3 /dev/pts/5
#
tmux_list_panes () {
    tmux list-panes -F '#P #{pane_tty} #{?pane_active,(active),}'
}

# Get the current tmux pane in focus
tmux_get_active_pane_id () {
    tmux_list_panes | awk '/(active)/ { print $1}'
}

# Get the pane ID for a given TTY.
# Useful for performing an action on a pane not in focus.
tmux_get_pane_id () {
    tmux_list_panes | awk "\$0~\"${1}\" { print \$1}"
}

# Get the current pane TTY  for a given TTY
tmux_get_active_pane_tty () {
    tmux_list_panes | awk '/(active)/ { print $2}'
}

ORIGINAL_PANE=$(tmux_get_active_pane_id)

# Make a new split to the the right of the original pane
tmux split-window -h 'echo PANE_1; sleep 10'
# Focus is now in the new pane, save the ID
PANE_1_TTY=$(tmux_get_active_pane_tty)

# Switch focus to original pane
tmux select-pane -t $ORIGINAL_PANE

# Make a new split below the original pane
tmux split-window -v 'echo PANE_2; sleep 10'
# Focus is now in the new pane, save the ID
PANE_2_TTY=$(tmux_get_active_pane_tty)

# Switch focus to original pane
tmux select-pane -t $ORIGINAL_PANE

# Make a new split below the original pane
tmux split-window -v 'echo PANE_3; sleep 10'
PANE_3_TTY=$(tmux_get_active_pane_tty)

tmux select-pane -t $ORIGINAL_PANE

echo 'Opened panes:'
tmux_list_panes

echo Killing PANE_3 in 5 seconds...
sleep 5
# Kill a pane
tmux kill-pane -t $(tmux_get_pane_id $PANE_3_TTY)
