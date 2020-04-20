#!/usr/bin/env fish
set MIN 30
set SEC (math "60*$MIN")
xscreensaver-command -watch | while read -l event
  if string match -q -r "^UNBLANK.*\$" $event
    date
    echo "Unlocked! Work for $MIN min then lock."
    sleep $SEC
    xscreensaver-command -lock
  end
end
