#!/usr/bin/env fish

if not pacman -Qs smartmontools >/dev/null
  sudo pacman -S smatmontools
end

set disks /dev/sda /dev/sdb /dev/sdc /dev/sdd

function read_selftest
  for d in $disks
    sudo smartctl -l selftest $d
  end
end

function start_short_test
  for d in $disks
    sudo smartctl -t short $d
  end

end

function start_long_test
  for d in $disks
    sudo smartctl -t long $d
  end
end

if string match -q 'shortwait' $argv[1]
  start_short_test
  echo 'Waiting...'
  sleep 65
  read_selftest
else if string match -q 'short' $argv[1]
  start_short_test
else if string match -q 'long' $argv[1]
  echo start_long_test
else
  read_selftest
end

