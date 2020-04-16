#!/usr/bin/env ruby
# coding: utf-8
fan_readings = []
temp_readings = []

`sensors`.lines.each do |line|
  if line =~ /(\d+ RPM)/
    fan_readings << $1.strip.chomp
  end
  if line =~ /^CPU: *(.*?)$/ ||
     line =~ /^Other: *(.*?)$/
    temp_readings << $1.strip.chomp
  end
end
# readings.reject!(&:nil?)
# puts " " + readings.join(" ‖ ") + " "
puts "<txt><span fgcolor='White'>CPU  #{fan_readings[0]}  #{temp_readings[0]}"
puts "GPU  #{fan_readings[1]}  #{temp_readings[1]}</span></txt>"
