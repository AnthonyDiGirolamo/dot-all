#!/usr/bin/env ruby
# coding: utf-8
core_temps = []
temp_readings = []

`sensors`.lines.each do |line|
  if line =~ /^Core \d: *\+?([^ ]+?) .*$/
    core_temps << $1.strip.chomp
  end
  if line =~ /^temp1: *\+?(.*?)$/ ||
     line =~ /^Package id 0: *\+?([^ ]+?) .*$/
    temp_readings << $1.strip.chomp
  end
end
# readings.reject!(&:nil?)
# puts " " + readings.join(" ‖ ") + " "
# puts "╔══════════════════════════════╗"
# puts "╚══════════════════════════════╝"
# puts "🔥 CPU: #{temp_readings[1]} ⦗#{core_temps[0]}⦘ ⦗#{core_temps[0]}⦘  📶 WIFI: #{temp_readings[0]}"
puts "🔥 CPU: #{temp_readings[1]}  📶 WIFI: #{temp_readings[0]}"
