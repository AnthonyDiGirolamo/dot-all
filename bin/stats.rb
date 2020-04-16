#!/usr/bin/env ruby
require 'pp'
folders = %W[app spec features config bin lib]
puts "Folders: " + folders.join(' ')

commit_stats = (`git shortlog -sne -- #{folders.join(' ')}`).split("\n").each_with_object({}) do |line, hash|
  count, name = line.strip.split("\t")
  if name =~ /.* (\S+) <(\S+)>/
    last_name = $1
    email = $2
  end
  hash[last_name] = {commit_count: count, commits: []}
end

puts 'Name'.ljust(20) + "Commits".ljust(15) + "Additions".ljust(15) + "Deletions".ljust(15) + "Average Changes Per Commit".ljust(28) + "Percent Contribution"

commit_stats.keys.each do |name|
  `git log --all --use-mailmap --author="#{name}" --pretty=tformat: --numstat -- #{folders.join(' ')}`.split("\n\n\n").each do |commit|
    commit_stats[name][:commits] << commit.split("\n").each_with_object([0, 0]) do |line, s|
      additions, deletions, file = line.split("\t")
      s[0] += additions.to_i
      s[1] += deletions.to_i
    end
  end
  commit_count = commit_stats[name][:commits].size || 0
  additions = commit_stats[name][:commits].collect{|additions, deletions| additions }.reduce(:+) || 0
  deletions = commit_stats[name][:commits].collect{|additions, deletions| deletions }.reduce(:+) || 0
  commit_stats[name]["additions"] = additions
  commit_stats[name]["deletions"] = deletions
  commit_stats[name]["changes"] = additions + deletions
  average = (additions.to_f + deletions.to_f) / commit_count.to_f || 0
  commit_stats[name]["average"] = average
end

total_changes = commit_stats.collect{|name, stats| stats["changes"]}.reduce(:+)

commit_stats.keys.each do |name|
  commit_count = commit_stats[name][:commits].size || 0
  additions = commit_stats[name]["additions"]
  deletions = commit_stats[name]["deletions"]
  average = commit_stats[name]["average"]
  contribution_percent = 100.0 * (commit_stats[name]["changes"].to_f / total_changes.to_f)
  puts "#{(name.nil? ? "" : name).ljust(20)}#{commit_count.to_s.ljust(15)}#{additions.to_s.ljust(15)}#{deletions.to_s.ljust(15)}#{average.round(2).to_s.ljust(28)}#{contribution_percent.round(2).to_s}%"
end


