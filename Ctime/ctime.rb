#!/usr/bin/env ruby
## Ruby program that takes as an input the output of last, and produces a report
## summarizing the total connect time for each user with a connect time

# adds two time strings together
def add(time1, time2)
  # initialize and set indexes
  plusloc1 = time1.index("+")
  plusloc2 = time2.index("+")
  cloc1 = time1.index(":")
  cloc2 = time2.index(":")
  days = 0
  hours = 0

  # conditional statements dealing with if there is a day or not
  # gets raw number of days, hours, and minutes
  if not plusloc1.nil?
    then days = time1.slice(0, plusloc1).to_i and
         hours = time1.slice(plusloc1 + 1, 2).to_i
  else hours = time1.slice(0, 2).to_i end

  if not plusloc2.nil?
    then days += time2.slice(0, plusloc2).to_i and
         hours += time2.slice(plusloc2 + 1, 2).to_i
  else hours += time2.slice(0, 2).to_i end

  mins = time1.slice(cloc1 + 1, 2).to_i + time2.slice(cloc2 + 1, 2).to_i

  # convert excess minutes to hours, excess hours to days
  strmins = (mins % 60).to_s
  if strmins.to_i < 10 then strmins = "0" + strmins end

  strhours = ((hours + mins.div(60)) % 24).to_s
  if strhours.to_i < 10 then strhours = "0" + strhours end

  strdays = (days + (hours + mins.div(60)).div(24)).to_s

  # prints everything nicely back to how the input was formatted
  if strdays.to_i == 0 then strhours + ":" + strmins
  else
    strdays + "+" + strhours + ":" + strmins end
end

# Initialize variables
myfile = ARGF
data = myfile.read
hash = Hash.new

# while loop that finds all the matches in the input, and adds their times
# to a hash table, adding times if the username already exists in the table
while true  # exit in middle of loop
  match = /(\w+)(.*|\n)\((\d?)\+?(\d\d):(\d\d)/.match(data)
  if not match;   break;   end  # exit when match fails

  if match[1] == "reboot" #deals with edge case that isn't a username
    then match = /(\w+)(.*|\n)\((\d?)\+?(\d\d):(\d\d)/.match(match.post_match) end

  # format the match so it can be added to other matches and the table nicely
  name = match[1]
  if match[3].to_i > 0 then
  time = match[3] + "+" + match[4] + ":" + match[5] else
  time = match[4] + ":" + match[5] end

  # add appropriate data to the table
  if hash.key?(name)
  then hash[name] = add(time, hash[name])
  else hash[name] = time end

  # next increment
  data = match.post_match
end

# prints everything nicely out!
hash.sort.each { |name, time| puts name + "   " + " " * (16 - name.length - time.length) + time}
