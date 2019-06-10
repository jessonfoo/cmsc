# Fill in these regular expressions
ID = /\d{2,5}/
NAME = /([A-Z]\w*) ([A-Z]\w*)/
HOMETOWN = /[A-Z]\w*/
DOB = /(\d{2}-\d{2})-(\d{4})/
EMAIL = /\w+@\w+\.([A-z]{3})/

# This is the regular expression for a single record
RECORD = /^#{ID},#{NAME},#{HOMETOWN},#{DOB},#{EMAIL}$/

class ApertureScheduler

	# phone? : String -> Boolean
	# Determines if a phone number is valid
	def self.phone?(number)
		if number =~ /\(\d{3}\) \d{3}-\d{4}/
			return true
		elsif number =~ /\d{3}-\d{3}-\d{4}/
			return true
		else
			return false
		end
	end

	# sanitize : Array -> Array
	# Returns non-corrupt employee records
	def self.sanitize(records)
		arr_of_recs = []
		records.each do |line|
			if line =~ RECORD
				arr_of_recs.push(line)
			end
		end
		return arr_of_recs
	end

	# schedule : Array -> Array
	# Returns schedule of newly formatted employee records
	def self.schedule(records)
		arr = []
		arr = sanitize(records)
		proper = []
		arr.each do |line|
			line =~ RECORD
			name = $2 + " " + $1
			line_arr = line.split(',')
			year = $4
			month_day = $3
			year_month_day = $4 + "-" + $3
			line_arr[3] = year_month_day
			line_arr[1] = name
			line = line_arr.join(',')
			proper.push(line)
		end
		proper.map! do |line|
			line.split(',')
		end
		proper.sort_by! { |line|  line[1]}
		proper.map! do |line|
			line.join(',')
		end 
	end
end
