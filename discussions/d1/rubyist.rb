class RubyIdentifier
  def initialize
	  @name_hash ={}
	  @haters = Array.new
  end

  # add : String, String ->
  # Records a person and whether they love Ruby
  def add(name, loves_ruby)
    bool = nil

    if loves_ruby == "Y"
	    bool = true
    end
    if loves_ruby == "N"
	    bool = false
    end
    if (@name_hash.has_key?(name) == false)
	    @name_hash[name] = bool
	    if (bool == false)
		    @haters.push(name)
	    end
    end
  end

  # ruby_haters : -> Array
  # Returns the people who don't love Ruby
  def ruby_haters()
    return @haters
  end

  # delete_the_haters : Array ->
  # Deletes people in the names array who don't like Ruby
  def delete_the_haters(names)
	  names.each{|x|
		  @name_hash.each{|k,v|
			  if (x == k && v == false)
				  @name_hash.delete(k)
			  end
		  }
	  }  


  end

  # get_everyone : -> Array
  # Returns an array of all the people
  def get_everyone()
	  everyone = (@name_hash.keys)
	  return everyone
  end
end
