class Node
  attr_accessor :val, :next_node

  def initialize(val, next_node)
    @val = val
    @next_node = next_node
  end
end

class LinkedList
  def initialize(elem)
    @head = Node.new(elem, nil)
  end

  # add : Object ->
  # Adds an elements to the linked list
  def add(elem)
    curr = @head 
      while (curr.next_node != nil)
        curr = curr.next_node
      end
      curr.next_node = Node.new(elem, nil)
  end

  # to_s : -> String
  # Returns a string representation of the linked list
  # (the format should be as irb would print it)
  def to_s	  
	curr = @head
	s = "["
	while curr.next_node != nil
		curr_val = curr.val.to_s
		s = s + curr_val + ", "
		curr = curr.next_node
	end
	curr_val = curr.val.to_s
	s = s + curr_val + "]"
  end
end
