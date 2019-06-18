require_relative "graph.rb"

class Synsets
  def initialize
    @synsets = {}
  end

  def get_keys()
    return @synsets.keys
  end

  def get_has_key?(key)
    return @synsets.has_key?(key)
  end

  def load(synset_file)
    invalid_lines = []
    file_keys = []
    line_num = 0
    f = File.open(synset_file, "r")
    #goes through each line in file and checks if it's valid. If not valid, adds to array
    f.each do |line|
      line_num += 1
      line =~ /^id:\s(\d+)\ssynset:\s([A-z]((,[A-z])?)+)+$/
      if (((line =~ /^id:\s(\d+)\ssynset:\s([A-z]((,[A-z])?)+)+$/)  == nil) || (@synsets.has_key?($1)) || file_keys.include?($1))
        invalid_lines.push(line_num)
      else
        file_keys.push($1)
      end
    end
    f.close
    #if there were invalid lines, returns array
    if (invalid_lines.size > 0)
      return invalid_lines
    end
    f = File.open(synset_file, "r")
    f.each do |line|
      line =~ /id:\s(\d+)\ssynset:\s([A-z]((,[A-z])?)+)+/
      if (!@synsets.has_key?($1))
        arr = $2.split(',')
        @synsets[$1] = []
        arr.each do |noun|
          (@synsets[$1]).push(noun)
        end
      end
    end
    f.close
    return nil
  end

  def addSet(synset_id, nouns)
    if (nouns.length == 0 || synset_id < 0 || @synsets.has_key?(synset_id))
      return false
    else
      @synsets[synset_id] = nouns
      return true
    end
  end

  def lookup(synset_id)
    if @synsets.has_key?(synset_id)
      puts "sysnets: #{@sysnsets}"
      puts "sysnet id : #{synset_id}"
      return @synsets[synset_id]
    else
      return []
    end
  end

  def findSynsets(to_find)
    synset_ids = []
    synset_ids_hash = {}
    if to_find.kind_of?(String)
      @synsets.keys.each do |key|
        if @synsets[key].include?(to_find)
          synset_ids.push(key.to_i)
        end
      end
      return synset_ids
    elsif to_find.kind_of?(Array)
      to_find.each do |noun|
        synset_ids_hash[noun] = findSynsets(noun)
      end
      return synset_ids_hash
    else
      return nil
    end
  end
end

class Hypernyms
  def initialize
    @hypernyms = {}
    @graph = Graph.new()
  end

  def get_graph()
    return @graph
  end

  def get_hypernyms()
    return @hypernyms
  end

  def load(hypernyms_file)
    invalid_lines = []
    line_num = 0
    f = File.open(hypernyms_file, "r")
    f.each do |line|
      line_num += 1
      if (line =~ /from:\s(\d+)\sto:\s([\d+]((,[\d+])?)+)+$/) == nil
        invalid_lines.push(line_num)
      end
    end
    f.close
    if (invalid_lines.size > 0)
      return invalid_lines
    end
    f = File.open(hypernyms_file, "r")
    f.each do |line|
      line =~/from:\s(\d+)\sto:\s([\d+]((,[\d+])?)+)+/
      if (@hypernyms.has_key?($1))
        if !@graph.hasVertex?($1)
          @graph.addVertex($1)
        end
        arr = $2.split(',')
        arr.each do |noun|
          if (!(@hypernyms[$1].include?(noun)))
            @hypernyms[$1].push(noun)
            if !@graph.hasVertex?(noun)
              @graph.addVertex(noun)
            end
            @graph.addEdge($1, noun)
          end
        end
      else
        @hypernyms[$1] = []
        if !@graph.hasVertex?($1)
          @graph.addVertex($1)
        end
        arr = $2.split(',')
        arr.each do |noun|
          @hypernyms[$1].push(noun)
          if !@graph.hasVertex?(noun)
            @graph.addVertex(noun)
          end
          @graph.addEdge($1, noun)
        end
      end
    end
    return nil
  end

  def addHypernym(source, destination)
    if (source < 0 || destination < 0 || source == destination)
      return false
    elsif (@hypernyms.has_key?(source))
      if !(@hypernyms[source].include?(destination))
        @hypernyms[source].push(destination)
        if (!@graph.hasVertex?(source))
          @graph.addVertex(source)
        end
        if (!@graph.hasVertex?(destination))
          @graph.addVertex(destination)
        end
        @graph.addEdge(source, destination)
      end
    else
      if !(@graph.hasVertex?(source))
        @graph.addVertex(source)
      end
      if !(@graph.hasVertex?(destination))
        @graph.addVertex(destination)
      end
      @graph.addEdge(source, destination)
      @hypernyms[source] = []
      @hypernyms[source].push(destination)
    end
    return true
  end

  def lca(id1, id2)
    if (!@hypernyms.has_key?(id1) || !@hypernyms.has_key?(id2))
      return nil
    end
    id1_hash = @graph.bfs(id1)
    id2_hash = @graph.bfs(id2)
    id1_ancestors = id1_hash.keys
    id2_ancestors = id2_hash.keys
    common_ancestors = id1_ancestors & id2_ancestors
    if common_ancestors.size == 0
      return []
    end
    common_ancestors_distance = {}
    common_ancestors.each do |ancestor|
      common_ancestors_distance[ancestor] = id1_hash[ancestor] + id2_hash[ancestor]
    end
    sorted_common_ancestors = common_ancestors_distance.sort_by {|ancestor, distance| distance}
    lowest_distance = (sorted_common_ancestors[0])[1]
    lowest_common_ancestors = []
    sorted_common_ancestors.each do |arr|
      if lowest_distance == arr[1]
        lowest_common_ancestors.push(arr[0])
      end
    end
    return lowest_common_ancestors
  end
end

class CommandParser
  def initialize
    @synsets = Synsets.new
    @hypernyms = Hypernyms.new
  end

  def parse(command)
    h = {}
    load_command = /\s*load\s([\w\d\/\\\\.\-\_]+)\s+([\w\d\/\\\\.\-\_]+)\s*/
    find_command = /\s*find\s+(\w+)\s*/
    find_many_command = /\s*findmany\s+([A-z]((,[A-z])?)+)+\s*/
    lookup_command = /\s*lookup\s+(\d+)\s*/
    lca_command = /\s*lca\s+(\d+)\s+(\d+)\s*/
    totally_valid = 0
    if (command =~ /\s*load\s*/)
      h[:recognized_command] = :load
      if ((command =~ /\s*load\s([\w\d\/\\\\.\-\_]+)\s+([\w\d\/\\\\.\-\_]+)\s+(\S+)/) != nil)
        h[:result] = :error
      elsif (command =~ load_command && @synsets.load($1) == nil && @hypernyms.load($2) == nil)
        vertices = @hypernyms.get_graph().vertices
        vertices.each do |vertex|
          if !(@synsets.get_has_key?(vertex))
            totally_valid -= 1
          end
        end
        h[:result] = true
        if (totally_valid < 0)
          h[:result] = false
        end
      else
        h[:result] = false
      end
    elsif (command =~ /\s*find\s*/)
      h[:recognized_command] = :find
      if ((command =~ /\s*find\s+(\w+)\s+(\S+)\s*/) != nil)
        h[:result] = :error
      elsif (command =~ find_command && @synsets.findSynsets($1) != nil)
        h[:result] = @synsets.findSynsets($1)
      else
        h[:result] = :error
      end

    elsif (command =~ /\s*findmany\s*/)
      h[:recognized_command] = :findmany
      if ((command =~ /\s*findmany\s+([A-z]((,[A-z])?)+)+\s+(\S+)\s*/) != nil)
        h[:result] = :error
      elsif (command =~ find_many_command)
        arr = $1.split(',')
        h[:result] = @synsets.findSynset(arr)
      else
        h[:result] = :error
      end

    elsif (command =~ /\s*lookup\s*/)
      h[:recognized_command] = :lookup
      if ((command =~ /\s*lookup\s+(\d+)\s+(\S+)\s*/) != nil)
        h[:result] = :error
      elsif (command =~ lookup_command)
        h[:result] = @synsets.lookup($1)
      else
        h[:result] = :error
      end

    elsif (command =~ /\s*lca\s*/)
      h[:recognized_command] = :lca
      if ((command =~ /\s*lca\s+(\d+)\s+(\d+)\s+(\S+)\s*/) != nil)
        h[:result] = :error
      elsif (command =~ lca_command)
        h[:result] = @hypernyms.lca($1, $2)
      else
        h[:result] = :error
      end

    else
      h[:recognized_command] = :invalid
    end
    return h
  end
end

