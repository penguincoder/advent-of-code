#!/usr/bin/env ruby

grid = [ ['.', '#', '.'],
         ['.', '.', '#'],
         ['#', '#', '#'] ]

def string_to_grid(string)
  string.chomp.split("/").map do |row|
    row.split("")
  end
end

def get_permutations(grid, depth = 0)
  return [grid] if depth == 4
  flipped = grid.map(&:reverse)
  rotated = grid.transpose.map(&:reverse)
  flipped_and_rotated = rotated.map(&:reverse)
  [ grid, flipped, flipped_and_rotated ].concat(get_permutations(rotated, depth + 1)).uniq
end

def get_input_rules(filename)
  sum = {}
  File.readlines(filename).each do |line|
    source, dest = line.split(" => ")
    sgrid = string_to_grid(source)
    dgrid = string_to_grid(dest)
    get_permutations(sgrid).each do |perm|
      sum[perm] = dgrid
    end
  end
  sum
end

def break_grid_into(grid, size)
  results = []
  split = grid[0].size / size
  split.times do |i|
    grid[i * size, size].each_with_index do |row, index|
      split.times do |j|
        results[i] ||= []
        results[i][j] ||= []
        results[i][j] << row[j * size, size]
      end
    end
  end
  results
end

def recombine_grid(grid)
  results = []
  size = grid[0][0].size
  grid.each_with_index do |row, k|
    size.times do |i|
      row_index = size * k + i
      row.size.times do |j|
        results[row_index] ||= []
        results[row_index].concat(row[j][i])
      end
    end
  end
  results
end

def process_grid(grid, rules, depth = 5)
  if depth == 0
    grid.each { |row| puts row.join("") }
    puts grid.flatten.select { |x| x == "#" }.size
    return
  end
  new_size = grid[0].length.even? ? 2 : 3
  new_grid = break_grid_into(grid, new_size)
  mutated = new_grid.map do |row|
    row.map do |entry|
      if rules[entry].nil?
        rules.each { |k, v| puts "#{k} => #{v}" }
        raise "#{entry.inspect} has no match"
      else
        rules[entry]
      end
    end
  end
  mutated_grid = recombine_grid(mutated)
  process_grid(mutated_grid, rules, depth - 1)
end

process_grid(grid, get_input_rules('sample'), 2)
process_grid(grid, get_input_rules('problem'), 5)
process_grid(grid, get_input_rules('problem'), 18)
