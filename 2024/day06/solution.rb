def find_guard(grid)
  grid.each_with_index.map do |y, j|
    y.each_with_index.map do |x, i|
      case x
      when "^" then return [j, i], [-1, 0]
      when ">" then return [j, i], [0, 1]
      when "v" then return [j, i], [1, 0]
      when "<" then return [j, i], [0, -1]
      end
    end
  end
end

def rotate(dir)
  y, x = dir
  [x, -y]
end

input = File.read("input.txt")
grid = input.lines.map(&:chomp).map(&:chars)

pos, dir = find_guard(grid)
places = Set.new

loop do
  places << pos
  new_pos = pos[0] + dir[0], pos[1] + dir[1]

  if new_pos[0] < 0 || new_pos[0] >= grid.size || new_pos[1] < 0 || new_pos[1] >= grid[0].size
    break
  end

  if grid[new_pos[0]][new_pos[1]] == "#"
    dir = rotate(dir)
  else
    pos = new_pos
  end
end

puts places.size
