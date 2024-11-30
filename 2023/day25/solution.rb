require "ruby-graphviz"
require "pry"

INPUT = File.read("input.txt")

def parse(input)
  parsed = input.lines.map(&:chomp)

  parsed.to_h do |line|
    src, dests = line.split(":").map(&:strip)
    [src, dests.split]
  end
end

def all_nodes(adj_list)
  (adj_list.keys + adj_list.values.flatten).uniq
end

def contract!(adj)
  u, v = nil, nil

  until u && v
    u = adj.keys.sample
    v = adj[u].sample
  end

  adj[u].delete(v)
  uv = [u, v].join("-")

  u_list = adj.delete(u)
  v_list = adj.delete(v) || []
  uv_list = (u_list + v_list)

  if uv_list.any?
    adj[uv] = uv_list
  end

  adj.transform_values! do |list|
    list.map { |n| [u, v].include?(n) ? uv : n }
  end

  adj.keys.each do |k|
    adj[k].delete(k)
  end
end

def build_graph(adj_list)
  g = GraphViz.new(:G, type: :digraph)

  adj_list.each do |n, verts|
    verts.each do |v|
      g.add_edges(n, v, dir: :none)
    end
  end

  g.output(png: "output.png")
end

loop do
  adj_list = parse(INPUT)

  while all_nodes(adj_list).size > 2
    contract!(adj_list)
    # pp adj_list
    # puts "\n"
  end

  c = adj_list.values.sum(&:size)

  if c == 3
    puts "Done!"
    build_graph(adj_list)
    ns = all_nodes(adj_list).map { |x| x.split("-") }
    puts ns[0].size * ns[1].size
    break
  end

  puts c
end
