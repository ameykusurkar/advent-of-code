def repeating?(n)
  s = n.to_s

  divs = divisors(s.size)

  divs.any? do |d|
    repeating_slices?(s, d)
  end
end

def repeating_slices?(s, num_slices)
  return false unless s.size % num_slices == 0

  s.chars.each_slice(s.size / num_slices).to_a.uniq.size == 1
end

def divisors(n)
  sqrt_n = Math.sqrt(n).floor
  result = Set.new
  (1..sqrt_n).each do |i|
    if n % i == 0
      result << i
      result << n / i
    end
  end

  result.delete(1)
end

input = STDIN.read.split(",").map(&:strip)

result1 = input.sum do |line|
  start, finish = line.split("-").map(&:to_i)
  (start..finish).filter { |n| repeating_slices?(n.to_s, 2) }.sum
end

puts result1

result2 = input.sum do |line|
  start, finish = line.split("-").map(&:to_i)
  (start..finish).filter { |n| repeating?(n) }.sum
end

puts result2
