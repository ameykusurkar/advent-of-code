input = STDIN.read.lines.map(&:chomp)

pointer = 50
at_zero = 0
crossing_zero = 0

input.each do |inp|
  dir = inp[0]
  full_amount = inp[1..].to_i

  while full_amount > 0
    amount = [full_amount, 99].min

    if dir == "L"
      dest = pointer - amount

      if pointer > 0 && dest <= 0
        crossing_zero += 1
      end
    else
      dest = pointer + amount

      if pointer > 0 && dest > 99
        crossing_zero += 1
      end
    end

    pointer = dest % 100
    full_amount -= amount
  end

  if pointer == 0
    at_zero += 1
  end
end

puts at_zero
puts crossing_zero
