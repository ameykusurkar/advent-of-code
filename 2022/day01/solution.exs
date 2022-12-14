defmodule Solution do
  def parse(input) do
    input |> String.split("\n\n") |> Enum.map(&parse_block/1)
  end

  def largest_blocks(parsed, n \\ 1) do
    parsed |> Enum.map(&Enum.sum/1) |> max_n(n) |> Enum.sum()
  end

  defp max_n(enumerable, n) do
    enumerable |> Enum.sort() |> Enum.reverse() |> Enum.take(n)
  end

  defp parse_block(input) do
    input |> String.split() |> Enum.map(&String.to_integer/1)
  end
end

[path] = System.argv()
IO.puts("File: #{path}")

input = File.read!(path)
blocks = Solution.parse(input)

result1 = blocks |> Solution.largest_blocks()
IO.puts(result1)

result2 = blocks |> Solution.largest_blocks(3)
IO.puts(result2)
