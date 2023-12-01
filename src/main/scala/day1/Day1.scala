package de.mr_pine.aoc.y2023
package day1

object Day1 extends Day[() => Int]{
  override def init(example: Boolean): () => Int = {
    val fun = getInput(1, example).map {
      case '(' => up
      case ')' => down
    }.fold(neutral)((o1, o2) => o1.andThen(o2))
    return () => fun(0)
  }

  override def part1(example: Boolean): String = init(example)().toString

  override def part2(example: Boolean): String = ???
}

def neutral = (_: Int) + 0
def up = (_: Int) + 1
def down = (_: Int) - 1