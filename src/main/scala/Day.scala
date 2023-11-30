package de.mr_pine.aoc.y2023

trait Day[T]:
  def init(): T
  def part1(example: Boolean = false): String
  def part2(example: Boolean = false): String