package de.mr_pine.aoc.y2023
package day10

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.boundary, boundary.break

object Day10 extends Day[Array[Array[Char]]] {
  override def init(example: Boolean): Array[Array[Char]] = {
    val fun = getInput(10, example).split("\n").map(_.toCharArray)
    fun
  }

  override def part1(example: Boolean): String = DFSFromTo(start = 'S', end = 'S', init(example)).toString

  override def part2(example: Boolean): String = "Hi"

  private def DFSFromTo(start: Char, end: Char, graph: Array[Array[Char]]): Int = {
    val startY = graph.indexWhere(_.contains(start))
    val startX = graph(startY).indexOf(start)
    val startPosition = Coordinates(startX, startY)
    val startNode = Node(startPosition, start, null)

    val nodeQueue = mutable.Queue[Node | LayerSeparator.type](startNode, LayerSeparator)
    val reached = ListBuffer[Node](startNode)
    var depth = 0;

    boundary {
      while (nodeQueue.nonEmpty) {
        val next = nodeQueue.dequeue()
        next match
          case LayerSeparator =>
            depth += 1
            nodeQueue.enqueue(LayerSeparator)
          case Node(neighbours, char, position, parent) =>
            for (neighbourCoords <- neighbours) {
              val neighbourChar = graph.applyOrElse(neighbourCoords.y, default = _ => Array[Char]()).applyOrElse(neighbourCoords.x, default = _ => '.')
              val neighbour = Node(neighbourCoords, neighbourChar, next.asInstanceOf[Node])
              if (neighbour.nextPositions.contains(position)) {

                if (!reached.contains(neighbour)) {
                  reached.append(neighbour)
                  nodeQueue.enqueue(neighbour)
                } else if (neighbour != parent && parent != null) {
                  boundary.break(depth + 1)
                }
              }
            }
      }
      -1
    }
  }
}

private case class Coordinates(x: Int, y: Int) {
  def north: Coordinates = Coordinates(x, y - 1)

  def south: Coordinates = Coordinates(x, y + 1)

  def east: Coordinates = Coordinates(x + 1, y)

  def west: Coordinates = Coordinates(x - 1, y)
}

private case class Node(nextPositions: Array[Coordinates], kind: Char, position: Coordinates, parent: Node) {
  override def equals(obj: Any): Boolean = obj match {
    case Node(_, _, otherPos, _) => otherPos == position
    case _ => false
  }
}

private object Node {
  def apply(coordinates: Coordinates, char: Char, parent: Node): Node = Node(nextPositions = char match {
    case '.' => Array[Coordinates]()
    case 'S' => Array(coordinates.north, coordinates.east, coordinates.south, coordinates.west)
    case '-' => Array(coordinates.east, coordinates.west)
    case '|' => Array(coordinates.north, coordinates.south)
    case 'L' => Array(coordinates.north, coordinates.east)
    case 'J' => Array(coordinates.north, coordinates.west)
    case '7' => Array(coordinates.south, coordinates.west)
    case 'F' => Array(coordinates.south, coordinates.east)
  },
    kind = char,
    position = coordinates,
    parent = parent
  )
}

private object LayerSeparator {}

