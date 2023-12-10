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

  override def part1(example: Boolean): String = (DFSFromTo(start = 'S', end = 'S', init(example)).length / 2).toString

  override def part2(example: Boolean): String = {


    val board = init(example)
    val loop = DFSFromTo('S', 'S', board)/*.toSet*/.map(_.position)
    var insideCount = 0
    for (y <- Range(0, board.length)) {
      var inside = false
      var inLoop = 0
      for (x <- Range(0, board(y).length)) {
        if (loop.contains(Coordinates(x,y))) {
          var char = board(y)(x)
          if (char == 'S') then char = sReplacement

          char match
            case 'F' => inLoop = -1
            case 'L' => inLoop = 1
            case '7' => if inLoop == 1 then inside = !inside
            case 'J' => if inLoop == -1 then inside = !inside
            case '|' => inside = !inside
            case _ => {}
        } else if (inside) {
          insideCount += 1
          inLoop = 0
        }
      }
    }

    def sReplacement: Char = {
      val sCoord = loop.head
      val neighbours = Set(loop(1), loop.last)
      val chars = "7LFJ-|"
      val replacement = chars.find(char => {
        Node(sCoord, char, null).nextPositions.toSet == neighbours
      })
      replacement.get
    }

    insideCount.toString
  }

  private def DFSFromTo(start: Char, end: Char, graph: Array[Array[Char]]): List[Node] = {
    val startY = graph.indexWhere(_.contains(start))
    val startX = graph(startY).indexOf(start)
    val startPosition = Coordinates(startX, startY)
    val startNode = Node(startPosition, start, null)

    val nodeQueue = mutable.Queue[Node | LayerSeparator.type](startNode, LayerSeparator)
    val reached = mutable.Set[Node](startNode)
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
                  reached.add(neighbour)
                  nodeQueue.enqueue(neighbour)
                } else if (neighbour != parent && parent != null) {
                  val myChain = next.asInstanceOf[Node].parentChain.dropRight(1).result()
                  val reachedNeighbour = reached.find(_ == neighbour)
                  val neighbourChain = reachedNeighbour.get.parentChain.reverse
                  boundary.break(neighbourChain.prependToList(myChain))
                }
              }
            }
      }
      List[Node]()
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

  override def hashCode(): Int = position.hashCode()

  def parentChain: ListBuffer[Node] = {
    val path = ListBuffer[Node]()
    var node = this
    while (node != null) {
      path.append(node)
      node = node.parent
    }

    path
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

