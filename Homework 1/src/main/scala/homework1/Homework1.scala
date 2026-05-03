package homework1

import scala.annotation.tailrec

def fromDigits(digits: List[Int], radix: Int = 10): Int =
  digits.reverse.zipWithIndex.foldLeft(0) { case (acc, (digit, index)) =>
    acc + digit * math.pow(radix, index).toInt
  }

def parseInteger(integer: String, radix: Int = 10): Int =
  val isNegative = integer.startsWith("-")
  val digits = if isNegative then integer.drop(1) else integer

  val result = digits.foldLeft(0) { (acc, char) =>
    val digitValue = char match
      case c if c.isDigit => c - '0'
      case c if c.isUpper => c - 'A' + 10
      case c if c.isLower => c - 'a' + 10
      case _              => throw new IllegalArgumentException(s"Invalid character: $char")
    acc * radix + digitValue
  }

  if isNegative then -result else result

def zipMap[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
  a.zip(b).map { case (x, y) => f(x, y) }

def countCoinChangeVariants(denominations: Set[Int], change: Int): Int =
  if change < 0 then 0
  else if change == 0 then 1
  else if denominations.isEmpty then 0
  else
    val firstDenomination = denominations.head
    val remainingDenominations = denominations.tail
    countCoinChangeVariants(denominations, change - firstDenomination) +
      countCoinChangeVariants(remainingDenominations, change)

def combinations[A](xs: List[A], n: Int): List[List[A]] =
  if n == 0 then List(List.empty)
  else if n < 0 || xs.isEmpty then List.empty
  else
    xs.tails.flatMap {
      case Nil => Nil
      case head :: tail =>
        combinations(tail, n - 1).map(head :: _)
    }.toList

class Queue[A] private (front: List[A], rear: List[A], sizeValue: Int):
  def push(a: A): Queue[A] =
    new Queue(front, a :: rear, sizeValue + 1)

  def pop: (A, Queue[A]) =
    front match
      case head :: tail => (head, new Queue(tail, rear, sizeValue - 1))
      case Nil =>
        rear.reverse match
          case head :: tail => (head, new Queue(tail, Nil, sizeValue - 1))
          case Nil          => throw new UnsupportedOperationException("Queue is empty")

  def isEmpty: Boolean = sizeValue == 0

  def size: Int = sizeValue

  override def toString: String =
    val all = front ++ rear.reverse
    s"Queue(${all.mkString(", ")})"

object Queue:
  def empty[A]: Queue[A] = new Queue(Nil, Nil, 0)

  def apply[A](xs: A*): Queue[A] =
    new Queue(xs.toList, Nil, xs.length)

def bfsTraversal(neighbours: Int => List[Int])(start: Int, end: Int): Queue[Int] =
  @tailrec
  def bfs(queue: Queue[Int], visited: Set[Int], result: Queue[Int]): Queue[Int] =
    if queue.isEmpty then result
    else
      val (current, newQueue) = queue.pop
      if current == end then result.push(current)
      else if visited.contains(current) then bfs(newQueue, visited, result)
      else
        val currentNeighbours = neighbours(current).filterNot(visited.contains)
        val nextQueue = currentNeighbours.foldLeft(newQueue)(_.push(_))
        val nextVisited = visited + current
        val nextResult = result.push(current)
        bfs(nextQueue, nextVisited, nextResult)

  if start == end then Queue(start)
  else bfs(Queue(start), Set.empty, Queue.empty)
