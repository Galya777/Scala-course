package homework1

class Homework1Test extends munit.FunSuite:

  test("fromDigits should convert digits to number in base 10") {
    assertEquals(fromDigits(List(1, 2, 3)), 123)
    assertEquals(fromDigits(List(0)), 0)
    assertEquals(fromDigits(List.empty), 0)
    assertEquals(fromDigits(List(9, 9, 9)), 999)
  }


    test("fromDigits should convert digits to number in given radix") {
    assertEquals(fromDigits(List(1, 12, 4), 16), 452) // 0x1C4 = 452
    assertEquals(fromDigits(List(1, 0, 1, 1), 2), 11) // binary 1011 = 11
    assertEquals(fromDigits(List(7, 7), 8), 63) // octal 77 = 63
  }

  test("parseInteger should parse decimal strings") {
    assertEquals(parseInteger("123"), 123)
    assertEquals(parseInteger("0"), 0)
    assertEquals(parseInteger("999"), 999)
  }

  test("parseInteger should parse hex strings") {
    assertEquals(parseInteger("1C4", 16), 452)
    assertEquals(parseInteger("FF", 16), 255)
    assertEquals(parseInteger("A", 16), 10)
  }

  test("parseInteger should parse binary strings") {
    assertEquals(parseInteger("0111001", 2), 57)
    assertEquals(parseInteger("-0111001", 2), -57)
  }

  test("parseInteger should handle negative numbers") {
    assertEquals(parseInteger("-123"), -123)
    assertEquals(parseInteger("-FF", 16), -255)
  }

  test("zipMap should combine two lists with function") {
    assertEquals(zipMap(List(1, 2, 3), List(4, 5, 6))(_ * _), List(4, 10, 18))
    assertEquals(zipMap(List(3, 6), List(20, 30, 40))((x, y) => y - x), List(17, 24))
    assertEquals(zipMap(List.empty[Int], List(1, 2, 3))(_ + _), List.empty)
    assertEquals(zipMap(List(1, 2, 3), List.empty[Int])(_ + _), List.empty)
  }

  test("zipMap should stop at shorter list length") {
    assertEquals(zipMap(List(1, 2, 3), List(10, 20))(_ + _), List(11, 22))
  }

  test("countCoinChangeVariants should count ways to make change") {
    assertEquals(countCoinChangeVariants(Set(1, 2, 5), 6), 5)
    assertEquals(countCoinChangeVariants(Set(1, 2), 4), 3) // 1+1+1+1, 1+1+2, 2+2
    assertEquals(countCoinChangeVariants(Set(1), 5), 1) // only 1+1+1+1+1
    assertEquals(countCoinChangeVariants(Set(2, 4), 7), 0) // impossible
    assertEquals(countCoinChangeVariants(Set(1, 2, 5), 0), 1) // one way to make 0 change
  }

  test("combinations should generate all n-element combinations") {
    assertEquals(combinations(List(1, 2, 3), 2), List(List(1, 2), List(1, 3), List(2, 3)))
    assertEquals(combinations(List(1, 2, 3), 0), List(List.empty))
    assertEquals(combinations(List(1, 2, 3), 4), List.empty) // n > list size
    assertEquals(combinations(List.empty[Int], 1), List.empty)
    assertEquals(combinations(List(1, 2, 3, 4, 5), 3).length, 10)
  }

  test("Queue should support push and pop") {
    val q = Queue.empty.push(1).push(2).push(3)
    val (head, rest) = q.pop
    assertEquals(head, 1)
    assertEquals(rest.pop._1, 2)
  }

  test("Queue should throw on pop when empty") {
    intercept[UnsupportedOperationException] {
      Queue.empty.pop
    }
  }

  test("Queue isEmpty should work") {
    assert(Queue.empty.isEmpty)
    assert(!Queue(1, 2, 3).isEmpty)
  }

  test("Queue size should work") {
    assertEquals(Queue.empty.size, 0)
    assertEquals(Queue(1, 2, 3).size, 3)
    assertEquals(Queue.empty.push(1).push(2).size, 2)
  }

  test("Queue apply should create queue from elements") {
    val q = Queue(3, 2, 1).push(0)
    assertEquals(q.size, 4)
    assertEquals(q.pop._1, 3)
  }

  test("bfsTraversal should perform BFS on graph") {
    val neighbours = (n: Int) => n match {
      case 1 => List(2, 5, 8)
      case 2 => List(1, 3, 6)
      case 3 => List(2, 4)
      case 4 => List(3)
      case 5 => List(6)
      case 6 => List(7)
      case 8 => List(9)
      case _ => List.empty
    }

    val result1 = bfsTraversal(neighbours)(1, 6)
    // Queue should contain: 1, 2, 5, 8, 3, 6
    assertEquals(result1.size, 6)
    assertEquals(result1.pop._1, 1)

    val result2 = bfsTraversal(neighbours)(4, 6)
    // Queue should contain: 4, 3, 2, 1, 6
    assertEquals(result2.size, 5)
  }

  test("bfsTraversal when start equals end") {
    val neighbours = (n: Int) => List.empty
    val result = bfsTraversal(neighbours)(5, 5)
    assertEquals(result.size, 1)
    assertEquals(result.pop._1, 5)
  }
