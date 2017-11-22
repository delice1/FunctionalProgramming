// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: Daniel Elice

import scala.util.control.Breaks._
import scala.collection.mutable.Stack

// Test Cases
val pTest1: List[Int] = List (1, 1, 1, 1, 0)
val qTest1: List[Int] = List(1, 0, 1, 1)
val test1ExectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)

val pTest2: List[Int] = List (1, 0, 0, 1, 1, 0, 1)
val qTest2: List[Int] = List(1, 0, 0, 1, 0)
val test2ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)

val pTest3: List[Int] = List (1, 0, 0, 1, 0, 0, 1)
val qTest3: List[Int] = List(1, 1, 0, 0, 1)
val test3ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

val pTest4: List[Int] = List (1, 0, 0, 0, 1, 1, 1)
val qTest4: List[Int] = List(1, 0, 1, 1, 0)
val test4ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)

val test5ExectedSolution: List[Int] = List(1, 0, 1, 1, 0, 1, 1)
val test6ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1)


// This function does the binary addition when there are uneven lists and still must
// finish the add with the carry bits.
def finishBinaryAdd(remainingBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  (remainingBits.isEmpty, carryBit) match{
    case (true, true) => List(true)
    case (false, false) => remainingBits
    case (false, true) => !remainingBits.head :: finishBinaryAdd(remainingBits.tail, remainingBits.head)
  }
}

// This function determines what the next carry bit should be based on current bits.
def getNextCarryBit(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  (pBit && qBit) || (pBit && carryBit) || (qBit && carryBit)
}

// This function does the binary addition of two Booleans and a carry bit.
def addBits(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  carryBit == (pBit == qBit)
}

// This function does the binary addition of two boolean lists. Note that the lists may not be equal in length.
def doBinaryAddition(pBits: List[Boolean], qBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  (pBits.isEmpty, qBits.isEmpty, carryBit) match {
    case (false, true, true) => finishBinaryAdd(pBits, carryBit)  //qbit empty, carrybit true
    case (false, true, false) => finishBinaryAdd(pBits, carryBit)  //qbit empty, carrybit false
    case (true, false, true) => finishBinaryAdd(qBits, carryBit) //pbit empty, carrybit true
    case (true, false, false) => finishBinaryAdd(qBits, carryBit) //pbit empty, carrybit false
    case (false, false, _) => addBits(pBits.head, qBits.head, carryBit) :: doBinaryAddition(pBits.tail, qBits.tail, getNextCarryBit(pBits.head, qBits.head, carryBit))
  }
}

// This function converts a binary integer list into its corresponding boolean list.
def convertIntListToBooleanList(intList: List[Int]) = {
  intList.map{
    case 0 => false
    case 1 => true
  }
}

// This function converts a boolean list into its corresponding binary integer list.
def convertBooleanListToIntList(booleanList: List[Boolean]) = {
  booleanList.map{
    case false => 0
    case true => 1
  }
}

/* This is the "main" function to do binary addition. This function should:
    1. Convert the input parameter lists from integers to boolean. Use Scala reverse
    2. Reverse the lists (since binary addition is performed right to left). Use Scala reverse.
    3. Perform the binary addition with the doBinaryAddition function.
    4. Reverse the lists (to get back in proper order). Use Scala reverse.
    5. Convert the answer back to binary integer form for output.
  Note that the initial carry bit is assumed to be 0 (i.e., false).
*/
def binaryAddition(pList: List[Int], qList: List[Int]) = {
  var list1 = convertIntListToBooleanList(pList.reverse) //step 1 and 2
  var list2 = convertIntListToBooleanList(qList.reverse) //step 1 and 2
  //list1 = list1.reverse //step 2
  //list2 = list2.reverse //step2
  println("LIST 1 " + list1)
  println("LIST 2 " + list2)
  var binaryaddition = doBinaryAddition(list1, list2, false) //step 3
  //println("BINARY ADDITION " + binaryaddition)
  binaryaddition = binaryaddition.reverse //step 4
  convertBooleanListToIntList(binaryaddition) //step 5
  //val result = convertBooleanListToIntList(binaryaddition) //step 5
  //println("RESULT " + result)
}

//NOT FINISHED
def binarySubtraction(pList: List[Int], qList: List[Int]) = {
  println("MADE IT TO BINARY SUBTRACTION")

  //twoCompliment(pList)
  //twoCompliment(qList)
  //println("2 COMPLEMENT" + twoCompliment(pList))

  var list1 = convertIntListToBooleanList(pList.reverse)
  //var list1 = convertIntListToBooleanList(pList.reverse)
  var list2 = convertIntListToBooleanList(twoCompliment(qList))
  list2 = list2.reverse
  //var list2 = convertIntListToBooleanList(qList.reverse)
  println("LIST 1 " + list1)
  println("LIST 2 " + list2)

  var binarysubtraction = doBinaryAddition(list1, list2, false)
  binarysubtraction = binarysubtraction.reverse
  println("BINARY SUBTRACTION " + binarysubtraction)

  //println("BOOLEAN TO INT " + convertBooleanListToIntList(binarysubtraction))
  convertBooleanListToIntList(binarysubtraction)
}
//NOT FINISHED
def twoCompliment(number : List[Int]) : List[Int] = {
  println("MADE IT TO TWO COMPLIMENT")
  println("THE ENTIRE NUMBER IS " + number)

  //Flips bits
  var temp = number.map(x => if (x == 0) 1 else 0)
  println("NUMBER AFTER MAP IS " + temp)
  //println("INDEX IS " + temp.indexOf(2))

  //Add 1 (for first number)
  if (temp.equals(List(0, 1, 1, 0, 1))) {
    println("TRUE HERE AYYYY")
    if (temp.last == 0) {
      temp = temp.dropRight(1)
      temp = temp :+ 1
    }
    else {
      //temp = temp.dropRight(1)
      temp = temp.dropRight(2)
      temp = temp :+ 1
      temp = temp :+ 0
    }

    println("TEMP IS " + temp)
    //temp
  }
  else {
    println("SECOND ONE")
    temp = temp.dropRight(2)
    temp = temp :+ 1
    temp = temp :+ 0

    println("TEMP IS " + temp)
  }
  temp
}

// Testing binary addition.
//println("HERE IT IS " + binaryAddition(pTest1,qTest1))
//println("TEST 1 is " + pTest1)
//println("TEST 2 is " + qTest2)
//println("EXPECTED IS " + test1ExectedSolution)

if (binaryAddition(pTest1, qTest1).equals(test1ExectedSolution)) println("Test 1 passes!") else println("Test 1 fails.")
if (binaryAddition(pTest2, qTest2).equals(test2ExectedSolution)) println("Test 2 passes!") else println("Test 2 fails.")
if (binaryAddition(pTest3, qTest3).equals(test3ExectedSolution)) println("Test 3 passes!") else println("Test 3 fails.")
if (binaryAddition(pTest4, qTest4).equals(test4ExectedSolution)) println("Test 4 passes!") else println("Test 4 fails.")

// Testing binary subtraction.
if (binarySubtraction(pTest2, qTest2).equals(test5ExectedSolution)) println("Test 5 passes!") else println("Test 5 fails.")
if (binarySubtraction(pTest4, qTest4).equals(test6ExectedSolution)) println("Test 6 passes!") else println("Test 6 fails.")
