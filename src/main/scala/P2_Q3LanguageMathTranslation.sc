val chinese : List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english : List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

//Testing
go(List("yi", "nine", "six", "ba"))
go(List("yi", "josh", "three", "si"))

//Main Function
def go (aList : List[String]) = {
  var testlist = aList

  //Checks to make sure numbers are legal
  if (legalnumbers(testlist.head) && legalnumbers(testlist.tail.head) && legalnumbers(testlist.tail.tail.head)&& legalnumbers(testlist.tail.tail.tail.head) ){
    //A variable that stores the converted int number from the language
    var intlist: List[Int] = aList.map(language => toIntNumber(language))
    //Adds numbers in the list by foldRight function
    var addition = intlist.foldRight(0)(_ + _)
    //Multiplies numbers in the list by foldRight function
    var multiplication = intlist.foldRight(1)(_ * _)

    //Calls submethods
    print("Translation ")
    printNum(intlist)
    println()
    print("Addition ")
    numAddition(intlist)
    print(" = ")
    print(addition)
    println()
    print("Multiplication ")
    numMultiply(intlist)
    print(" = ")
    print(multiplication)
    println()
    println()
  }
  else {
    println("Dropping unrecognized number in list and continuing operation")
    var intlist: List[Int] = aList.map(language => toIntNumber(language))
    var filter : List[Int] = intlist filter (_ >= 0)
    //Adds numbers in the list by foldRight function
    var addition = filter.foldRight(0)(_ + _)
    //Multiplies numbers in the list by foldRight function
    var multiplication = filter.foldRight(1)(_ * _)

    //Calls submethods
    print("Translation ")
    printNum(filter)
    println()
    print("Addition ")
    numAddition(filter)
    print(" = ")
    print(addition)
    println()
    print("Multiplication ")
    numMultiply(filter)
    print(" = ")
    print(multiplication)
    println()
    println()
  }
}

def legalnumbers(a : String) : Boolean =
  a match {
    case "ling" | "zerp" => true
    case "yi" | "one" => true
    case "er" | "two" => true
    case "san" | "three" => true
    case "si" | "four" => true
    case "wu" | "five" => true
    case "liu" | "six" => true
    case "qi" | "seven" => true
    case "ba" | "eight" => true
    case "jiu" | "nine" => true
    case "shi" | "ten" => true
    case _ => false
  }

//Prints each number in the list followed by a space
def printNum(aList: List[Int]) : Unit = {
  if(!aList.isEmpty) {
    print(aList.head + " ")
    printNum(aList.tail)
  }
}

//Prints each number in the list followed by an addition sign (unless last in list)
def numAddition(aList: List[Int]): Unit = {
  if(!aList.isEmpty)
    if(aList.length != 1) {
      print(aList.head + " + ")
      numAddition(aList.tail)
    }
    else {
      print(aList.head)
      numAddition(aList.tail)
    }
}

//Prints each number in the list followed by a multiplication sign (unless last in list)
def numMultiply(aList: List[Int]) : Unit = {
  if(!aList.isEmpty)
    if(aList.length != 1) {
      print(aList.head + " * ")
      numMultiply(aList.tail)
    }
    else {
      print(aList.head)
      numMultiply(aList.tail)
    }
}

//Converts the language (either chinese or english) to an english integer number
def toIntNumber(language : String) : Int = {
  language match {
    case "ling" | "zero" => 0
    case "yi" | "one" => 1
    case "er" | "two" => 2
    case "san" | "three" => 3
    case "si" | "four" => 4
    case "wu" | "five" => 5
    case "liu" | "six" => 6
    case "qi" | "seven" => 7
    case "ba" | "eight" => 8
    case "jiu" | "nine" => 9
    case "shi" | "ten" => 10
    case _ => -999
  }
}