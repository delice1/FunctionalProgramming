//Warmup 1- Method to test if number is prime
def prime (x : Int) : Boolean = {
  //1 cannot be a prime number
  if (x == 1){
    false}
  //Checks if number is prime
  else {
    (2 until x) forall (x % _ != 0)
  }
}
//Test
prime(5)
prime(10)

//Warmup 2- Method to check if the difference between 2 prime numbers is exacly 2
def twinprimes(x : Int, y: Int) : Boolean = {
  //Checks if both values are prime
  if (prime(x) && prime(y)) {
    //if first number is greater than second number, check difference
    if (x > y) {
      x - y == 2
    }
    //if second number is greater than first number, check difference
    else {
      y - x == 2
    }
  }
  else {
    false
  }
}
//Test
twinprimes(41,43)
twinprimes(41,47)

//Warmup 3- Returns list of integers of all twin primes starting up to n
def twinprimeslist(n : Int) : List[Int] = {
  n match {
    case n if (n < 3) => Nil //Empty list if integer if integer less than 3
    case _ => if (twinprimes(n-2, n) == true || twinprimes(n+2, n)== true) n :: twinprimeslist(n-1) //Returns list of twin primes
              else twinprimeslist(n-1)
  }
}
//Test
twinprimeslist(50)

//Warmup 4- Prints solution satisfying Goldbach Conjecture
def goldbach (x : Int) = {
  //Integer must be even
  if (x % 2 != 0){
    println("Integer must be even")
  }
  //Integer needs to be greater than 2
  else if (x <= 2){
    println("Integer must be greater than 2")
  }
  //If above conditions apply, do Goldbach Conjecture
  else {
    for (i <- 3 until x / 2){
      if (prime(i) && prime(x - i)){
        println(i + " and " + (x-i))
      }
    }
  }
}
//Test
goldbach(28)