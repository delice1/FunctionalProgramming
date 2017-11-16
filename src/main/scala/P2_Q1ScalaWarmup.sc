//Warmup 1
def prime (x : Int) : Boolean = {
  if (x == 1)
    false
  else
    (2 until x) forall (x % _ != 0)
}
//Test
prime(5) 
prime(10)

//Warmup 2
def twinprimes(x : Int, y: Int) : Boolean = {
  if (prime(x) && prime(y)){
    if (x > y)
      x - y == 2
    else
      y - x == 2
  }
  else{
    false
  }
}

//Test
twinprimes(41,43)
twinprimes(41,47)
