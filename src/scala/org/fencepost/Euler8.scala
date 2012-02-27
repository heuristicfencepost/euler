package org.fencepost

import scala.math.max

object Euler8 {

  val thestr = """73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450"""

  def product(alist:List[Int]) = (1 /: alist)(_*_)

  // Recursive function that forms the meat of our operation.  For a given input
  // value and buffer, drop the head of the current buffer, add the input item
  // at the end of the buffer and determine the product.  We then return the max
  // of this product, the current max and the value of the recursive call.
  def biggest(alist:List[Int],abuff:List[Int],currmax:Int):Int = {

    if (alist.length == 0)
      return 0
    val newbuff = abuff.tail ::: List(alist.head)
    val newcurrmax = max(currmax,product(newbuff))
    max(newcurrmax,biggest(alist.tail,newbuff,newcurrmax))
  }

  def main(args: Array[String]): Unit = {

    // A list of Strings, one for each line of our input argument
    val linelist = thestr.lines.toList

    // Initialize the buffer we wish to evaluate and the current max,
    // which in this case is really just the product of the current buffer
    val initbuff = linelist(0).slice(0,5).map(_ asDigit).toList
    val initmax = product(initbuff)

    // Create the rest of our input list with a for expression applied to all
    // lines after the first.  For a language that prides itself on removing
    // boilerplate this is a somewhat cumbersome syntax.
    val restoflist =
      for {
        i <- 1 to 19
        ilist = linelist(i)
        j <- ilist
      } yield j asDigit
    val inputlist = linelist(0).slice(5,linelist(0).length).map(_ asDigit).toList ::: restoflist.toList

    print("Biggest: " + biggest(inputlist,initbuff,0))
  }
}
