//input document has lines of text, we need first digit and last digit from each line.
//there are also spelled out numbers, now we include them.
import scala.io.Source

val numbers = Map("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9)

case class numberIndexes(number: Int, firstIndex: Int, lastIndex: Int)

def singleLineValue(line: String) = {
    val justDigits = line.filter(c => c.isDigit)

    val numIndexes = numbers.map((word, num) => {
        val startWordIndex = line.indexOf(word) 
        val startNumIndex = line.indexOf(s"$num")
        val lastWordIndex = line.lastIndexOf(word)
        val lastNumIndex = line.lastIndexOf(s"$num")

        val noNegativeStartWordIndex = if (startWordIndex > -1) startWordIndex else Int.MaxValue 
        val noNegativeStartNumIndex = if (startNumIndex > -1) startNumIndex else Int.MaxValue 

        val nI = numberIndexes(num, noNegativeStartWordIndex.min(noNegativeStartNumIndex), lastWordIndex.max(lastNumIndex))

        println(s" line: $line swI: $noNegativeStartWordIndex sni: $noNegativeStartNumIndex lwi $lastWordIndex lni: $lastNumIndex ni: $nI")
        nI
    })

    val firstNumber = numIndexes.minBy(ni => ni.firstIndex)
    val lastNumber = numIndexes.maxBy(ni => ni.lastIndex)

    val num = (firstNumber.number * 10) + lastNumber.number
    println(s" num: $num from first: $firstNumber last: $lastNumber line: $line")
    num
}

def calibrationValue(calibrationDoc: Iterator[String]) ={
    val fullsum = calibrationDoc.map(l => singleLineValue(l)).fold(0)(_ + _)
    println(s"full sum: $fullsum")
}

val finalValue = calibrationValue(Source.fromFile("input.txt").getLines())

println(finalValue)
