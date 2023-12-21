//input document has lines of text, we need first digit and last digit from each line.
import scala.io.Source

def singleLineValue(line: String) = {
    line.foreach(c => {
        println(s"char $c is an digit: ${c.isDigit}")
    })
    line.indexOf("a")

    val justDigits = line.filter(c => c.isDigit)
    println(justDigits)

    for {
        firstInt <- justDigits.headOption
        lastInt <- justDigits.lastOption
        fullNum = s"$firstInt$lastInt"
        _ = println(s"got fullnum: $fullNum from firstInt: $firstInt lastInt: $lastInt line: $line")
    } yield fullNum
}

def calibrationValue(calibrationDoc: Iterator[String]) ={
    val fullsum = calibrationDoc.flatMap(l => singleLineValue(l)).map(s => s.toInt).fold(0)(_ + _)
    println(s"full sum: $fullsum")
}
val finalValue = calibrationValue(Source.fromFile("input.txt").getLines())

println(finalValue)
