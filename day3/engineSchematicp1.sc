import scala.io.Source

//example engine schematic:

// 467..114..
// ...*......
// ..35..633.
// ......#...
// 617*......
// .....+.58.
// ..592.....
// ......755.
// ...$.*....
// .664.598..

//In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 
//114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

//loading it in: read the file and parse it into 2D array of chars
//possible sols:
//1. go down each line in array, check if each element is a digit. If it is, check if it is neighboring a symbol in any directon
// track digits until we hit a period to get the full number, so keep track of the current number, and if it is a part num or not.
//2. add the part numbers to a list
//3. add it up

// 1-......
// ........
// ......-1
// ........
// .24.....
// ......*4

def loadSchematic() = Source.fromFile("/Users/ngould/source/third_party_libs/adventofcode-2023/day3/input.txt").getLines().toArray.map(l => l.toCharArray())

val schematicArray = loadSchematic()
println(schematicArray.map(_.mkString).mkString("\n"))

val specialCharRegex = "[^a-zA-Z0-9. ]".r

def checkIfPartNum(currentIndex: Tuple2[Int, Int], schematic: Array[Array[Char]]): Boolean = {
    val adjecents = List[Tuple2[Int, Int]]((1,1),(1,0),(0,1),(-1,0),(0,-1),(-1,-1),(-1,1),(1,-1))
    val adjc1 = adjecents.filter((x,_) => currentIndex._1 + x > -1 && currentIndex._1 + x < schematic.length)
    val adjc2 = adjc1.filter((_,y) => currentIndex._2 + y > -1 && currentIndex._2 + y < schematic(currentIndex._1).length)
    val filtredLetterDigits = adjc2.filterNot((x, y) => schematic(currentIndex._1 + x)(currentIndex._2 + y).isLetterOrDigit)
    val filteredPeriods = filtredLetterDigits.filterNot((x,y) => schematic(currentIndex._1 + x)(currentIndex._2 + y).equals('.'))
    val filteredSpecials = adjc2.filter((x,y) => specialCharRegex.matches(schematic(currentIndex._1 + x)(currentIndex._2 + y).toString()))
    // println(s"index: $currentIndex charInQuestino: ${schematic(currentIndex._1)(currentIndex._2)} adj2: $adjc2")
    // println(s"index: $currentIndex charInQuestino: ${schematic(currentIndex._1)(currentIndex._2)}")
    // println(s"filtered Regex: $filteredSpecials filteredPeriods: $filteredPeriods ")
    if (filteredSpecials != filteredPeriods) then println("NOT EQUAL")

    return !filteredSpecials.isEmpty
}

def partNumbersFromSchematic(schematic: Array[Array[Char]]): Seq[Int] = {
    var partNumbers: List[Int] = List.empty
    for (x <- schematic.indices)
        var isPartNum = false
        var currNum = ""
        for (y <- schematic(x).indices)
            if (schematic(x)(y).isDigit)
                val currentItem = schematic(x)(y)
                currNum = currNum.appended(currentItem)
                if (!isPartNum)
                    isPartNum = checkIfPartNum((x,y), schematic)
            else
                if (isPartNum)
                    partNumbers = partNumbers.appended(currNum.toInt)
                isPartNum = false
                currNum = ""
            // println(s" part numbers: $partNumbers")
        if (isPartNum)
            partNumbers = partNumbers.appended(currNum.toInt)
    
    return partNumbers
}



val partNums = partNumbersFromSchematic(schematicArray)
println(partNums)

val partNumSum = partNums.sum
println(s"partNumSum: $partNumSum")
