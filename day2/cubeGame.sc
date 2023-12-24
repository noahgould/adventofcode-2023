import scala.io.Source

case class cubePull(red: Int, green: Int, blue: Int)
case class game(id: Int, cubePulls: List[cubePull])


def ispullPossible(pull: cubePull): Boolean = pull.red <= possibleGameBag.red && pull.green <= possibleGameBag.green && pull.blue <= possibleGameBag.blue

def getNumberOfColorCubes(rawPull: String, color: String) = {
    val numColor = Option.when(rawPull.contains(s" $color"))(rawPull.split(s" $color")(0).split(" ").last.toInt).getOrElse(0)
    println(s"raw pull: $rawPull color: $color numColor: $numColor")
    numColor
}

def getGames(file: String) : Seq[game] = Source.fromFile("input.txt").getLines().map { line => 
    val splitToGames = line.split(":")
    val id = splitToGames(0).split(" ")(1).toInt
    val rawPulls = splitToGames(1).split(";").toSeq
    println(s"rawpulls $rawPulls")
    val pulls = rawPulls.map(rawPull => cubePull(getNumberOfColorCubes(rawPull, "red"), getNumberOfColorCubes(rawPull, "green"), getNumberOfColorCubes(rawPull, "blue")))
    println(s"line: $line pulls: $pulls id: $id")
    game(id, pulls.toList)
}.toSeq

val possibleGameBag = cubePull(12, 13, 14)


val allGames = getGames("input.txt")
allGames.map(g => println(g))
val possibleGames = allGames.filter(game => game.cubePulls.forall(ispullPossible))
possibleGames.map(g => println(g))
val possibleGamesSum = possibleGames.map(_.id).sum
println(s"sum: $possibleGamesSum")

