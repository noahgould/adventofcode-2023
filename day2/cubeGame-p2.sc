import scala.io.Source

case class cubeGroup(red: Int, green: Int, blue: Int)
case class game(id: Int, cubeGroups: List[cubeGroup])


def ispullPossible(pull: cubeGroup): Boolean = pull.red <= possibleGameBag.red && pull.green <= possibleGameBag.green && pull.blue <= possibleGameBag.blue

def getNumberOfColorCubes(rawPull: String, color: String) =  Option.when(rawPull.contains(s" $color"))(rawPull.split(s" $color")(0).split(" ").last.toInt).getOrElse(0)


def getGames(file: String) : Seq[game] = Source.fromFile("input.txt").getLines().map { line => 
    val splitToGames = line.split(":")
    val id = splitToGames(0).split(" ")(1).toInt
    val rawPulls = splitToGames(1).split(";").toSeq
    val pulls = rawPulls.map(rawPull => cubeGroup(getNumberOfColorCubes(rawPull, "red"), getNumberOfColorCubes(rawPull, "green"), getNumberOfColorCubes(rawPull, "blue")))
    game(id, pulls.toList)
}.toSeq

val possibleGameBag = cubeGroup(12, 13, 14)

val allGames = getGames("input.txt")
// allGames.map(g => println(g))

val minCubeGroups = allGames.map(g => 
    g.cubeGroups.fold(cubeGroup(0,0,0))((existing, curr) => 
        cubeGroup(existing.red.max(curr.red), existing.green.max(curr.green), existing.blue.max(curr.blue)) ))

println(s"min cube groups: $minCubeGroups")

val cubePowers = minCubeGroups.map(cg => cg.red * cg.blue * cg.green)

println(s"cube powers: $cubePowers")

val cubePowerSum = cubePowers.sum
println(s"cube power sum: $cubePowerSum")

