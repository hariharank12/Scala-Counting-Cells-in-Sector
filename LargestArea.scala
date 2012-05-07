package area

object LargestArea {
	def main(args: Array[String]) {
		println("Largest Sector is " + (Traversal.calculateSectorUnits.max + 1))
	}
}

object Traversal {
	
	private var visitedCells:List[(Int, Int)] = List.empty
	
	//def getFilledCells = Set((0,0), (1,0), (0,2), (1,2)) //Your input comes here
	def getFilledCells = Set((0,0), (0,1), (0,2), (1,2), (2,0), (2, 1)) //Your input comes here

	def largestCount = {
		val sector_count:List[Int] = List.empty
	}

	def adjacentIndices = {
		val range = (-1 to 1).toList
		val adjacent_nos = range.flatMap(e => range.map( _*1 ))
		adjacent_nos.sorted.zip(adjacent_nos).filter(_ != (0,0))
	}

	def calculateSectorUnits = {
		var resultsCount:List[Int] = List.empty
		for(cell <- getFilledCells) {
			resultsCount :+= calculateAdjacentForCell(cell)
		}
		resultsCount
	}

	def calculateAdjacentForCell(cell:(Int, Int)):Int = { 
		visitedCells :+= cell
	  adjacentIndices.map(index => calculateAdjacent(cell, index)).sum
	}

	def calculateAdjacent(cell:(Int, Int), index:(Int, Int)) = cell match {
		case cell if hasAdjacent(cell, index) => visitNeighbor(cell, index) 
		case _ => 0
	}

	def hasAdjacent(cell:(Int, Int), index:(Int, Int)) = getFilledCells.contains(nextCell(cell, index))

	def isVisited(cell:(Int, Int)) = cell match {
		case cell if visitedCells.contains(cell) => true
		case _ => false
	}

	def nextCell(cell:(Int, Int), index:(Int, Int)) = ((cell._1 + index._1),(cell._2 + index._2))

	def visitNeighbor(cell:(Int, Int), index:(Int, Int)) = {
		(if(isVisited(nextCell(cell, index))) 0 else 1 + calculateAdjacentForCell(nextCell(cell, index)))
	}

}