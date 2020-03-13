import MoveGen._
import Board._ 
import MakeMove._
import IO._

object Perft {
    var perft_leafNodes = 0

    def Perft(depth : Int) {
        if(depth == 0) {
            perft_leafNodes += 1
            return
        }

        GenerateMoves()

        var move = 0
        for(index <- brd_moveListStart(brd_ply) until brd_moveListStart(brd_ply + 1)) {
            move = brd_moveList(index)
            if(MakeMove(move)) {
                Perft(depth-1)
                TakeMove()
            }

        }
    }

    def PerftTest(depth : Int) {
        PrintBoard()
        println("Starting Test To Depth:" + depth)
        perft_leafNodes = 0

        GenerateMoves()

        var move = 0
        var moveNum = 0
        for(index <- brd_moveListStart(brd_ply) until brd_moveListStart(brd_ply + 1) ) {
            move = brd_moveList(index)

            if(MakeMove(move)) {
                moveNum += 1
                val cumnodes = perft_leafNodes
                Perft(depth-1)
                TakeMove()
                val oldnodes = perft_leafNodes - cumnodes
                println("move:" + moveNum + " " + PrMove(move) + " " + oldnodes)
            }
        }

        println("Test Complete : " + perft_leafNodes + " leaf nodes visited")
        //$("#FenOutput").text("Test Complete : " + perft_leafNodes + " leaf nodes visited")
    }
}
