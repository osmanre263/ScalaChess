import Board._
import Defs._
import MoveGen._
import MakeMove._
import util.control.Breaks._

object PVTable {
    def GetPvLine(depth : Int) : Int = {
        //println("GetPvLine")

        var move = ProbePvTable()
        var count = 0
        breakable {
        while(move != NOMOVE && count < depth) {
            if(MoveExists(move)) {
                MakeMove(move)
                brd_PvArray(count) = move
                count += 1
            } else {
                break
            }
            move = ProbePvTable()
        }}

        while(brd_ply > 0) {
            TakeMove()
        }
        return count
    }

    def StorePvMove(move : Int) {
        val index = brd_posKey % PVENTRIES

        brd_PvTable(index).move = move
        brd_PvTable(index).posKey = brd_posKey
    }

    def ProbePvTable(): Int = {
        val index = brd_posKey % PVENTRIES

        if(brd_PvTable(index).posKey == brd_posKey ) {
            return brd_PvTable(index).move
        }

        return NOMOVE
    }
}
