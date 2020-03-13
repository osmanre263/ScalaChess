import Board._
import Defs._ 
import MoveGen._
import MakeMove._
import util.control.Breaks._

object IO {
    /*def SqFromAlg(moveAlg) {

        //println('SqFromAlg' + moveAlg)
        if(moveAlg.length != 2) return SQUARES.NO_SQ

        if(moveAlg(0) > 'h' || moveAlg(0) < 'a' ) return SQUARES.NO_SQ
        if(moveAlg(1) > '8' || moveAlg(1) < '1' ) return SQUARES.NO_SQ

        file = moveAlg(0).charCodeAt() - 'a'.charCodeAt()
        rank = moveAlg(1).charCodeAt() - '1'.charCodeAt()

        return FR2SQ(file,rank)
    }*/

    def PrintMoveList() {
        var move = 0
        println("MoveList:")

        for(index <- brd_moveListStart(brd_ply) until brd_moveListStart(brd_ply + 1) ) {

            move = brd_moveList(index)
            println("Move:" + (index+1) + " > " + PrMove(move))

        }
    }

    def PrSq(sq : Int) {
        val file = FilesBrd(sq)
        val rank = RanksBrd(sq)

        val sqStr = String.fromCharCode('a'.charCodeAt() + file) + String.fromCharCode('1'.charCodeAt() + rank)
        return sqStr
    }

    def PrMove(move : Int): String = {

        var MvStr = ""

        //MvStr = String.fromCharCode('a'.charCodeAt() + ff) + String.fromCharCode('1'.charCodeAt() + rf) +
         //   String.fromCharCode('a'.charCodeAt() + ft) + String.fromCharCode('1'.charCodeAt() + rt)

        val promoted = PROMOTED(move)

        if(promoted != PIECES.EMPTY.id) {
            var pchar = "q"
            if(PieceKnight(promoted)) {
                pchar = "n"
            } else if(PieceRookQueen(promoted) && !PieceBishopQueen(promoted))  {
                pchar = "r"
            } else if(!PieceRookQueen(promoted) && PieceBishopQueen(promoted))   {
                pchar = "b"
            }
            MvStr += pchar
        }
        return MvStr
    }

    def ParseMove(from : Int, to : Int): Int = {

        GenerateMoves()

        var Move = NOMOVE
        var PromPce = PIECES.EMPTY.id
        var found = false
        for(index <- brd_moveListStart(brd_ply) until brd_moveListStart(brd_ply + 1)) {
            Move = brd_moveList(index)
            if(FROMSQ(Move)==from && TOSQ(Move)==to) {
                PromPce = PROMOTED(Move)
                if(PromPce!=PIECES.EMPTY.id) {
                    if( (PromPce==PIECES.wQ.id && brd_side==COLORS.WHITE.id) || (PromPce==PIECES.bQ.id && brd_side==COLORS.BLACK.id) ) {
                        found = true
                        break
                    }
                    //continue
                }
                found = true
                break
            }
        }

        if(found) {
            if(!MakeMove(Move)) {
                return NOMOVE
            }
            TakeMove()
            return Move
        }

        return NOMOVE
    }
}
