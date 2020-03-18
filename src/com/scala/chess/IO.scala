package com.scala.chess

import Board._
import Defs._
import MakeMove._
import MoveGen._
import scala.util.control.Breaks._

object IO {
    def PrintMoveList() {
        var move = 0
        println("MoveList:")

        for(index <- brd_moveListStart(brd_ply) until brd_moveListStart(brd_ply + 1) ) {
            move = brd_moveList(index)
            println(PrMove(move))
        }
    }

    def PrSq(sq : Int): String = {
        val file = FilesBrd(sq)
        val rank = RanksBrd(sq)

        FileChar(file).toString + RankChar(rank).toString
    }

    def PrMove(move : Int): String = {

        var MvStr = ""

        val ff = FilesBrd(FROMSQ(move))
        val rf = RanksBrd(FROMSQ(move))
        val ft = FilesBrd(TOSQ(move))
        val rt = RanksBrd(TOSQ(move))

        MvStr = FileChar(ff).toString + RankChar(rf).toString + FileChar(ft).toString + RankChar(rt).toString

        val promoted = PROMOTED(move)

        if(promoted != PIECES.EMPTY.id) {
            var pchar = 'q'
            if(PieceKnight(promoted)) {
                pchar = 'n'
            } else if(PieceRookQueen(promoted) && !PieceBishopQueen(promoted))  {
                pchar = 'r'
            } else if(!PieceRookQueen(promoted) && PieceBishopQueen(promoted))   {
                pchar = 'b'
            }
            MvStr += pchar
        }
        MvStr
    }

    def ParseMove(from : Int, to : Int): Int = {
        GenerateMoves()

        var Move = NOMOVE
        var PromPce = PIECES.EMPTY.id
        var found = false
        breakable {
        for(index <- brd_moveListStart(brd_ply) until brd_moveListStart(brd_ply + 1)) {
            Move = brd_moveList(index)
            if(FROMSQ(Move) == from && TOSQ(Move) == to) {
                PromPce = PROMOTED(Move)
                if(PromPce!=PIECES.EMPTY.id) {
                    if( (PromPce == PIECES.wQ.id && brd_side == COLORS.WHITE.id) || (PromPce == PIECES.bQ.id && brd_side == COLORS.BLACK.id) ) {
                        found = true
                        break
                    }
                    //continue
                }
                found = true
                break
            }
        }}

        if(found) {
            if(!MakeMove(Move)) {
                return NOMOVE
            }
            TakeMove()
            return Move
        }

        NOMOVE
    }
}
