import Defs._
import Search._ 
import Board._ 
import MoveGen._
import Evaluate._

object Main {
    def InitBoardVars() {
        var index = 0
        for(index <- 0 until MAXGAMEMOVES) {
            brd_history.push({
                move : NOMOVE,
                castlePerm : 0,
                enPas : 0,
                fiftyMove : 0,
                posKey : 0
            })
        }

        for(index <- 0 until PVENTRIES ) {
            brd_PvTable.push({
                move : NOMOVE,
                posKey : 0
            })
        }

    }

    def EvalInit() {
        for(index <- 0 until 10 ) {
            PawnRanksWhite(index) = 0
            PawnRanksBlack(index) = 0
        }
    }

    def InitHashKeys() {
        for(index <- 0 until 13 * 120 ) {
            PieceKeys(index) = RAND_32()
        }

        SideKey = RAND_32()

        for(index <- 0 until 16 ) {
            CastleKeys(index) = RAND_32()
        }
    }

    def InitSq120To64() {
        val file = FILES.FILE_A.id
        val rank = RANKS.RANK_1.id
        var sq = SQUARES.A1.id
        var sq64 = 0

        for(index <- 0 until BRD_SQ_NUM ) {
            Sq120ToSq64(index) = 65
        }

        for(index <- 0 until 64 ) {
            Sq64ToSq120(index) = 120
        }

        for(rank <- RANKS.RANK_1.id to RANKS.RANK_8.id) {
            for(file <- FILES.FILE_A.id to FILES.FILE_H.id) {
                sq = FR2SQ(file,rank)
                Sq64ToSq120(sq64) = sq
                Sq120ToSq64(sq) = sq64
                sq64 += 1
            }
        }
    }

    def InitFilesRanksBrd() {
        var file = FILES.FILE_A.id
        var rank = RANKS.RANK_1.id
        var sq = SQUARES.A1.id
        var sq64 = 0

        for(index <- 0 until BRD_SQ_NUM ) {
            FilesBrd(index) = SQUARES.OFFBOARD.id
            RanksBrd(index) = SQUARES.OFFBOARD.id
        }

        for(rank <- RANKS.RANK_1.id to RANKS.RANK_8.id) {
            for(file <- FILES.FILE_A.id to FILES.FILE_H.id) {
                sq = FR2SQ(file,rank)
                FilesBrd(sq) = file
                RanksBrd(sq) = rank
            }
        }
    }

    def init() {
        InitFilesRanksBrd()
        InitSq120To64()
        InitHashKeys()
        InitBoardVars()
        InitMvvLva()
        //initBoardSquares()
        EvalInit()
        srch_thinking = false
    }

    def main(args: Array[String]): Unit = {
        init()
    }
}
