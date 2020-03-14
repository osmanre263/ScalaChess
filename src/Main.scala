import Defs._
import Search._
import Board._
import MoveGen._
import MakeMove._
import Evaluate._
import Perft._
import IO._

object Main {
    def InitBoardVars() {
        brd_history = Array.fill[HistoryMove](MAXGAMEMOVES)(new HistoryMove())
        brd_PvTable = Array.fill[PvMove](MAXGAMEMOVES)(new PvMove())
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
        var sq = SQUARES.A1.id

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
        var one = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        ParseFen(START_FEN)
        //PrintBoard()
        GenerateMoves()
        //PrintPceLists()
        //PrintMoveList()
        CheckBoard()
        SearchPosition(10)
    }
}
