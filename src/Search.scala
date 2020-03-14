import Defs._
import Board._
import PVTable._
import Evaluate._
import MoveGen._
import MakeMove._
import IO._
import java.util.Calendar._
import util.control.Breaks._

object Search {
    var srch_nodes = 0
    var srch_fh = 0
    var srch_fhf = 0
    var srch_depth = 0
    var srch_time = 0
    var srch_start : Long = 0
    var srch_stop = false
    var srch_best = 0
    var srch_thinking = false
    var srch_fen = ""

    def CheckUp() {
        val time = System.currentTimeMillis
        if ((time - srch_start) > srch_time ) srch_stop = true
    }

    def PickNextMove(moveNum : Int) {
        var bestScore = 0
        var bestNum = moveNum
        var temp = 0
        
        for (index <- moveNum until brd_moveListStart(brd_ply + 1)) {
            if (brd_moveScores(index) > bestScore) {
                bestScore = brd_moveScores(index)
                bestNum = index
            }
        }

        temp = brd_moveList(moveNum)
        brd_moveList(moveNum) = brd_moveList(bestNum)
        brd_moveList(bestNum) = temp

        temp = brd_moveScores(moveNum)
        brd_moveScores(moveNum) = brd_moveScores(bestNum)
        brd_moveScores(bestNum) = temp
    }

    def IsRepetition(): Boolean = {
        for (index <- brd_hisPly-brd_fiftyMove until brd_hisPly-1) {
            if (brd_posKey == brd_history(index).posKey) {
                return true
            }
        }
        return false
    }

    def ClearPvTable() {
        brd_PvTable = Array.fill[PvMove](PVENTRIES)(new PvMove())
    }

    def ClearForSearch() {
        for (index <- 0 until 14 * BRD_SQ_NUM) {
            brd_searchHistory(index) = 0
        }

        for (index <- 0 until 3 * MAXDEPTH) {
            brd_searchKillers(index) = 0
        }

        ClearPvTable()

        brd_ply = 0

        srch_nodes = 0
        srch_fh = 0
        srch_fhf = 0
        srch_start = System.currentTimeMillis
        srch_stop = false
    }

    def Quiescence(_alpha : Int, _beta : Int): Int = {
        var alpha = _alpha
        val beta = _beta
        
        if ((srch_nodes & 2047) == 0) CheckUp()

        srch_nodes += 1

        if (IsRepetition() || brd_fiftyMove >= 100) {
            return 0
        }

        if (brd_ply > MAXDEPTH - 1) {
            return EvalPosition()
        }

        var Score = EvalPosition()

        if (Score >= beta) {
            return beta
        }

        if (Score > alpha) {
            alpha = Score
        }

        GenerateCaptures()

        val MoveNum = 0
        var Legal = 0
        val OldAlpha = alpha
        var BestMove = NOMOVE
        val PvMove = ProbePvTable()
        var Move = 0
        Score = -INFINITE

        if (PvMove != NOMOVE) {
            breakable {
            for (MoveNum <- brd_moveListStart(brd_ply) until brd_moveListStart(brd_ply + 1)) {
                if (brd_moveList(MoveNum) == PvMove) {
                    brd_moveScores(MoveNum) = 2000000
                    break
                }
            }}
        }

        for (MoveNum <- brd_moveListStart(brd_ply) until brd_moveListStart(brd_ply + 1) )  {
            PickNextMove(MoveNum)
            Move = brd_moveList(MoveNum)

            if (MakeMove(Move))  {
                Legal += 1
                Score = -Quiescence(-beta, -alpha)
                TakeMove()
                if (srch_stop) return 0
                if (Score > alpha) {
                    if (Score >= beta) {
                        if (Legal == 1) {
                            srch_fhf += 1
                        }
                        srch_fh += 1
    
                        return beta
                    }
                    alpha = Score
                    BestMove = Move
                }
            }
        }

        if (alpha != OldAlpha) {
            StorePvMove(BestMove)
        }

        return alpha
    }

    def AlphaBeta(_alpha : Int, _beta : Int, _depth : Int, DoNull : Boolean): Int = {
        var alpha = _alpha
        val beta = _beta
        var depth = _depth

        if (depth <= 0) {
            return Quiescence(alpha, beta)
        }
        
        if ((srch_nodes & 2047) == 0) CheckUp()

        srch_nodes += 1

        if ((IsRepetition() || brd_fiftyMove >= 100) && brd_ply != 0) {
            return 0
        }

        if (brd_ply > MAXDEPTH - 1) {
            return EvalPosition()
        }

        val InCheck = SqAttacked(brd_pList(PCEINDEX(Kings(brd_side).id, 0)), brd_side^1)
        if (InCheck) {
            depth += 1
        }

        var Score = -INFINITE

        /*if (DoNull && !InCheck && brd_ply != 0 && (brd_material(brd_side) > 50200) && depth >= 4) {
            val ePStore = brd_enPas
            if (brd_enPas != SQUARES.NO_SQ.id) HASH_EP()
            brd_side ^= 1
            HASH_SIDE()
            brd_enPas = SQUARES.NO_SQ.id

            Score = -AlphaBeta( -beta, -beta + 1, depth-4, false)

            brd_side ^= 1
            HASH_SIDE()
            brd_enPas = ePStore
            if (brd_enPas != SQUARES.NO_SQ.id) HASH_EP()

            if (srch_stop) return 0
            if (Score >= beta) {
                return beta
            }
        }*/

        GenerateMoves()

        val MoveNum = 0
        var Legal = 0
        val OldAlpha = alpha
        var BestMove = NOMOVE
        var Move = NOMOVE
        Score = -INFINITE
        
        val PvMove = ProbePvTable()

        if (PvMove != NOMOVE) {
            breakable {
            for (MoveNum <- brd_moveListStart(brd_ply) until brd_moveListStart(brd_ply + 1)) {
                if (brd_moveList(MoveNum) == PvMove) {
                    brd_moveScores(MoveNum) = 2000000
                    break
                }
            }}
        }

        for (MoveNum <- brd_moveListStart(brd_ply) until brd_moveListStart(brd_ply + 1))  {
            PickNextMove(MoveNum)
            Move = brd_moveList(MoveNum)

            if (MakeMove(Move)) {
                Legal += 1
                Score = -AlphaBeta(-beta, -alpha, depth-1, true)

                TakeMove()

                if (srch_stop) return 0

                if (Score > alpha) {
                    if (Score >= beta) {
                        if (Legal == 1) {
                            srch_fhf += 1
                        }
                        srch_fh += 1

                        if ((Move & MFLAGCAP) == 0) {
                            brd_searchKillers(MAXDEPTH + brd_ply) = brd_searchKillers(brd_ply)
                            brd_searchKillers(brd_ply) = Move
                        }
                        return beta
                    }
                    alpha = Score
                    BestMove = Move

                    if ((BestMove & MFLAGCAP) == 0) {
                        brd_searchHistory(brd_pieces(FROMSQ(BestMove)) * BRD_SQ_NUM + TOSQ(BestMove)) += depth * depth
                    }
                }
            }
        }

        if (Legal == 0) {
            if (InCheck) {
                return -MATE + brd_ply
            } else {
                return 0
            }
        }

        if (alpha != OldAlpha) {
            StorePvMove(BestMove)
        }

        return alpha
    }

    /*var domUpdate_depth
    var domUpdate_move
    var domUpdate_score
    var domUpdate_nodes
    var domUpdate_ordering

    def UpdateDOMStats {
    var scoreText = "Score: " + (domUpdate_score/100).toFixed(2)
    if (Math.abs(domUpdate_score) > MATE-MAXDEPTH) {
        scoreText = "Score: " + "Mate In " + (MATE - Math.abs(domUpdate_score)) + " moves"
    }

    //println("UpdateDOMStats depth:" + domUpdate_depth + " score:" + domUpdate_score + " nodes:" + domUpdate_nodes)
    $("#OrderingOut").text("Ordering: " + domUpdate_ordering + "%")
    $("#DepthOut").text("Depth: " + domUpdate_depth)
    $("#ScoreOut").text(scoreText)
    $("#NodesOut").text("Nodes: " + domUpdate_nodes)
    $("#TimeOut").text("Time: " + (($.now()-srch_start)/1000).toFixed(1) + "s")
    */

    def SearchPosition(depth : Int) {
        println("FEN: " + srch_fen)
        println("Search started...")
        var bestMove = NOMOVE
        var bestScore = -INFINITE
        var pvMoves = 0
        var line = ""

        ClearForSearch()
        srch_time = 3*1000
        breakable {
        for (currentDepth <- 1 to depth ) {
            bestScore = AlphaBeta(-INFINITE, INFINITE, currentDepth, true)
            if (srch_stop) break
            bestMove = ProbePvTable()

            line = "D: " + currentDepth + " Best: " + PrMove(bestMove) + " Score: " + bestScore + " nodes: " + srch_nodes

            pvMoves = GetPvLine(currentDepth)
            line += " Pv:"
            for (pvNum <- 0 until pvMoves) {
                line += " " + PrMove(brd_PvArray(pvNum))
            }
            if (currentDepth != 1) {
                //println(srch_fhf.toDouble + " " + srch_fh.toDouble)
                line += " Ordering: " + ((srch_fhf.toDouble/srch_fh.toDouble)*100).formatted("%.2f") + "%"
            }
            println(line)
        }}
        println("Search stopped... Best Move: " + PrMove(bestMove))
        srch_best = bestMove
        srch_thinking = false
    }
}
