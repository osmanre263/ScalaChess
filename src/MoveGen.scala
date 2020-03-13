import Board._
import Defs._
import MakeMove._
import util.control.Breaks._

object MoveGen {
    var VictimScore = Array( 0, 100, 200, 300, 400, 500, 600, 100, 200, 300, 400, 500, 600 )
    var MvvLvaScores = new Array[Int](14 * 14)

    def InitMvvLva() {
        val Attacker = 0
        val Victim = 0
        for(Attacker <- PIECES.wP.id to PIECES.bK.id) {
            for(Victim <- PIECES.wP.id to PIECES.bK.id) {
                val score = VictimScore(Victim) + 6 - ( VictimScore(Attacker) / 100)
                val index = Victim * 14 + Attacker
                MvvLvaScores(index) = score
            }
        }
    }

    def MOVE(from : Int,to : Int,captured : Int,promoted : Int,flag : Int): Int = {
        return (from | (to << 7) | (captured << 14) | (promoted << 20) | flag)
    }

    def MoveExists(move : Int) : Boolean = {
        GenerateMoves()

        var moveFound = NOMOVE
        for(index <- brd_moveListStart(brd_ply) until brd_moveListStart(brd_ply + 1)) {
            moveFound = brd_moveList(index)
            if(MakeMove(moveFound)) {
                TakeMove()
                if (move == moveFound) {
                    return true
                }
            }
        }
        return false
    }

    def AddCaptureMove(move : Int) {
        brd_moveList(brd_moveListStart(brd_ply + 1)) = move
        //needs to be tested
        brd_moveListStart(brd_ply + 1) += 1
        val index = brd_moveListStart(brd_ply + 1)
        val score = CAPTURED(move) * 14 + brd_pieces(FROMSQ(move))
        val finalScore = MvvLvaScores(score) + 1000000
        brd_moveScores(index) = finalScore
    }

    def AddQuietMove(move : Int) {
        brd_moveList(brd_moveListStart(brd_ply + 1)) = move

        if(brd_searchKillers(brd_ply) == move) {
            brd_moveScores(brd_moveListStart(brd_ply + 1)) = 900000
        } else if(brd_searchKillers(MAXDEPTH + brd_ply) == move) {
            brd_moveScores(brd_moveListStart(brd_ply + 1)) = 800000
        } else {
            brd_moveScores(brd_moveListStart(brd_ply + 1)) = brd_searchHistory( brd_pieces(FROMSQ(move)) * BRD_SQ_NUM + TOSQ(move) )
        }
        brd_moveListStart(brd_ply + 1) += 1
    }

    def AddEnPassantMove(move : Int) {
        brd_moveList(brd_moveListStart(brd_ply + 1)) = move
        //needs to be tested
        brd_moveListStart(brd_ply + 1) += 1
        val index = brd_moveListStart(brd_ply + 1)
        brd_moveScores(index) = 105 + 1000000
    }

    def AddWhitePawnCaptureMove(from : Int, to : Int, cap : Int) {
        if(RanksBrd(from)==RANKS.RANK_7) {
            AddCaptureMove(MOVE(from,to,cap,PIECES.wQ.id,0))
            AddCaptureMove(MOVE(from,to,cap,PIECES.wR.id,0))
            AddCaptureMove(MOVE(from,to,cap,PIECES.wB.id,0))
            AddCaptureMove(MOVE(from,to,cap,PIECES.wN.id,0))
        } else {
            AddCaptureMove(MOVE(from,to,cap,PIECES.EMPTY.id,0))
        }
    }

    def AddWhitePawnQuietMove(from : Int, to : Int) {
        if(RanksBrd(from)==RANKS.RANK_7) {
            AddQuietMove(MOVE(from,to,PIECES.EMPTY.id,PIECES.wQ.id,0))
            AddQuietMove(MOVE(from,to,PIECES.EMPTY.id,PIECES.wR.id,0))
            AddQuietMove(MOVE(from,to,PIECES.EMPTY.id,PIECES.wB.id,0))
            AddQuietMove(MOVE(from,to,PIECES.EMPTY.id,PIECES.wN.id,0))
        } else {
            AddQuietMove(MOVE(from,to,PIECES.EMPTY.id,PIECES.EMPTY.id,0))
        }
    }

    def AddBlackPawnCaptureMove(from : Int, to : Int, cap : Int) {
        if(RanksBrd(from)==RANKS.RANK_2) {
            AddCaptureMove(MOVE(from,to,cap,PIECES.bQ.id,0))
            AddCaptureMove(MOVE(from,to,cap,PIECES.bR.id,0))
            AddCaptureMove(MOVE(from,to,cap,PIECES.bB.id,0))
            AddCaptureMove(MOVE(from,to,cap,PIECES.bN.id,0))
        } else {
            AddCaptureMove(MOVE(from,to,cap,PIECES.EMPTY.id,0))
        }
    }

    def AddBlackPawnQuietMove(from : Int, to : Int) {
        if(RanksBrd(from)==RANKS.RANK_2) {
            AddQuietMove(MOVE(from,to,PIECES.EMPTY.id,PIECES.bQ.id,0))
            AddQuietMove(MOVE(from,to,PIECES.EMPTY.id,PIECES.bR.id,0))
            AddQuietMove(MOVE(from,to,PIECES.EMPTY.id,PIECES.bB.id,0))
            AddQuietMove(MOVE(from,to,PIECES.EMPTY.id,PIECES.bN.id,0))
        } else {
            AddQuietMove(MOVE(from,to,PIECES.EMPTY.id,PIECES.EMPTY.id,0))
        }
    }

    def GenerateMoves() {
        brd_moveListStart(brd_ply + 1) = brd_moveListStart(brd_ply)
        var pceType = 0
        var pceIndex = 0
        var pce = 0
        var sq = 0
        var t_sq = 0
        var dir = 0

        if(brd_side == COLORS.WHITE.id) {
            pceType = PIECES.wP.id
            for(pceNum <- 0 until brd_pceNum(pceType)) {
                sq = brd_pList(PCEINDEX(pceType,pceNum))
                if(brd_pieces(sq + 10) == PIECES.EMPTY.id) {
                    AddWhitePawnQuietMove(sq, sq+10)
                    if(RanksBrd(sq) == RANKS.RANK_2 && brd_pieces(sq + 20) == PIECES.EMPTY.id) {
                        AddQuietMove(MOVE(sq,sq+20,PIECES.EMPTY.id,PIECES.EMPTY.id,MFLAGPS))
                    }
                }

                if(!SQOFFBOARD(sq + 9) && PieceCol(brd_pieces(sq + 9)).id == COLORS.BLACK.id) {
                    AddWhitePawnCaptureMove(sq, sq+9, brd_pieces(sq + 9))
                }
                if(!SQOFFBOARD(sq + 11) && PieceCol(brd_pieces(sq + 11)).id == COLORS.BLACK.id) {
                    AddWhitePawnCaptureMove(sq, sq+11, brd_pieces(sq + 11))
                }

                if(brd_enPas != SQUARES.NO_SQ.id) {
                    if(sq + 9 == brd_enPas) {
                        AddEnPassantMove(MOVE(sq,sq + 9,PIECES.EMPTY.id,PIECES.EMPTY.id,MFLAGEP))
                    }
                    if(sq + 11 == brd_enPas) {
                        AddEnPassantMove(MOVE(sq,sq + 11,PIECES.EMPTY.id,PIECES.EMPTY.id,MFLAGEP))
                    }
                }
            }
            if((brd_castlePerm & CASTLEBIT.WKCA.id) != 0) {
                if(brd_pieces(SQUARES.F1.id) == PIECES.EMPTY.id && brd_pieces(SQUARES.G1.id) == PIECES.EMPTY.id) {
                    if(!SqAttacked(SQUARES.E1.id, COLORS.BLACK.id) && !SqAttacked(SQUARES.F1.id, COLORS.BLACK.id)) {
                        AddQuietMove(MOVE(SQUARES.E1.id, SQUARES.G1.id, PIECES.EMPTY.id, PIECES.EMPTY.id, MFLAGCA))
                    }
                }
            }

            if((brd_castlePerm & CASTLEBIT.WQCA.id) != 0) {
                if(brd_pieces(SQUARES.D1.id) == PIECES.EMPTY.id && brd_pieces(SQUARES.C1.id) == PIECES.EMPTY.id && brd_pieces(SQUARES.B1.id) == PIECES.EMPTY.id) {
                    if(!SqAttacked(SQUARES.E1.id, COLORS.BLACK.id) && !SqAttacked(SQUARES.D1.id, COLORS.BLACK.id) ) {
                        AddQuietMove(MOVE(SQUARES.E1.id, SQUARES.C1.id, PIECES.EMPTY.id, PIECES.EMPTY.id, MFLAGCA))
                    }
                }
            }

            pceType = PIECES.wN.id // HACK to set for loop other pieces

        } else {
            pceType = PIECES.bP.id
            for(pceNum <- 0 until brd_pceNum(pceType) ) {
                sq = brd_pList(PCEINDEX(pceType,pceNum))

                if(brd_pieces(sq - 10) == PIECES.EMPTY.id) {
                    AddBlackPawnQuietMove(sq, sq-10)
                    if(RanksBrd(sq) == RANKS.RANK_7 && brd_pieces(sq - 20) == PIECES.EMPTY.id) {
                        AddQuietMove(MOVE(sq,sq-20,PIECES.EMPTY.id,PIECES.EMPTY.id,MFLAGPS))
                    }
                }

                if(!SQOFFBOARD(sq - 9) && PieceCol(brd_pieces(sq - 9)).id == COLORS.WHITE.id) {
                    AddBlackPawnCaptureMove(sq, sq-9, brd_pieces(sq - 9))
                }

                if(!SQOFFBOARD(sq - 11) && PieceCol(brd_pieces(sq - 11)).id == COLORS.WHITE.id) {
                    AddBlackPawnCaptureMove(sq, sq-11, brd_pieces(sq - 11))
                }
                if(brd_enPas != SQUARES.NO_SQ.id) {
                    if(sq - 9 == brd_enPas) {
                        AddEnPassantMove(MOVE(sq,sq - 9,PIECES.EMPTY.id,PIECES.EMPTY.id,MFLAGEP))
                    }
                    if(sq - 11 == brd_enPas) {
                        AddEnPassantMove(MOVE(sq,sq - 11,PIECES.EMPTY.id,PIECES.EMPTY.id,MFLAGEP))
                    }
                }
            }
            if((brd_castlePerm & CASTLEBIT.BKCA.id) != 0) {
                if(brd_pieces(SQUARES.F8.id) == PIECES.EMPTY.id && brd_pieces(SQUARES.G8.id) == PIECES.EMPTY.id) {
                    if(!SqAttacked(SQUARES.E8.id, COLORS.WHITE.id) && !SqAttacked(SQUARES.F8.id, COLORS.WHITE.id)) {
                        AddQuietMove(MOVE(SQUARES.E8.id, SQUARES.G8.id, PIECES.EMPTY.id, PIECES.EMPTY.id, MFLAGCA))
                    }
                }
            }

            if((brd_castlePerm & CASTLEBIT.BQCA.id) != 0) {
                if(brd_pieces(SQUARES.D8.id) == PIECES.EMPTY.id && brd_pieces(SQUARES.C8.id) == PIECES.EMPTY.id && brd_pieces(SQUARES.B8.id) == PIECES.EMPTY.id) {
                    if(!SqAttacked(SQUARES.E8.id, COLORS.WHITE.id) && !SqAttacked(SQUARES.D8.id, COLORS.WHITE.id) ) {
                        AddQuietMove(MOVE(SQUARES.E8.id, SQUARES.C8.id, PIECES.EMPTY.id, PIECES.EMPTY.id, MFLAGCA))
                    }
                }
            }

            pceType = PIECES.bN.id // HACK to set for loop other pieces
        }


        pceIndex = LoopSlideIndex(brd_side)
        pceIndex += 1
        pce = LoopSlidePce(pceIndex)

        while(pce != 0) {
            for(pceNum <- 0 until brd_pceNum(pce) ) {
                sq = brd_pList(PCEINDEX(pce,pceNum))

                for(index <- 0 until DirNum(pce) ) {
                    dir = PceDir(pce)(index)
                    t_sq = sq + dir

                    while(!SQOFFBOARD(t_sq)) {
                        if(brd_pieces(t_sq) != PIECES.EMPTY.id) {
                            if( PieceCol(brd_pieces(t_sq)).id == (brd_side ^ 1)) {
                                AddCaptureMove(MOVE(sq, t_sq, brd_pieces(t_sq), PIECES.EMPTY.id, 0))
                            }
                            break
                        }
                        AddQuietMove(MOVE(sq, t_sq, PIECES.EMPTY.id, PIECES.EMPTY.id, 0))
                        t_sq += dir
                    }
                }
            }
            pceIndex += 1
            pce = LoopSlidePce(pceIndex)
        }

        pceIndex = LoopNonSlideIndex(brd_side)
        pceIndex += 1
        pce = LoopNonSlidePce(pceIndex)

        while( pce != 0) {
            for(pceNum <- 0 until brd_pceNum(pce)) {
                sq = brd_pList(PCEINDEX(pce,pceNum))

                for(index <- 0 until DirNum(pce) ) {
                    dir = PceDir(pce)(index)
                    t_sq = sq + dir

                    if(!SQOFFBOARD(t_sq)) {
                        if (brd_pieces(t_sq) != PIECES.EMPTY.id) {
                            if (PieceCol(brd_pieces(t_sq)).id == (brd_side ^ 1)) {
                                AddCaptureMove(MOVE(sq, t_sq, brd_pieces(t_sq), PIECES.EMPTY.id, 0))
                            }
                            //continue
                        }
                        AddQuietMove(MOVE(sq, t_sq, PIECES.EMPTY.id, PIECES.EMPTY.id, 0))
                    }
                }
            }
            pceIndex += 1
            pce = LoopNonSlidePce(pceIndex)
        }

    }

    def GenerateCaptures() {
        var pceType = 0
        var pceIndex = 0
        var pce = 0
        var sq = 0
        var t_sq = 0
        var dir = 0

        brd_moveListStart(brd_ply + 1) = brd_moveListStart(brd_ply)

        if(brd_side == COLORS.WHITE.id) {
            pceType = PIECES.wP.id
            for(pceNum <- 0 until brd_pceNum(pceType)) {
                sq = brd_pList(PCEINDEX(pceType,pceNum))

                if(!SQOFFBOARD(sq + 9) && PieceCol(brd_pieces(sq + 9)).id == COLORS.BLACK.id) {
                    AddWhitePawnCaptureMove(sq, sq+9, brd_pieces(sq + 9))
                }
                if(SQOFFBOARD(sq + 11) && PieceCol(brd_pieces(sq + 11)).id == COLORS.BLACK.id) {
                    AddWhitePawnCaptureMove(sq, sq+11, brd_pieces(sq + 11))
                }

                if(brd_enPas != SQUARES.NO_SQ.id) {
                    if(sq + 9 == brd_enPas) {
                        AddEnPassantMove(MOVE(sq,sq + 9,PIECES.EMPTY.id,PIECES.EMPTY.id,MFLAGEP))
                    }
                    if(sq + 11 == brd_enPas) {
                        AddEnPassantMove(MOVE(sq,sq + 11,PIECES.EMPTY.id,PIECES.EMPTY.id,MFLAGEP))
                    }
                }
            }

            pceType = PIECES.wN.id // HACK to set for loop other pieces

        } else {
            pceType = PIECES.bP.id
            for(pceNum <- 0 until brd_pceNum(pceType)) {
                sq = brd_pList(PCEINDEX(pceType,pceNum))

                if(!SQOFFBOARD(sq - 9) && PieceCol(brd_pieces(sq - 9)).id == COLORS.WHITE.id) {
                    AddBlackPawnCaptureMove(sq, sq-9, brd_pieces(sq - 9))
                }

                if(!SQOFFBOARD(sq - 11) && PieceCol(brd_pieces(sq - 11)).id == COLORS.WHITE.id) {
                    AddBlackPawnCaptureMove(sq, sq-11, brd_pieces(sq - 11))
                }
                if(brd_enPas != SQUARES.NO_SQ.id) {
                    if(sq - 9 == brd_enPas) {
                        AddEnPassantMove(MOVE(sq,sq - 9,PIECES.EMPTY.id,PIECES.EMPTY.id,MFLAGEP))
                    }
                    if(sq - 11 == brd_enPas) {
                        AddEnPassantMove(MOVE(sq,sq - 11,PIECES.EMPTY.id,PIECES.EMPTY.id,MFLAGEP))
                    }
                }
            }

            pceType = PIECES.bN.id // HACK to set for loop other pieces
        }


        pceIndex = LoopSlideIndex(brd_side)
        pceIndex += 1
        pce = LoopSlidePce(pceIndex)

        while( pce != 0) {
            for(pceNum <- 0 until brd_pceNum(pce)) {
                sq = brd_pList(PCEINDEX(pce,pceNum))

                for(index <- 0 until DirNum(pce)) {
                    dir = PceDir(pce)(index)
                    t_sq = sq + dir

                    while(!SQOFFBOARD(t_sq)) {

                        if(brd_pieces(t_sq) != PIECES.EMPTY.id) {
                            if( PieceCol(brd_pieces(t_sq)).id == (brd_side^1)) {
                                AddCaptureMove(MOVE(sq, t_sq, brd_pieces(t_sq), PIECES.EMPTY.id, 0))
                            }
                            break
                        }
                        t_sq += dir
                    }
                }
            }
            pceIndex += 1
            pce = LoopSlidePce(pceIndex)
        }

        pceIndex = LoopNonSlideIndex(brd_side)
        pceIndex += 1
        pce = LoopNonSlidePce(pceIndex)

        //needs tested
        while( pce != 0) {
            for(pceNum <- 0 until brd_pceNum(pce)) {
                sq = brd_pList(PCEINDEX(pce,pceNum))

                for(index <- 0 until DirNum(pce)) {
                    dir = PceDir(pce)(index)
                    t_sq = sq + dir

                    if(!SQOFFBOARD(t_sq)) {
                        if(brd_pieces(t_sq) != PIECES.EMPTY.id) {
                            if (PieceCol(brd_pieces(t_sq)).id == (brd_side ^ 1)) {
                                AddCaptureMove(MOVE(sq, t_sq, brd_pieces(t_sq), PIECES.EMPTY.id, 0))
                            }
                            //continue
                        }
                    }
                }
            }
            pceIndex += 1
            pce = LoopNonSlidePce(pceIndex)
        }

    }
}
