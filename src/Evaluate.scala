import Defs._
import Board._ 
import PSQT._

object Evaluate {
    var RookOpenFile = 10
    var RookSemiOpenFile = 5
    var QueenOpenFile = 5
    var QueenSemiOpenFile = 3
    var BishopPair = 40

    var PawnRanksWhite = new Array[Int](10)
    var PawnRanksBlack = new Array[Int](10)

    var PawnIsolated = -10
    var PawnPassed = Array( 0, 5, 10, 20, 35, 60, 100, 200 )

    def MaterialDraw(): Boolean = {
        if (0 == brd_pceNum(PIECES.wR.id) && 0 == brd_pceNum(PIECES.bR.id) && 0 == brd_pceNum(PIECES.wQ.id) && 0 == brd_pceNum(PIECES.bQ.id)) {
            if (0 == brd_pceNum(PIECES.bB.id) && 0 == brd_pceNum(PIECES.wB.id)) {
                if (brd_pceNum(PIECES.wN.id) < 3 && brd_pceNum(PIECES.bN.id) < 3) {  return true }
            } else if (0 == brd_pceNum(PIECES.wN.id) && 0 == brd_pceNum(PIECES.bN.id)) {
                if (Math.abs(brd_pceNum(PIECES.wB.id) - brd_pceNum(PIECES.bB.id)) < 2) { return true }
            } else if ((brd_pceNum(PIECES.wN.id) < 3 && 0 == brd_pceNum(PIECES.wB.id)) || (brd_pceNum(PIECES.wB.id) == 1 && 0 == brd_pceNum(PIECES.wN.id))) {
                if ((brd_pceNum(PIECES.bN.id) < 3 && 0 == brd_pceNum(PIECES.bB.id)) || (brd_pceNum(PIECES.bB.id) == 1 && 0 == brd_pceNum(PIECES.bN.id)))  { return true }
            }
        } else if (0 == brd_pceNum(PIECES.wQ.id) && 0 == brd_pceNum(PIECES.bQ.id)) {
            if (brd_pceNum(PIECES.wR.id) == 1 && brd_pceNum(PIECES.bR.id) == 1) {
                if ((brd_pceNum(PIECES.wN.id) + brd_pceNum(PIECES.wB.id)) < 2 && (brd_pceNum(PIECES.bN.id) + brd_pceNum(PIECES.bB.id)) < 2)	{ return true }
            } else if (brd_pceNum(PIECES.wR.id) == 1 && 0 == brd_pceNum(PIECES.bR.id)) {
                if ((brd_pceNum(PIECES.wN.id) + brd_pceNum(PIECES.wB.id) == 0) && (((brd_pceNum(PIECES.bN.id) + brd_pceNum(PIECES.bB.id)) == 1) || ((brd_pceNum(PIECES.bN.id) + brd_pceNum(PIECES.bB.id)) == 2))) { return true }
            } else if (brd_pceNum(PIECES.bR.id) == 1 && 0 == brd_pceNum(PIECES.wR.id)) {
                if ((brd_pceNum(PIECES.bN.id) + brd_pceNum(PIECES.bB.id) == 0) && (((brd_pceNum(PIECES.wN.id) + brd_pceNum(PIECES.wB.id)) == 1) || ((brd_pceNum(PIECES.wN.id) + brd_pceNum(PIECES.wB.id)) == 2))) { return true }
            }
        }
        return false
    }

    var ENDGAME_MAT = 1 * PieceVal(PIECES.wR.id) + 2 * PieceVal(PIECES.wN.id) + 2 * PieceVal(PIECES.wP.id) + PieceVal(PIECES.wK.id)

    def PawnsInit() {
        var pce = 0
        var sq = 0
        
        for(index <- 0 until 10) {
            PawnRanksWhite(index) = RANKS.RANK_8.id
            PawnRanksBlack(index) = RANKS.RANK_1.id
        }

        pce = PIECES.wP.id
        for(pceNum <- 0 until brd_pceNum(pce)) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            if(RanksBrd(sq) < PawnRanksWhite(FilesBrd(sq)+1)) {
                PawnRanksWhite(FilesBrd(sq)+1) = RanksBrd(sq)
            }
        }

        pce = PIECES.bP.id
        for(pceNum <- 0 until brd_pceNum(pce) ) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            if(RanksBrd(sq) > PawnRanksBlack(FilesBrd(sq)+1)) {
                PawnRanksBlack(FilesBrd(sq)+1) = RanksBrd(sq)
            }
        }
    }

    def EvalPosition(): Int = {
        var pce = 0
        var sq = 0
        var file = 0
        var rank = 0
        
        var score = brd_material(COLORS.WHITE.id) - brd_material(COLORS.BLACK.id)
        
        if(0 == brd_pceNum(PIECES.wP.id) && 0 == brd_pceNum(PIECES.bP.id) && MaterialDraw()) {
            //return 0
        }

        PawnsInit()

        pce = PIECES.wP.id
        for(pceNum <- 0 until brd_pceNum(pce) ) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            score += PawnTable(SQ64(sq))
            file = FilesBrd(sq)+1
            rank = RanksBrd(sq)
            if(PawnRanksWhite(file-1)==RANKS.RANK_8.id && PawnRanksWhite(file+1)==RANKS.RANK_8.id) {
                //score += PawnIsolated
            }

            if(PawnRanksBlack(file-1)<=rank && PawnRanksBlack(file)<=rank && PawnRanksBlack(file+1)<=rank) {
                //score += PawnPassed(rank)
            }
        }

        pce = PIECES.bP.id
        for(pceNum <- 0 until brd_pceNum(pce)) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            score -= PawnTable(MIRROR64(SQ64(sq)))
            file = FilesBrd(sq)+1
            rank = RanksBrd(sq)
            if(PawnRanksBlack(file-1)==RANKS.RANK_1.id && PawnRanksBlack(file+1)==RANKS.RANK_1.id) {
                //score -= PawnIsolated
            }

            if(PawnRanksWhite(file-1)>=rank && PawnRanksWhite(file)>=rank && PawnRanksWhite(file+1)>=rank) {
                //score -= PawnPassed(7-rank)
            }
        }

        pce = PIECES.wN.id
        for(pceNum <- 0 until brd_pceNum(pce) ) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            //score += KnightTable(SQ64(sq))
        }

        pce = PIECES.bN.id
        for(pceNum <- 0 until brd_pceNum(pce) ) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            //score -= KnightTable(MIRROR64(SQ64(sq)))
        }

        pce = PIECES.wB.id
        for(pceNum <- 0 until brd_pceNum(pce) ) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            //score += BishopTable(SQ64(sq))
        }

        pce = PIECES.bB.id
        for(pceNum <- 0 until brd_pceNum(pce) ) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            //score -= BishopTable(MIRROR64(SQ64(sq)))
        }

        pce = PIECES.wR.id
        for(pceNum <- 0 until brd_pceNum(pce) ) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            //score += RookTable(SQ64(sq))
            file = FilesBrd(sq)+1
            if(PawnRanksWhite(file)==RANKS.RANK_8.id) {
                if(PawnRanksBlack(file)==RANKS.RANK_1.id) {
                    //score += RookOpenFile
                } else  {
                    //score += RookSemiOpenFile
                }
            }
        }

        pce = PIECES.bR.id
        for(pceNum <- 0 until brd_pceNum(pce)) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            //score -= RookTable(MIRROR64(SQ64(sq)))
            file = FilesBrd(sq)+1
            if(PawnRanksBlack(file)==RANKS.RANK_1.id) {
                if(PawnRanksWhite(file)==RANKS.RANK_8.id) {
                    //score -= RookOpenFile
                } else  {
                    //score -= RookSemiOpenFile
                }
            }
        }

        pce = PIECES.wQ.id
        for(pceNum <- 0 until brd_pceNum(pce)) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            //score += RookTable(SQ64(sq))
            file = FilesBrd(sq)+1
            if(PawnRanksWhite(file)==RANKS.RANK_8.id) {
                if(PawnRanksBlack(file)==RANKS.RANK_1.id) {
                    //score += QueenOpenFile
                } else  {
                    //score += QueenSemiOpenFile
                }
            }
        }

        pce = PIECES.bQ.id
        for(pceNum <- 0 until brd_pceNum(pce)) {
            sq = brd_pList(PCEINDEX(pce,pceNum))
            //score -= RookTable(MIRROR64(SQ64(sq)))
            file = FilesBrd(sq)+1
            if(PawnRanksBlack(file)==RANKS.RANK_1.id) {
                if(PawnRanksWhite(file)==RANKS.RANK_8.id) {
                    //score -= QueenOpenFile
                } else  {
                    //score -= QueenSemiOpenFile
                }
            }
        }

        pce = PIECES.wK.id
        sq = brd_pList(PCEINDEX(pce,0))

        if( brd_material(COLORS.BLACK.id) <= ENDGAME_MAT ) {
            //score += KingE(SQ64(sq))
        } else {
            //score += KingO(SQ64(sq))
        }

        pce = PIECES.bK.id
        sq = brd_pList(PCEINDEX(pce,0))

        if( brd_material(COLORS.WHITE.id) <= ENDGAME_MAT ) {
            //score -= KingE(MIRROR64(SQ64(sq)))
        } else {
            //score -= KingO(MIRROR64(SQ64(sq)))
        }

        //if(brd_pceNum(PIECES.wB.id) >= 2) score += BishopPair
        //if(brd_pceNum(PIECES.bB.id) >= 2) score -= BishopPair

        if(brd_side == COLORS.WHITE.id) {
            return score
        } else {
            return -score
        }
    }
}
