import Board._
import Defs._
import util.control.Breaks.break

object MakeMove {
    def ClearPiece(sq : Int) {

        val pce = brd_pieces(sq)
        val col = PieceCol(pce).id
        var t_pceNum = -1

        HASH_PCE(pce,sq)

        brd_pieces(sq) = PIECES.EMPTY.id
        brd_material(col) -= PieceVal(pce)

        for(index <- 0 until brd_pceNum(pce)) {
            if (brd_pList(PCEINDEX(pce,index)) == sq) {
                t_pceNum = index
                break
            }
        }

        brd_pceNum(pce) -= 1
        brd_pList(PCEINDEX(pce,t_pceNum)) = brd_pList(PCEINDEX(pce,brd_pceNum(pce)))
    }

    def AddPiece(sq : Int, pce : Int) {

        val col = PieceCol(pce).id

        HASH_PCE(pce,sq)

        brd_pieces(sq) = pce
        brd_material(col) += PieceVal(pce)
        brd_pList(PCEINDEX(pce,brd_pceNum(pce))) = sq
        brd_pceNum(pce) += 1
    }

    def MovePiece(from : Int, to : Int) {
        val pce = brd_pieces(from)

        HASH_PCE(pce,from)
        brd_pieces(from) = PIECES.EMPTY.id

        HASH_PCE(pce,to)
        brd_pieces(to) = pce

        for(index <- 0 until brd_pceNum(pce) ) {
            if (brd_pList(PCEINDEX(pce,index)) == from) {
                brd_pList(PCEINDEX(pce,index)) = to
                break
            }
        }
    }

    def MakeMove(move : Int): Boolean = {

        val from = FROMSQ(move)
        val to = TOSQ(move)
        val side = brd_side

        brd_history(brd_hisPly).posKey = brd_posKey

        if ( (move & MFLAGEP) != 0) {
            if (side == COLORS.WHITE.id) {
                ClearPiece(to-10)
            } else {
                ClearPiece(to+10)
            }
        } else if ( (move & MFLAGCA) != 0) {
            /*switch(to) {
                case SQUARES.C1.id:
                MovePiece(SQUARES.A1.id, SQUARES.D1.id)
                break
                case SQUARES.C8.id:
                    MovePiece(SQUARES.A8, SQUARES.D8.id)
                break
                case SQUARES.G1.id:
                    MovePiece(SQUARES.H1.id, SQUARES.F1.id)
                break
                case SQUARES.G8.id:
                    MovePiece(SQUARES.H8.id, SQUARES.F8.id)
                break
                default: break
            }*/
            println("hi")
        }

        if (brd_enPas != SQUARES.NO_SQ.id) HASH_EP()
        HASH_CA()

        brd_history(brd_hisPly).move = move
        brd_history(brd_hisPly).fiftyMove = brd_fiftyMove
        brd_history(brd_hisPly).enPas = brd_enPas
        brd_history(brd_hisPly).castlePerm = brd_castlePerm

        brd_castlePerm &= CastlePerm(from)
        brd_castlePerm &= CastlePerm(to)
        brd_enPas = SQUARES.NO_SQ.id

        HASH_CA()

        val captured = CAPTURED(move)
        brd_fiftyMove += 1

        if (captured != PIECES.EMPTY.id) {
            ClearPiece(to)
            brd_fiftyMove = 0
        }

        brd_hisPly += 1
        brd_ply += 1

        if (PiecePawn(brd_pieces(from))) {
            brd_fiftyMove = 0
            if ( (move & MFLAGPS) != 0) {
                if (side==COLORS.WHITE.id) {
                    brd_enPas=from+10
                } else {
                    brd_enPas=from-10
                }
                HASH_EP()
            }
        }

        MovePiece(from, to)

        val prPce = PROMOTED(move)
        if (prPce != PIECES.EMPTY.id)   {
            ClearPiece(to)
            AddPiece(to, prPce)
        }

        brd_side ^= 1
        HASH_SIDE()


        if (SqAttacked(brd_pList(PCEINDEX(Kings(side).id,0)), brd_side))  {
            TakeMove()
            return false
        }

        return true
    }

    def TakeMove() {

        brd_hisPly -= 1
        brd_ply -= 1

        val move = brd_history(brd_hisPly).move
        val from = FROMSQ(move)
        val to = TOSQ(move)

        if (brd_enPas != SQUARES.NO_SQ.id) HASH_EP()
        HASH_CA()

        brd_castlePerm = brd_history(brd_hisPly).castlePerm
        brd_fiftyMove = brd_history(brd_hisPly).fiftyMove
        brd_enPas = brd_history(brd_hisPly).enPas

        if (brd_enPas != SQUARES.NO_SQ.id) HASH_EP()
        HASH_CA()

        brd_side ^= 1
        HASH_SIDE()

        if ( (MFLAGEP & move) != 0) {
            if (brd_side == COLORS.WHITE.id) {
                AddPiece(to-10, PIECES.bP.id)
            } else {
                AddPiece(to+10, PIECES.wP.id)
            }
        } else if ( (MFLAGCA & move) != 0) {
            /*switch(to) {
                case SQUARES.C1.id: MovePiece(SQUARES.D1.id, SQUARES.A1.id) break
                case SQUARES.C8.id: MovePiece(SQUARES.D8.id, SQUARES.A8) break
                case SQUARES.G1.id: MovePiece(SQUARES.F1.id, SQUARES.H1.id) break
                case SQUARES.G8.id: MovePiece(SQUARES.F8.id, SQUARES.H8.id) break
                default: break
            }*/
            println("hi")
        }

        MovePiece(to, from)

        val captured = CAPTURED(move)
        if (captured != PIECES.EMPTY.id) {
            AddPiece(to, captured)
        }

        if (PROMOTED(move) != PIECES.EMPTY.id)   {
            ClearPiece(from)
            val pce = if (PieceCol(PROMOTED(move)).id == COLORS.WHITE.id) PIECES.wP.id else PIECES.bP.id
            AddPiece(from, pce)
        }
    }
}