import Defs._
import util.control.Breaks._

object Board {
    //board variables
    var brd_side = COLORS.WHITE.id
    var brd_pieces = new Array[Int](BRD_SQ_NUM)
    var brd_enPas = SQUARES.NO_SQ.id
    var brd_fiftyMove = 0
    var brd_ply = 0
    var brd_hisPly = 0
    var brd_castlePerm = 0
    var brd_posKey = 0
    var brd_pceNum = new Array[Int](13)
    var brd_material = new Array[Int](2)
    var brd_pList = new Array[Int](14 * 10)

    var brd_history = new Array[Int](1)

    var brd_bookLines = new Array[Int](1)

    var brd_moveList = new Array[Int](MAXDEPTH * MAXPOSITIONMOVES)
    var brd_moveScores = new Array[Int](MAXDEPTH * MAXPOSITIONMOVES)
    var brd_moveListStart = new Array[Int](MAXDEPTH)

    var brd_PvTable = new Array[Int](1)
    var brd_PvArray = new Array[Int](MAXDEPTH)
    var brd_searchHistory = new Array[Int](14 * BRD_SQ_NUM)
    var brd_searchKillers = new Array[Int](3 * MAXDEPTH)

    //board functions
    // board functions

    def BoardToFen() {
        var fenStr = ""
        var sq = 0
        var piece = 0
        var emptyCount = 0

        for (rank <- RANKS.RANK_8.id to RANKS.RANK_1.id by -1) {
            emptyCount = 0
            for (file <- FILES.FILE_A.id to FILES.FILE_H.id) {
                sq = FR2SQ(file,rank)
                piece = brd_pieces(sq)
                if (piece == PIECES.EMPTY.id) {
                    emptyCount+=1
                } else {
                    if (emptyCount!=0) {
                        //fenStr += String.fromCharCode("0".charCodeAt() + emptyCount)
                    }
                    emptyCount = 0
                    fenStr += PceChar(piece)
                }
            }
            if (emptyCount!=0) {
                //fenStr += String.fromCharCode("0".charCodeAt() + emptyCount)
            }

            if (rank != RANKS.RANK_1.id) {
                fenStr += "/"
            } else {
                fenStr += " "
            }
        }

        fenStr += SideChar(brd_side) + " "
        if (brd_enPas == SQUARES.NO_SQ.id) {
            fenStr += "- "
        } else {
            //fenStr += PrSq(brd_enPas) + " "
        }

        if (brd_castlePerm == 0) {
            fenStr += "- "
        } else {
            if ((brd_castlePerm & CASTLEBIT.WKCA.id) != 0) fenStr += "K"
            if ((brd_castlePerm & CASTLEBIT.WQCA.id) != 0) fenStr += "Q"
            if ((brd_castlePerm & CASTLEBIT.BKCA.id) != 0) fenStr += "k"
            if ((brd_castlePerm & CASTLEBIT.BQCA.id) != 0) fenStr += "q"
        }
        fenStr += " "
        fenStr += brd_fiftyMove
        fenStr += " "
        var tempHalfMove = brd_hisPly
        if (brd_side == COLORS.BLACK.id) {
            tempHalfMove-=1
        }
        fenStr += tempHalfMove/2

        //return fenStr
    }

    def CheckBoard(): Boolean = {

        val t_pceNum = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        val t_material = Array(0, 0)

        var t_piece,sq120=0

        // check piece lists
        for (t_piece <- PIECES.wP.id to PIECES.bK.id) {
            for (t_pce_num <- 0 until brd_pceNum(t_piece)) {
                sq120 = brd_pList(PCEINDEX(t_piece,t_pce_num))
                if (brd_pieces(sq120) != t_piece) {
                   println("Error Pce Lists")
                    return false
                }
            }
        }

        // check piece count and other counters
        for (sq64 <- 0 until 64) {
            sq120 = SQ120(sq64)
            t_piece = brd_pieces(sq120)
            t_pceNum(t_piece)+=1
            t_material(PieceCol(t_piece).id) += PieceVal(t_piece)
        }

        for (t_piece <- PIECES.wP.id to PIECES.bK.id) {
            if (t_pceNum(t_piece)!=brd_pceNum(t_piece)) {
                println("Error t_pceNum")
                return false
            }
        }

        if (t_material(COLORS.WHITE.id) != brd_material(COLORS.WHITE.id) || t_material(COLORS.BLACK.id)!=brd_material(COLORS.BLACK.id)) {
            println("Error t_material")
            return false
        }
        if (brd_side!=COLORS.WHITE.id && brd_side!=COLORS.BLACK.id) {
            println("Error brd_side")
            return false
        }
        //if (GeneratePosKey()!=brd_posKey) {
        //    println("Error brd_posKey")
        //    return false
       // }


        true
    }

    def printGameLine(): String = {
        val gameLine = ""
        for (moveNum <- 0 until brd_hisPly) {
            //gameLine += PrMove(brd_history(moveNum).move) + " "
        }
        println("Game Line: " + gameLine)
        gameLine.trim()
    }

    /*def LineMatch(BookLine,gameline) {
        //println("Matching " + gameline + " with " + BookLine + " len = " + gameline.length)
        for (var len = 0 len < gameline.length +=1len) {
            //println("Char Matching " + gameline(len) + " with " + BookLine(len))
            if (len>=BookLine.length) { /*println("no match")*/ return false	}
            if (gameline(len) != BookLine(len)) { /*println("no match") */return false	}
        }
        //println("Match")
        return true
    }*/

    /*def BookMove() {

        var gameLine = printGameLine()
        var bookMoves = ()

        var lengthOfLineHack = gameLine.length

        if (gameLine.length == 0) lengthOfLineHack--

        for (var bookLineNum = 0 bookLineNum <brd_bookLines.length +=1bookLineNum) {

            if (LineMatch(brd_bookLines(bookLineNum),gameLine) == true) {
                var move = brd_bookLines(bookLineNum).substr(lengthOfLineHack + 1, 4)
                //println("Parsing book move:" + move)
                if (move.length==4) {
                    var from = SqFromAlg(move.substr(0,2))
                    var to = SqFromAlg(move.substr(2,2))
                    //println("from:"+from+" to:"+to)
                    varInternalMove = ParseMove(from,to)
                    //println("varInternalMove:" + PrMove(varInternalMove))
                    bookMoves.push(varInternalMove)
                }
            }

        }

        println("Total + " + bookMoves.length + " moves in array")

        if (bookMoves.length==0) return NOMOVE

        var num = Math.floor(Math.random()*bookMoves.length)

        return bookMoves(num)
    }*/

    def PrintPceLists() {
        for (piece <- PIECES.wP.id to PIECES.bK.id) {
            for (pceNum <- 0 until piece) {
                //println("Piece " + PceChar(piece) + " on " + PrSq(brd_pList(PCEINDEX(piece,pceNum))))
            }
        }

    }

    def UpdateListsMaterial() {
        var piece,sq,color = 0

        for (index <- 0 until BRD_SQ_NUM) {
            sq = index
            piece = brd_pieces(index)
            if (piece != SQUARES.OFFBOARD.id && piece != PIECES.EMPTY.id) {
                color = PieceCol(piece).id

                brd_material(color) += PieceVal(piece)

                sq = brd_pList(PCEINDEX(piece,brd_pceNum(piece)))
                brd_pceNum(piece) += 1
            }
        }
    }

    def GeneratePosKey(): Int = {
        var finalKey = 0
        var piece = PIECES.EMPTY.id

        // pieces
        for (sq <- 0 until BRD_SQ_NUM) {
            piece = brd_pieces(sq)
            if (piece != PIECES.EMPTY.id && piece != SQUARES.OFFBOARD.id) {
                finalKey ^= PieceKeys((piece * 120) + sq)
            }
        }

        if (brd_side == COLORS.WHITE.id) {
            finalKey ^= SideKey
        }

        if (brd_enPas != SQUARES.NO_SQ.id) {
            finalKey ^= PieceKeys(brd_enPas)
        }

        finalKey ^= CastleKeys(brd_castlePerm)

        finalKey
    }

    def PrintBoard() {

        var sq,piece = 0

        println("\nGame Board:\n")

        for (rank <- RANKS.RANK_8.id to RANKS.RANK_1.id by -1) {
            var line =(rank+1) + "  "
            for (file <- FILES.FILE_A.id to FILES.FILE_H.id) {
                sq = FR2SQ(file,rank)
                piece = brd_pieces(sq)
                line += (" " + PceChar(piece) + " ")
            }
            println(line)
        }

        println("")
        var line = "   "
        for (file <- FILES.FILE_A.id to FILES.FILE_H.id) {
            //line += (" " + String.fromCharCode("a".charCodeAt() + file) + " ")
        }
        println(line)
        println("side:" + SideChar(brd_side) )
        println("enPas:" + brd_enPas)
        line = ""
        if ((brd_castlePerm & CASTLEBIT.WKCA.id) != 0) line += "K"
        if ((brd_castlePerm & CASTLEBIT.WQCA.id) != 0) line += "Q"
        if ((brd_castlePerm & CASTLEBIT.BKCA.id) != 0) line += "k"
        if ((brd_castlePerm & CASTLEBIT.BQCA.id) != 0) line += "q"

        println("castle:" + line)
        //println("key:" + brd_posKey.toString(16))
        //PrintPceLists()
    }

    def ResetBoard() {
        for (index <- 0 until BRD_SQ_NUM ) {
            brd_pieces(index) = SQUARES.OFFBOARD.id
        }

        for (index <- 0 until 64 ) {
            brd_pieces(SQ120(index)) = PIECES.EMPTY.id
        }

        for (index <- 0 until 14 * 120 ) {
            brd_pList(index) = PIECES.EMPTY.id
        }

        for (index <- 0 until 2 ) {
            brd_material(index) = 0
        }

        for (index <- 0 until 13) {
            brd_pceNum(index) = 0
        }

        brd_side = COLORS.BOTH.id
        brd_enPas = SQUARES.NO_SQ.id
        brd_fiftyMove = 0
        brd_ply = 0
        brd_hisPly = 0
        brd_castlePerm = 0
        brd_posKey = 0
        brd_moveListStart(brd_ply) = 0

    }

    def ParseFen(fen : String) {

        val rank = RANKS.RANK_8.id
        var file = FILES.FILE_A.id
        val piece = 0
        var count = 0
        var sq64 = 0
        var sq120 = 0
        var fenCnt = 0
        //ResetBoard()

        while ((rank >= RANKS.RANK_1.id) && fenCnt < fen.length) {
            count = 1
            /*switch (fen(fenCnt)) {
                case "p": piece = PIECES.bP break
                case "r": piece = PIECES.bR break
                case "n": piece = PIECES.bN break
                case "b": piece = PIECES.bB break
                case "k": piece = PIECES.bK.id break
                case "q": piece = PIECES.bQ break
                case "P": piece = PIECES.wP.id break
                case "R": piece = PIECES.wR break
                case "N": piece = PIECES.wN break
                case "B": piece = PIECES.wB break
                case "K": piece = PIECES.wK break
                case "Q": piece = PIECES.wQ break

                case "1":
                case "2":
                case "3":
                case "4":
                case "5":
                case "6":
                case "7":
                case "8":
                    piece = PIECES.EMPTY.id
                count = fen(fenCnt).charCodeAt() - "0".charCodeAt()
                break

                case "/":
                case " ":
                    rank-=1
                file = FILES.FILE_A.id
                fenCnt+=1
                continue

                default:
                    printf("FEN error \n")
                return
            }
            */
            for (i <- 0 until count) {
                sq64 = rank * 8 + file
                sq120 = SQ120(sq64)
                if (piece != PIECES.EMPTY.id) {
                    brd_pieces(sq120) = piece
                }
                file+=1
            }
            fenCnt+=1
        }

        //brd_side = (fen(fenCnt) == "w") ? COLORS.WHITE.id : COLORS.BLACK.id
        fenCnt += 2

        /*for (i <- 0 until 4) {
            if (fen(fenCnt) == " ") {
                break
            }
            switch(fen(fenCnt)) {
                case "K": brd_castlePerm |= CASTLEBIT.WKCA break
                case "Q": brd_castlePerm |= CASTLEBIT.WQCA break
                case "k": brd_castlePerm |= CASTLEBIT.BKCA break
                case "q": brd_castlePerm |= CASTLEBIT.BQCA break
                default:	     break
            }
            fenCnt+=1
        }*/
        fenCnt+=1

        if (fen(fenCnt) != "-") {
            //file = fen(fenCnt).charCodeAt() - "a".charCodeAt()
            //rank = fen(fenCnt+1).charCodeAt() - "1".charCodeAt()
            println("fen(fenCnt):" + fen(fenCnt) + " File:" + file + " Rank:" + rank)
            brd_enPas = FR2SQ(file,rank)
        }

        brd_posKey = GeneratePosKey()
        UpdateListsMaterial()
    }

    def SqAttacked(sq : Int, side : Int): Boolean = {
        var pce = 0
        var t_sq = 0
        var dir = 0

        if (side == COLORS.WHITE.id) {
            if (brd_pieces(sq-11) == PIECES.wP.id || brd_pieces(sq-9) == PIECES.wP.id) {
                return true
            }
        } else {
            if (brd_pieces(sq+11) == PIECES.bP.id || brd_pieces(sq+9) == PIECES.bP.id) {
                return true
            }
        }

        for (index <- 0 until 8) {
            pce = brd_pieces(sq + KnDir(index))
            if (pce != SQUARES.OFFBOARD.id && PieceKnight(pce) && PieceCol(pce).id == side) {
                return true
            }
        }

        for (index <- 0 until 4) {
            dir = RkDir(index)
            t_sq = sq + dir
            pce = brd_pieces(t_sq)
            while(pce != SQUARES.OFFBOARD.id) {
                if (pce != PIECES.EMPTY.id) {
                    if (PieceRookQueen(pce) && PieceCol(pce).id == side) {
                        return true
                    }
                    break
                }
                t_sq += dir
                pce = brd_pieces(t_sq)
            }
        }

        for (index <- 0 until 4) {
            dir = BiDir(index)
            t_sq = sq + dir
            pce = brd_pieces(t_sq)
            while(pce != SQUARES.OFFBOARD.id) {
                if (pce != PIECES.EMPTY.id) {
                    if (PieceBishopQueen(pce) && PieceCol(pce).id == side) {
                        return true
                    }
                    break
                }
                t_sq += dir
                pce = brd_pieces(t_sq)
            }
        }

        for (index <- 0 until 8) {
            pce = brd_pieces(sq + KiDir(index))
            if (pce != SQUARES.OFFBOARD.id && PieceKing(pce) && PieceCol(pce).id == side) {
                return true
            }
        }

        false
    }

    def PrintSqAttacked(): Unit = {
        var sq = 0
        var piece : String = ""

        println("\nAttacked:\n")

        for (rank <- RANKS.RANK_8.id to RANKS.RANK_1.id by -1) {
            var line =(rank+1) + "  "
            for (file <- FILES.FILE_A.id to FILES.FILE_H.id) {
                sq = FR2SQ(file,rank)
                if (SqAttacked(sq, COLORS.BLACK.id)) piece = "X"
                else piece = "-"
                line += (" " + piece + " ")
            }
            println(line)
        }

        println("")
    }
}
