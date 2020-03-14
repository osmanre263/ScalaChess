import Board._

object Defs {
    var BRD_SQ_NUM = 120
    //                                {val      0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12        }
    object PIECES extends Enumeration {val  EMPTY, wP, wN, wB, wR, wQ, wK, bP, bN, bB, bR, bQ, bK = Value}

    var MAXGAMEMOVES = 2048
    var MAXPOSITIONMOVES = 256
    var MAXDEPTH = 64

    var INFINITE = 30000
    var MATE = 29000

    var START_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    object FILES extends Enumeration {val FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H, FILE_NONE = Value}

    object RANKS extends Enumeration  {val RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8, RANK_NONE = Value}

    object COLORS extends Enumeration {val WHITE, BLACK, BOTH = Value}

    object SQUARES extends Enumeration {
        val A1 = Value(21); val B1 = Value(22); var C1 = Value(23); val D1 = Value(24); val E1 = Value(25); val F1 = Value(26); val G1 = Value(27); val H1 = Value(28)
        val A8 = Value(91); val B8 = Value(92); val C8 = Value(93); val D8 = Value(94); val E8 = Value(95); val F8 = Value(96); val G8 = Value(97); val H8 = Value(98)
        val NO_SQ = Value(99); val OFFBOARD = Value(100)
    }

    object CASTLEBIT extends Enumeration {
        val WKCA = Value(1)
        val WQCA = Value(2)
        val BKCA = Value(4)
        val BQCA = Value(8)
    }

    var FilesBrd = new Array[Int](BRD_SQ_NUM)
    var RanksBrd = new Array[Int](BRD_SQ_NUM)

    var Sq120ToSq64 = new Array[Int](BRD_SQ_NUM)
    var Sq64ToSq120 = new Array[Int](64)

    var PceChar = ".PNBRQKpnbrqk"
    var SideChar = "wb-"
    var RankChar = "12345678"
    var FileChar = "abcdefgh"

    var PieceBig = Array ( false, false, true, true, true, true, true, false, true, true, true, true, true )
    var PieceMaj = Array( false, false, false, false, true, true, true, false, false, false, true, true, true )
    var PieceMin = Array( false, false, true, true, false, false, false, false, true, true, false, false, false )
    var PieceVal = Array( 0, 100, 325, 325, 550, 1000, 50000, 100, 325, 325, 550, 1000, 50000)

    var PieceCol = Array(
        COLORS.BOTH.id, COLORS.WHITE.id, COLORS.WHITE.id, COLORS.WHITE.id, COLORS.WHITE.id, COLORS.WHITE.id, COLORS.WHITE.id,
        COLORS.BLACK.id, COLORS.BLACK.id, COLORS.BLACK.id, COLORS.BLACK.id, COLORS.BLACK.id, COLORS.BLACK.id
    )

    var PiecePawn = Array(false, true, false, false, false, false, false, true, false, false, false, false, false)
    var PieceKnight = Array(false, false, true, false, false, false, false, false, true, false, false, false, false)
    var PieceKing = Array(false, false, false, false, false, false, true, false, false, false, false, false, true)
    var PieceRookQueen = Array(false, false, false, false, true, true, false, false, false, false, true, true, false)
    var PieceBishopQueen = Array(false, false, false, true, false, true, false, false, false, true, false, true, false)
    var PieceSlides = Array(false, false, false, true, true, true, false, false, false, true, true, true, false)

    var KnDir = Array(-8, -19,	-21, -12, 8, 19, 21, 12)
    var RkDir = Array(-1, -10,	1, 10)
    var BiDir = Array(-9, -11, 11, 9)
    var KiDir = Array(-1, -10,	1, 10, -9, -11, 11, 9)

    var DirNum = Array(0, 0, 8, 4, 4, 8, 8, 0, 8, 4, 4, 8, 8)
    var PceDir = Array(Array(0), Array(0), KnDir, BiDir, RkDir, KiDir, KiDir, Array(0), KnDir, BiDir, RkDir, KiDir, KiDir)
    var LoopSlidePce = Array(PIECES.wB.id, PIECES.wR.id, PIECES.wQ.id, 0, PIECES.bB.id, PIECES.bR.id, PIECES.bQ.id, 0)
    var LoopNonSlidePce = Array(PIECES.wN.id, PIECES.wK.id, 0, PIECES.bN.id, PIECES.bK.id, 0)
    var LoopSlideIndex = Array(0, 4)
    var LoopNonSlideIndex = Array( 0, 3)
    var Kings = Array(PIECES.wK, PIECES.bK)

    var PieceKeys = new Array[Int](14 * 120)
    var SideKey = 0
    var CastleKeys = new Array[Int](16)

    var Mirror64 = Array(
        56, 57, 58, 59, 60, 61, 62, 63,
        48, 49, 50, 51, 52, 53, 54, 55,
        40, 41, 42, 43, 44, 45, 46, 47,
        32, 33, 34, 35, 36, 37, 38, 39,
        24, 25, 26, 27, 28, 29, 30, 31,
        16, 17, 18, 19, 20, 21, 22, 23,
         8,  9, 10, 11, 12, 13, 14, 15,
         0,  1,  2,  3,  4,  5,  6,  7
    )

    var CastlePerm = Array(
        15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
        15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
        15, 13, 15, 15, 15, 12, 15, 15, 14, 15,
        15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
        15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
        15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
        15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
        15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
        15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
        15,  7, 15, 15, 15,  3, 15, 15, 11, 15,
        15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
        15, 15, 15, 15, 15, 15, 15, 15, 15, 15
    )

    def FROMSQ(m : Int): Int = m & 0x7F
    def TOSQ(m : Int): Int = (m >> 7) & 0x7F
    def CAPTURED(m : Int): Int = (m >> 14) & 0xF
    def PROMOTED(m : Int): Int = (m >> 20) & 0xF

    var MFLAGEP = 0x40000
    var MFLAGPS = 0x80000
    var MFLAGCA = 0x1000000

    var MFLAGCAP = 0x7C000
    var MFLAGPROM = 0xF00000

    var NOMOVE = 0

    var PVENTRIES = 10000

    def PCEINDEX(pce : Int, pceNum : Int): Int = pce*10+pceNum

    def FR2SQ(f : Int,r : Int): Int = (21+f)+(r*10)

    def SQ64(sq120 : Int): Int = Sq120ToSq64(sq120)

    def SQ120(sq64 : Int): Int =  Sq64ToSq120(sq64)

    def MIRROR64(sq : Int): Int =  Mirror64(sq)

    def RAND_32(): Int = {
       (((Math.random()*255)+1).floor.toInt << 23) |(((Math.random()*255)+1).floor.toInt << 16) |(((Math.random()*255)+1).floor.toInt << 8) |((Math.random()*255) + 1).floor.toInt + 1
    }

    def SQOFFBOARD(sq : Int): Boolean = {
        if (FilesBrd(sq) == SQUARES.OFFBOARD.id) return true
        false
    }

    def HASH_PCE(pce : Int,sq : Int): Unit = brd_posKey ^= PieceKeys(pce * 120 + sq)
    def HASH_CA(): Unit = brd_posKey ^= CastleKeys(brd_castlePerm)
    def HASH_SIDE(): Unit = brd_posKey ^= SideKey
    def HASH_EP(): Unit = brd_posKey ^= PieceKeys(brd_enPas)

    /*var GameController = {}
    GameController.EngineSide = COLORS.BOTH
    GameController.PlayerSide = COLORS.BOTH
    GameController.BoardFlipped = false
    GameController.GameOver = false
    GameController.BookLoaded = false
    GameController.GameSaved = true*/
}
