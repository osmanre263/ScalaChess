package com.scala.chess

import Board._
import Defs._
import MakeMove._
import MoveGen._

import scala.util.control.Breaks.{break, breakable}

object PVTable {
    def GetPvLine(depth : Int) : Int = {
        //println("GetPvLine")

        var move = ProbePvTable()
        var count = 0
        breakable {
        while(move != NOMOVE && count < depth) {
            if (MoveExists(move)) {
                MakeMove(move)
                brd_PvArray(count) = move
                count += 1
            } else {
                break
            }
            move = ProbePvTable()
        }}

        while(brd_ply > 0) {
            TakeMove()
        }
        return count
    }

    def StorePvMove(move : Int) {
        val index = brd_posKey % PVENTRIES

        brd_PvTable(index).move = move
        brd_PvTable(index).posKey = brd_posKey
    }

    def ProbePvTable(): Int = {
        val index = brd_posKey % PVENTRIES

        if (brd_PvTable(index).posKey == brd_posKey ) {
            return brd_PvTable(index).move
        }

        return NOMOVE
    }

    def ClearPvTable() {
        brd_PvTable = Array.fill[PvMove](PVENTRIES)(new PvMove())
    }

    var HashSize = 0x100000 * 16

    /*def ClearHashTable(S_HASHTABLE table) {

        S_HASHENTRY tableEntry

        for (tableEntry <- table.pTable until table.pTable + table.numEntries) {
            tableEntry.posKey = null
            tableEntry.move = NOMOVE
            tableEntry.depth = 0
            tableEntry.score = 0
            tableEntry.flags = 0
        }
        table.newWrite=0
    }*/

    /*def InitHashTable(table : S_HASHTABLE,MB : Int) {

        var HashSize = 0x100000  MB
        table.numEntries = HashSize / sizeof(S_HASHENTRY)
        table.numEntries -= 2

        if (table.pTable != null) {
            free(table.pTable)
        }

        table.pTable = (S_HASHENTRY) malloc(table.numEntries  sizeof(S_HASHENTRY))
        if (table.pTable == null) {
            printf("Hash Allocation Failed, trying %dMB...\n",MB/2)
            InitHashTable(table,MB/2)
        } else {
            ClearHashTable(table)
            printf("HashTable init complete with %d entries\n",table.numEntries)
        }

    }*/

    /*def ProbeHashEntry(_move : Int, _score : Int, alpha : Int,beta : Int,depth : Int): Boolean = {
        var score = _score
        var move = _move
        var index = brd_posKey % brd_HashTable.numEntries

        //ASSERT(index >= 0 && index <= brd_HashTable.numEntries - 1)
        //ASSERT(depth>=1&&depth<MAXDEPTH)
        //ASSERT(alpha<beta)
        //ASSERT(alpha>=-INFINITE&&alpha<=INFINITE)
        //ASSERT(beta>=-INFINITE&&beta<=INFINITE)
        //ASSERT(brd_ply>=0&&brd_ply<MAXDEPTH)

        if (brd_HashTable.pTable(index).posKey == brd_posKey ) {
            move = brd_HashTable.pTable(index).move
            if (brd_HashTable.pTable(index).depth >= depth){
                brd_HashTable.hit += 1

                //ASSERT(brd_HashTable.pTable(index).depth>=1&&brd_HashTable.pTable(index).depth<MAXDEPTH)
                //ASSERT(brd_HashTable.pTable(index).flags>=HFALPHA&&brd_HashTable.pTable(index).flags<=HFEXACT)

                score = brd_HashTable.pTable(index).score
                if (score > ISMATE) score -= brd_ply
                else if (score < -ISMATE) score += brd_ply

                brd_HashTable.pTable(index).flags match {

                    //ASSERT(score>=-INFINITE&&score<=INFINITE)

                    case HFALPHA =>
                        if (score <= alpha) {
                            score = alpha
                            return true
                         }
                    case HFBETA =>
                        if (score>=beta) {
                            score = beta
                            return true
                        }
                    case HFEXACT =>
                        return true
                    case _ =>
                }
            }
        }

        return FALSE
    }*/

    /*def StoreHashEntry(_move : Int,_score : Int,flags : Int,depth : Int) {
        var score = _score
        var move = _move
        var index = brd_posKey % brd_HashTable.numEntries

        //ASSERT(index >= 0 && index <= brd_HashTable.numEntries - 1)
        //ASSERT(depth>=1&&depth<MAXDEPTH)
        //ASSERT(flags>=HFALPHA&&flags<=HFEXACT)
        //ASSERT(score>=-INFINITE&&score<=INFINITE)
        //ASSERT(brd_ply>=0&&brd_ply<MAXDEPTH)

        if (brd_HashTable.pTable(index).posKey == 0) {
            brd_HashTable.newWrite += 1
        } else {
            brd_HashTable.overWrite += 1
        }

        if (score > ISMATE) score += brd_ply
        else if (score < -ISMATE) score -= brd_ply

        brd_HashTable.pTable(index).move = move
        brd_HashTable.pTable(index).posKey = brd_posKey
        brd_HashTable.pTable(index).flags = flags
        brd_HashTable.pTable(index).score = score
        brd_HashTable.pTable(index).depth = depth
    }*/
}
