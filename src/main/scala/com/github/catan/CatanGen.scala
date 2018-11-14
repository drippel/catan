package com.github.catan

import com.github.catan.ascii.HexPrinter
import org.apache.commons.math3.random.MersenneTwister

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/*
Credit where its due

Inspiration from here: https://github.com/mackorone/catan
Copied/converted the printer from here: https://github.com/cmelchior/asciihexgrid
 */

object CatanGen {

  class Terrain( val code : Char, val symbol : String, val color : String )
  // ;1 for bold
  case class Grain()  extends Terrain('G', "~", "\u001b[33m" ) // yellow
  case class Ore()    extends Terrain('O', "^", "\u001b[34m" ) // blue
  case class Wool()   extends Terrain('W', "*", "\u001b[37m" )  // white      *
  case class Lumber() extends Terrain('L', "|", "\u001b[32m" )  // green      |
  case class Brick()  extends Terrain('B', "=", "\u001b[31m" )  // red        =
  case class Desert() extends Terrain('D', ".", "\u001b[36m" )  // grey       .

  case class Port( t : Option[Terrain] = None )

  class Card( val symbol : Char )
  case class Knight() extends Card('K')
  case class Roads() extends Card('R')
  case class Monopoly() extends Card('M')
  case class Victory() extends Card('V')

  class Player( var color : String, val hand : List[Card], val played : List[Card] )

  case class Cell( terr : Terrain, token : Int )

  case class Intersection( x : Int, y : Int, cells : List[Cell] = List(), port : Option[Port] = None, var owner : Option[Player] = None )

  case class Road( from : Intersection, to : Intersection, var owner : Option[Player] = None )

  case class GameState( val is : List[Intersection], val rs : List[Road], val deck : List[Card],
                        val hands : Map[Int,List[Card]], val played : Map[Int,List[Card]] )

  case class Game( val states : ArrayBuffer[GameState], val players : List[Player], val cells : List[Cell] )

  val rand = new MersenneTwister()

  def main( args : Array[String] ) : Unit = {
    Console.println("\u001b[31m catangen... \u001b[0m")

    val ts = shuffle(createTerrains())
    val ks = shuffle(createTokens())
    val ps = shuffle(createPorts())

    val cs = createCells(ts, ks)

    val is = createIntersections( cs, ps)
    val rs = createRoads(is)

    val gs = GameState( is, rs, List(), Map(), Map() )
    val gss = ArrayBuffer[GameState]()
    gss += gs

    val game = Game( gss, List(), cs )

    HexPrinter.printGame( game )
  }

  def shuffle[T]( input : List[T]) : List[T] = {

    val src  = new ArrayBuffer[T]()
    val trgt = new ArrayBuffer[T]()

    src ++= input

    while( src.length > 1 ){
      trgt += src.remove(rand.nextInt(src.length))
    }

    trgt += src(0)

    trgt.toList

  }

  def createTerrains() : List[Terrain] = {

    val ts = new ArrayBuffer[Terrain]()

    ts += Grain()
    ts += Grain()
    ts += Grain()
    ts += Grain()

    ts += Ore()
    ts += Ore()
    ts += Ore()

    ts += Wool()
    ts += Wool()
    ts += Wool()
    ts += Wool()

    ts += Lumber()
    ts += Lumber()
    ts += Lumber()
    ts += Lumber()

    ts += Brick()
    ts += Brick()
    ts += Brick()

    ts += Desert()

    ts.toList

  }

  def weights() : Map[Int,Int] = {

    val ws = mutable.HashMap[Int,Int]()

    ws += ( 2 -> 1 )
    ws += ( 3 -> 2 )
    ws += ( 4 -> 3 )
    ws += ( 5 -> 4 )
    ws += ( 6 -> 5 )
    ws += ( 7 -> 6 )
    ws += ( 8 -> 5 )
    ws += ( 9 -> 4 )
    ws += ( 10 -> 3 )
    ws += ( 11 -> 2 )
    ws += ( 12 -> 1 )

    ws.toMap

  }

  def createTokens() : List[Int] = {

    val is = new ArrayBuffer[Int]()

    is += 2

    is += 3
    is += 3

    is += 4
    is += 4

    is += 5
    is += 5

    is += 6
    is += 6

    is += 8
    is += 8

    is += 9
    is += 9

    is += 10
    is += 10

    is += 11
    is += 11

    is += 12

    is.toList
  }

  def createPorts() : List[Port] = {


    val ps = new ArrayBuffer[Port]()

    ps += Port()
    ps += Port()
    ps += Port()
    ps += Port()

    ps += Port(Some(Grain()))
    ps += Port(Some(Ore()))
    ps += Port(Some(Lumber()))
    ps += Port(Some(Brick()))
    ps += Port(Some(Wool()))


    ps.toList

  }

  def createCells( tsi : List[Terrain], ksi : List[Int] ) : List[Cell] = {

    // we could randomize and assign the terrains

    val cells = new ArrayBuffer[Cell]()

    val ts = ArrayBuffer[Terrain]()
    ts ++= tsi
    val ks = ArrayBuffer[Int]()
    ks ++= ksi



    for( i <- 0 until 19 ){

      val t = ts.remove(0)
      t match {
        case Desert() => { cells += Cell(t, 0 ) }
        case _ => { cells += Cell(t, ks.remove(0) ) }
      }
    }

    cells.toList
  }

  def createIntersections( cs : List[Cell], ps : List[Port] ) : List[Intersection] = {

    val is = new ArrayBuffer[Intersection]()
    // case class Intersection( x : Int, y : Int, cells : List[Cell] = List(), port : Option[Port] = None, var owner : Option[Player] = None )

    is += new Intersection( 0, 0, List(cs(0)) )
    is += new Intersection( 1, 0, List(cs(1)) )
    is += new Intersection( 2, 0, List(cs(2)) )

    is += new Intersection( 0, 1, List(cs(0)) )
    is += new Intersection( 1, 1, List(cs(0), cs(1)) )
    is += new Intersection( 2, 1, List(cs(1), cs(2)) )
    is += new Intersection( 3, 1, List(cs(2))  )

    is += new Intersection( 0, 2, List(cs(0), cs(3) ) )
    is += new Intersection( 1, 2, List(cs(0), cs(1), cs(4) ) )
    is += new Intersection( 2, 2, List(cs(1), cs(2), cs(5) ) )
    is += new Intersection( 3, 2, List(cs(2), cs(6) )  )

    is += new Intersection( 0, 3, List(cs(3))  )
    is += new Intersection( 1, 3, List(cs(0), cs(3), cs(4) ) )
    is += new Intersection( 2, 3, List(cs(1), cs(4), cs(5) ) )
    is += new Intersection( 3, 3, List(cs(2), cs(5), cs(6) )  )
    is += new Intersection( 4, 3, List(cs(6))  )


    is += new Intersection( 0, 4, List(cs(3), cs(7)) )
    is += new Intersection( 1, 4, List(cs(3), cs(4), cs(8) )  )
    is += new Intersection( 2, 4, List(cs(4), cs(5), cs(9) ) )
    is += new Intersection( 3, 4, List(cs(5), cs(6), cs(10) ) )
    is += new Intersection( 4, 4, List(cs(6), cs(10) ) )

    is += new Intersection( 0, 5, List(cs(7))  )
    is += new Intersection( 1, 5, List(cs(3), cs(7), cs(8) )  )
    is += new Intersection( 2, 5, List(cs(4), cs(8), cs(9) ))
    is += new Intersection( 3, 5, List(cs(5), cs(9), cs(10) ))
    is += new Intersection( 4, 5, List(cs(6), cs(10), cs(11) ) )
    is += new Intersection( 5, 5, List(cs(11))  )

    is += new Intersection( 0, 6, List(cs(7))  )
    is += new Intersection( 1, 6, List(cs(7), cs(8), cs(12) ) )
    is += new Intersection( 2, 6, List(cs(8), cs(9), cs(13) ) )
    is += new Intersection( 3, 6, List(cs(9), cs(10), cs(14) ) )
    is += new Intersection( 4, 6, List(cs(10), cs(11), cs(15) ) )
    is += new Intersection( 5, 6, List(cs(11) ) )

    is += new Intersection( 0, 7, List(cs(7), cs(12) )  )
    is += new Intersection( 1, 7, List(cs(8), cs(12), cs(13) ) )
    is += new Intersection( 2, 7, List(cs(9), cs(13), cs(14) ) )
    is += new Intersection( 3, 7, List(cs(10), cs(14), cs(15) ) )
    is += new Intersection( 4, 7, List(cs(11), cs(15) ) )

    is += new Intersection( 0, 8, List(cs(12) ) )
    is += new Intersection( 1, 8, List(cs(12), cs(13), cs(16) ) )
    is += new Intersection( 2, 8, List(cs(13), cs(14), cs(17) ) )
    is += new Intersection( 3, 8, List(cs(14), cs(15), cs(18) ) )
    is += new Intersection( 4, 8, List(cs(15) ) )

    is += new Intersection( 0, 9, List(cs(12), cs(16) ) )
    is += new Intersection( 1, 9, List(cs(13), cs(16), cs(17) ) )
    is += new Intersection( 2, 9, List(cs(14), cs(17), cs(18) ) )
    is += new Intersection( 3, 9, List(cs(15), cs(18) ) )

    is += new Intersection( 0, 10, List(cs(16) ) )
    is += new Intersection( 1, 10, List(cs(16), cs(17) ) )
    is += new Intersection( 2, 10, List(cs(17), cs(18) ) )
    is += new Intersection( 3, 10, List(cs(18) ) )

    is += new Intersection( 0, 11, List(cs(16) ) )
    is += new Intersection( 1, 11, List(cs(17) ) )
    is += new Intersection( 2, 11, List(cs(18) ) )

    is.toList

  }

  def findIntersection( x : Int, y : Int, is : List[Intersection] ) : Intersection = {
    is.find( ( i : Intersection ) => { i.x == x && i.y == y } ).get
  }

  def createRoads( is : List[Intersection] ) : List[Road] = {

    val rs = new ArrayBuffer[Road]()

    // case class Road( from : Intersection, to : Intersection, var owner : Option[Player] = None )

    // H
    rs += Road( findIntersection( 0, 1, is ), findIntersection( 0, 0, is ) )
    rs += Road( findIntersection( 0, 0, is ), findIntersection( 1, 1, is ) )
    rs += Road( findIntersection( 1, 1, is ), findIntersection( 1, 0, is ) )
    rs += Road( findIntersection( 1, 0, is ), findIntersection( 2, 1, is ) )
    rs += Road( findIntersection( 2, 1, is ), findIntersection( 2, 0, is ) )
    rs += Road( findIntersection( 2, 0, is ), findIntersection( 3, 1, is ) )

    // V
    rs += Road( findIntersection( 0, 1, is ), findIntersection( 0, 2, is ) )
    rs += Road( findIntersection( 1, 1, is ), findIntersection( 1, 2, is ) )
    rs += Road( findIntersection( 2, 1, is ), findIntersection( 2, 2, is ) )
    rs += Road( findIntersection( 3, 1, is ), findIntersection( 3, 2, is ) )

    // H
    rs += Road( findIntersection( 0, 3, is ), findIntersection( 0, 2, is ) )
    rs += Road( findIntersection( 0, 2, is ), findIntersection( 1, 3, is ) )
    rs += Road( findIntersection( 1, 3, is ), findIntersection( 1, 2, is ) )
    rs += Road( findIntersection( 1, 2, is ), findIntersection( 2, 3, is ) )
    rs += Road( findIntersection( 2, 3, is ), findIntersection( 2, 2, is ) )
    rs += Road( findIntersection( 2, 2, is ), findIntersection( 3, 3, is ) )
    rs += Road( findIntersection( 3, 3, is ), findIntersection( 3, 2, is ) )
    rs += Road( findIntersection( 3, 2, is ), findIntersection( 4, 3, is ) )

    // V
    rs += Road( findIntersection( 0, 3, is ), findIntersection( 0, 4, is ) )
    rs += Road( findIntersection( 1, 3, is ), findIntersection( 1, 4, is ) )
    rs += Road( findIntersection( 2, 3, is ), findIntersection( 2, 4, is ) )
    rs += Road( findIntersection( 3, 3, is ), findIntersection( 3, 4, is ) )
    rs += Road( findIntersection( 4, 3, is ), findIntersection( 4, 4, is ) )

    // H 10
    rs += Road( findIntersection( 0, 5, is ), findIntersection( 0, 4, is ) )
    rs += Road( findIntersection( 0, 4, is ), findIntersection( 1, 5, is ) )
    rs += Road( findIntersection( 1, 5, is ), findIntersection( 1, 4, is ) )
    rs += Road( findIntersection( 1, 4, is ), findIntersection( 2, 5, is ) )
    rs += Road( findIntersection( 2, 5, is ), findIntersection( 2, 4, is ) )
    rs += Road( findIntersection( 2, 4, is ), findIntersection( 3, 5, is ) )
    rs += Road( findIntersection( 3, 5, is ), findIntersection( 3, 4, is ) )
    rs += Road( findIntersection( 3, 4, is ), findIntersection( 4, 5, is ) )
    rs += Road( findIntersection( 4, 5, is ), findIntersection( 4, 4, is ) )
    rs += Road( findIntersection( 4, 4, is ), findIntersection( 5, 5, is ) )

    // V - 6
    rs += Road( findIntersection( 0, 5, is ), findIntersection( 0, 6, is ) )
    rs += Road( findIntersection( 1, 5, is ), findIntersection( 1, 6, is ) )
    rs += Road( findIntersection( 2, 5, is ), findIntersection( 2, 6, is ) )
    rs += Road( findIntersection( 3, 5, is ), findIntersection( 3, 6, is ) )
    rs += Road( findIntersection( 4, 5, is ), findIntersection( 4, 6, is ) )
    rs += Road( findIntersection( 5, 5, is ), findIntersection( 5, 6, is ) )

    // H 10
    rs += Road( findIntersection( 0, 6, is ), findIntersection( 0, 7, is ) )
    rs += Road( findIntersection( 0, 7, is ), findIntersection( 1, 6, is ) )
    rs += Road( findIntersection( 1, 6, is ), findIntersection( 1, 7, is ) )
    rs += Road( findIntersection( 1, 7, is ), findIntersection( 2, 6, is ) )
    rs += Road( findIntersection( 2, 6, is ), findIntersection( 2, 7, is ) )
    rs += Road( findIntersection( 2, 7, is ), findIntersection( 3, 6, is ) )
    rs += Road( findIntersection( 3, 6, is ), findIntersection( 3, 7, is ) )
    rs += Road( findIntersection( 3, 7, is ), findIntersection( 4, 6, is ) )
    rs += Road( findIntersection( 4, 6, is ), findIntersection( 4, 7, is ) )
    rs += Road( findIntersection( 4, 7, is ), findIntersection( 5, 6, is ) )

    // V 5
    rs += Road( findIntersection( 0, 7, is ), findIntersection( 0, 8, is ) )
    rs += Road( findIntersection( 1, 7, is ), findIntersection( 1, 8, is ) )
    rs += Road( findIntersection( 2, 7, is ), findIntersection( 2, 8, is ) )
    rs += Road( findIntersection( 3, 7, is ), findIntersection( 3, 8, is ) )
    rs += Road( findIntersection( 4, 7, is ), findIntersection( 4, 8, is ) )

    // H 8
    rs += Road( findIntersection( 0, 8, is ), findIntersection( 0, 9, is ) )
    rs += Road( findIntersection( 0, 9, is ), findIntersection( 1, 8, is ) )
    rs += Road( findIntersection( 1, 8, is ), findIntersection( 1, 9, is ) )
    rs += Road( findIntersection( 1, 9, is ), findIntersection( 2, 8, is ) )
    rs += Road( findIntersection( 2, 8, is ), findIntersection( 2, 9, is ) )
    rs += Road( findIntersection( 2, 9, is ), findIntersection( 3, 8, is ) )
    rs += Road( findIntersection( 3, 8, is ), findIntersection( 3, 9, is ) )
    rs += Road( findIntersection( 3, 9, is ), findIntersection( 4, 8, is ) )

    // V 4
    rs += Road( findIntersection( 0, 9, is ), findIntersection( 0, 10, is ) )
    rs += Road( findIntersection( 1, 9, is ), findIntersection( 1, 10, is ) )
    rs += Road( findIntersection( 2, 9, is ), findIntersection( 2, 10, is ) )
    rs += Road( findIntersection( 3, 9, is ), findIntersection( 3, 10, is ) )

    // H 6
    rs += Road( findIntersection( 0, 10, is ), findIntersection( 0, 11, is ) )
    rs += Road( findIntersection( 0, 11, is ), findIntersection( 1, 10, is ) )
    rs += Road( findIntersection( 1, 10, is ), findIntersection( 1, 11, is ) )
    rs += Road( findIntersection( 1, 11, is ), findIntersection( 2, 10, is ) )
    rs += Road( findIntersection( 2, 10, is ), findIntersection( 2, 11, is ) )
    rs += Road( findIntersection( 2, 11, is ), findIntersection( 3, 10, is ) )


    rs.toList


  }


}




