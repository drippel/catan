package com.github.catan.ascii

object HexPrinter {

  def main( args : Array[String] ) : Unit = {
    Console.println("hex printer...")

    // val board = new AsciiBoard( 0, 6, 0, 6, new LargeFlatAsciiHexPrinter() )
    val board = new AsciiBoard( 0, 6, 0, 6, new MedFlatAsciiHexPrinter() )

    board.printHex( "", "", ' ', 0, 3)
    board.printHex( "", "", ' ', 1, 2)
    board.printHex( "", "", ' ', 2, 1)
    board.printHex( "", "", ' ', 3, 0)

    board.printHex( "", "", ' ', 0, 4)
    board.printHex( "H1", "-B-", '#', 1, 3)
    board.printHex( "H1", "-B-", '#', 2, 2)
    board.printHex( "H1", "-B-", '#', 3, 1)
    board.printHex( "", "", ' ', 4, 0)


    board.printHex( "", "", ' ', 0, 5)
    board.printHex( "H1", "-B-", '#', 1, 4)
    board.printHex( "H1", "-B-", '#', 2, 3)
    board.printHex( "H1", "-B-", '#', 3, 2)
    board.printHex( "H1", "-B-", '#', 4, 1)
    board.printHex( "", "", ' ', 5, 0)

    board.printHex( "", "", ' ', 0, 6)
    board.printHex( "H1", "W", '#', 1, 5)
    board.printHex( "H1", "W", '#', 2, 4)
    board.printHex( "H1", "W", '#', 3, 3)
    board.printHex( "H1", "W", '#', 4, 2)
    board.printHex( "H1", "W", '#', 5, 1)
    board.printHex( "", "", ' ', 6, 0)

    board.printHex( "", "", ' ', 1, 6)
    board.printHex( "H1", "W", '#', 2, 5)
    board.printHex( "H1", "W", '#', 3, 4)
    board.printHex( "H1", "W", '#', 4, 3)
    board.printHex( "H1", "W", '#', 5, 2)
    board.printHex( "", "", ' ', 6, 1)

    board.printHex( "", "", ' ', 2, 6)
    board.printHex( "H1", "W", '#', 3, 5)
    board.printHex( "H1", "W", '#', 4, 4)
    board.printHex( "H1", "W", '#', 5, 3)
    board.printHex( "", "", ' ', 6, 2)

    board.printHex( "", "", ' ', 3, 6)
    board.printHex( "", "", ' ', 4, 5)
    board.printHex( "", "", ' ', 5, 4)
    board.printHex( "", "", ' ', 6, 3)

    /*
    for( x <- 0 to 6 ){
      for( y <- 0 to 6 ){
        val s = x +","+ y
        board.printHex( s, "W", ' ', x, y)
      }
    }
    */


    val out = board.prettyPrint(false)
    Console.println( out )

  }


  object CharGrid {
    private val LINE_BREAK = "\n"
  }

  class CharGrid(val width : Int, val height : Int) {

    final private var grid = Array.ofDim[Char](this.height, this.width)

    prefillGrid()

    /**
      * Prefill grid with spaces.
      */
    private def prefillGrid() : Unit = {
      var i = 0
      while( {
        i < height
      } ) {
        var j = 0
        while( {
          j < width
        } ) {
          addChar(j, i, ' ')

          {
            j += 1; j - 1
          }
        }

        {
          i += 1; i - 1
        }
      }
    }

    /**
      * Add a string to the grid.
      *
      * @param x     Starting x coordinate.
      * @param y     Starting y coordinate.
      * @param input String put input. String will not wrap, but throws IndexOutOfBounds if to long.
      */
    def addString(x : Int, y : Int, input : String) : Unit = {
      if( input == null || input == "" ) return
      var i = 0
      while( {
        i < input.length
      } ) {
        addChar(x + i, y, input.charAt(i))

        {
          i += 1; i - 1
        }
      }
    }

    /**
      * Add a string to the grid.
      *
      * @param x     Starting x coordinate.
      * @param y     Starting y coordinate.
      * @param input Char to insert. Trows IndexOutOfBounds if outside grid.
      */
    def addChar(x : Int, y : Int, input : Char) : Unit = {
      if( x < 0 || x >= width || y < 0 || y >= height ) {
        val maxWidth = width - 1
        val maxHeight = height - 1
        throw new IndexOutOfBoundsException("(" + x + "," + y + ") is outside (" + maxWidth + "," + maxHeight + ")")
      }
      grid(y)(x) = input
    }

    /**
      * Returns a char from the grid
      */
    def getChar(x : Int, y : Int) : Char = grid(y)(x)

    /**
      * Returns the char grid as a string, ready for output.
      *
      * @param trimToBoundingBox If true, the grid is trimmed to it's contents bounding box. If not grid is printet as is.
      */
    def print(trimToBoundingBox : Boolean) : String = {
      var leftBound = if( trimToBoundingBox ) width - 1
      else 0
      var rightBound = if( trimToBoundingBox ) 0
      else width - 1
      var topBound = if( trimToBoundingBox ) height - 1
      else 0
      var bottomBound = if( trimToBoundingBox ) 0
      else height - 1
      // Find bounding box
      if( trimToBoundingBox ) {
        var i = 0
        while( {
          i < height
        } ) {
          var j = 0
          while( {
            j < width
          } ) {
            val c = grid(i)(j)
            if( c != ' ' ) {
              leftBound = Math.min(leftBound, j)
              rightBound = Math.max(rightBound, j)
              topBound = Math.min(topBound, i)
              bottomBound = Math.max(bottomBound, i)
            }

            {
              j += 1; j - 1
            }
          }

          {
            i += 1; i - 1
          }
        }
      }
      // Print grid
      val builder = new StringBuilder((width + CharGrid.LINE_BREAK.length) * height)
      var i = topBound
      while( {
        i <= bottomBound
      } ) {
        var j = leftBound
        while( {
          j <= rightBound
        } ) {
          builder.append(grid(i)(j))

          {
            j += 1; j - 1
          }
        }
        builder.append(CharGrid.LINE_BREAK)

        {
          i += 1; i - 1
        }
      }
      builder.toString
    }
  }


  /**
    * Description of a Ascii hex map.
    * The hex grid uses a trapezoidal or axial coordinate system, like so:
    *
                _ _
              /     \
    *    _ _ /(0,-1) \ _ _
    *  /     \  -R   /     \
    * /(-1,0) \ _ _ /(1,-1) \
    * \  -Q   /     \       /
    *  \ _ _ / (0,0) \ _ _ /
    *  /     \       /     \
    * /(-1,1) \ _ _ / (1,0) \
    * \       /     \  +Q   /
    *  \ _ _ / (0,1) \ _ _ /
    *        \  +R   /
    *         \ _ _ /
    *
    */
  class AsciiBoard(val minQ : Int, val maxQ : Int, val minR : Int, val maxR : Int, var printer : AsciiHexPrinter) {

    this.width = maxQ - minQ + 1
    this.height = maxR - minR + 1
    var grid = createGrid()

    final private var width = 0
    final private var height = 0

    private def createGrid() = {
      // This potentially creates the grid ½ a hexagon to heigh or wide, as we do not now given the max coordinates
      // (0,0,1,1) if both (0,1) or (1,1) is filled. This is OK, as we can fix it when outputting the grid.
      val gridSize = printer.getMapSizeInChars(width, height)
      new CharGrid(gridSize(0), gridSize(1))
    }

    /**
      *
      * @param line1      First line of text
      * @param line2      2nd line of
      * @param fillerChar Character used as filler, may be ' '
      * @param hexQ       Q coordinate for the hex in the hex grid.
      * @param hexR       R coordinate for the hex in the hex grid.
      */
    def printHex(line1 : String, line2 : String, fillerChar : Char, hexQ : Int, hexR : Int) : Unit = {
      val hex = printer.getHex(line1, line2, fillerChar)
      val charCoordinates = printer.mapHexCoordsToCharCoords(hexQ, hexR)
      val lines = hex.toString.split("\n")
      var i = 0
      while( { i < lines.length } ) {
        val content = lines(i)
        var j = 0
        while( { j < content.length } ) {
          val x = charCoordinates(0) + j
          val y = charCoordinates(1) + i
          // Only override empty spaces
          if( grid.getChar( x,y ) == ' ' ){ grid.addChar(x, y, content.charAt(j)) }

          {
            j += 1; j - 1
          }
        }

        {
          i += 1; i - 1
        }
      }
    }

    /**
      * Prints the Hexagonal map as a string.
      *
      * @param wrapInBox If true, output is wrapped in a Ascii drawn box.
      */
    def prettyPrint(wrapInBox : Boolean) : String = printBoard(wrapInBox)

    /**
      * Returns the Hexagonal map as a string. Any extra empty lines at the end are trimmed away,
      * but map still starts at (0,0), so eg. having a hex at (0,1) will produce whitespace at the top.
      *
      * @param wrapInBox If true, the hex map is wrapped in a ASCII bounding box.
      */
    private def printBoard(wrapInBox : Boolean) = if( wrapInBox ) {
      val sb = new StringBuilder
      // Get content
      val lines = grid.print(true).split("\n")
      val contentLength = if( lines.length > 0 ) lines(0).length
      else 0
      val verticalLine = getVerticalLine('=', contentLength)
      val spacerLine = getVerticalLine(' ', contentLength)
      // Build output
      sb.append(verticalLine)
      var i = 0
      while( { i < lines.length } ) {
        val line = lines(i)
        sb.append("| ")
        sb.append(line)
        sb.append(" |")
        sb.append('\n')

        {
          i += 1; i - 1
        }
      }

      // Flat hexes have to little bottom space as they use the _ char
      // so add a extra filler line.
      sb.append(spacerLine)
      sb.append(verticalLine)
      sb.toString
    }
    else grid.print(true)

    private def getVerticalLine(filler : Char, contentLength : Int) = {
      val verticalLine = new StringBuilder("| ")
      var i = 0
      while( {
        i < contentLength
      } ) {
        if( i % 2 == 0 ) verticalLine.append(filler)
        else verticalLine.append(' ')

        {
          i += 1; i - 1
        }
      }
      verticalLine.append(" |\n").toString
    }
  }

  abstract class AsciiHexPrinter {

    def getHex(line1 : String, line2 : String, fillerChar : Char) : String

    def mapHexCoordsToCharCoords(q : Int, r : Int) : Array[Int]

    def getMapSizeInChars(hexWidth : Int, hexHeight : Int) : Array[Int]

    protected def restrictToLength(str : String, length : Int) : String = {
      var result = "  "
      if( str != null ) if( str.length > length ) result = str.toUpperCase.substring(0, length)
      else if( str.length < length ) result = pad(str.toUpperCase, length - str.length)
      else result = str
      result
    }

    /**
      * Pads whitespace to both sides, effectively centering the text.
      * Padding starts at the left side
      *
      * @param s
      * @param n
      * @return
      */
    private def pad(s : String, n : Int) = {

      var ss = s
      var nn = n

      while( { nn > 0 } ) {
        if( nn % 2 == 0 ){ ss = " " + ss }
        else{ ss = ss + " " }
        nn -= 1
      }
      ss
    }
  }

  object LargeFlatAsciiHexPrinter {

    // 0 - 13
    // 12 - 24
    // 24 - 36
    // 36 - 48
    // 48 - 60
    // 60 - 72
    // 72 - 84
    val TEMPLATE = "   _ _ _ _  \n" +
                   "  / # # # \\  \n" +
                   " /# # # # #\\ \n" +
                   "/# XXXXXXX #\\\n" +
                   "\\# YYYYYYY #/\n" +
                   " \\# # # # #/ \n" +
                   "  \\_#_#_#_/  \n"

  }

  class LargeFlatAsciiHexPrinter extends AsciiHexPrinter {

    final private val width = 13
    final private val height = 7
    final private val sideLength = 3
    final private val sideHeight = 3

    override def getHex(line1 : String, line2 : String, filler : Char) : String = {
      var lline1 = line1
      var lline2 = line2

      var hex = new String(LargeFlatAsciiHexPrinter.TEMPLATE)
      lline1 = restrictToLength(lline1, 7)
      lline2 = restrictToLength(lline2, 7)
      hex = hex.replace("XXXXXXX", lline1)
      hex = hex.replace("YYYYYYY", lline2)
      hex.replace('#', filler)
    }

    override def mapHexCoordsToCharCoords(q : Int, r : Int) : Array[Int] = {
      val result = new Array[Int](2)
      result(0) = (width - sideLength) * q
      result(1) = sideHeight * q + (height - 1) * r
      result
    }

    override def getMapSizeInChars(hexWidth : Int, hexHeight : Int) : Array[Int] = {
      val widthInChars = hexWidth * (width - sideLength) + sideLength
      val heightInChars = (hexWidth - 1) * height / 2 + hexHeight * height
      Array[Int](widthInChars, heightInChars)
    }

  }

  object MedFlatAsciiHexPrinter {

    // 0 - 13
    // 12 - 24
    // 24 - 36
    // 36 - 48
    // 48 - 60
    // 60 - 72
    // 72 - 84
    val TEMPLATE2 =
      "   _ _ _ _  \n" +
      "  / # # # \\  \n" +
      " /# # # # #\\ \n" +
      "/# XXXXXXX #\\\n" +
      "\\# YYYYYYY #/\n" +
      " \\# # # # #/ \n" +
      "  \\_#_#_#_/  \n"


    val TEMPLATE = "  + - - +  \n" +
                   " /       \\ \n" +
                   "+ XXXXXXX +\n" +
                   " \\       / \n" +
                   "  + - - +  \n"

  }

  class MedFlatAsciiHexPrinter extends AsciiHexPrinter {

    final private val width = 11
    final private val height = 5
    final private val sideLength = 2
    final private val sideHeight = 2

    override def getHex(line1 : String, line2 : String, filler : Char) : String = {
      var lline1 = line1
      var lline2 = line2

      var hex = new String(MedFlatAsciiHexPrinter.TEMPLATE)
      lline1 = restrictToLength(lline1, 7)
      lline2 = restrictToLength(lline2, 7)
      // hex = hex.replace("XXXXXXX", lline1)
      //hex = hex.replace("YYYYYYY", lline2)
      hex.replace('#', filler)
    }

    override def mapHexCoordsToCharCoords(q : Int, r : Int) : Array[Int] = {
      val result = new Array[Int](2)
      result(0) = (width - sideLength - 1) * q
      result(1) = sideHeight * q + (height - 1) * r
      result
    }

    override def getMapSizeInChars(hexWidth : Int, hexHeight : Int) : Array[Int] = {
      val widthInChars = hexWidth * (width - sideLength) + sideLength
      val heightInChars = (hexWidth - 1) * height / 2 + hexHeight * height
      Array[Int](widthInChars, heightInChars)
    }

  }


}
