package com.github.catan

object ColorTester {

  def main( args : Array[String] ) : Unit = {

    Console.println( "color test" )
    for( i <- 30 to 37 ){
      val code = "\u001b["+ i + "m"
      val msg =  code +"Test\u001b[0m"
      Console.println( msg )
    }

    Console.println( "bold test" )
    for( i <- 30 to 37 ){
      val code = "\u001b["+ i + ";1m"
      val msg =  code +"Test\u001b[0m"
      Console.println( msg )
    }

    Console.println( "bright test" )
    for( i <- 90 to 97 ){
      val code = "\u001b["+ i + "m"
      val msg =  code +"Test\u001b[0m"
      Console.println( msg )
    }

    Console.println( "back test" )
    for( i <- 40 to 47 ){
      val code = "\u001b["+ i + "m"
      val msg =  code +"Test\u001b[0m"
      Console.println( msg )
    }

    Console.println( "back bold test" )
    for( i <- 40 to 47 ){
      val code = "\u001b["+ i + ";1m"
      val msg =  code +"Test\u001b[0m"
      Console.println( msg )
    }

    Console.println( "bold test" )
    Console.println( "\u001b[1mTest\u001b[0m\n" )
    Console.println( "\u001b[4mTest\u001b[0m\n" )
    Console.println( "\u001b[7mTest\u001b[0m\n" )

  }

}
