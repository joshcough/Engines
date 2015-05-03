package com.joshcough.engines
package exchange

import org.scalacheck._
import org.scalacheck.Prop._
import Shrink._
import Gen._
import Arbitrary.arbitrary
import Prop._
import scodec.bits._
import scodec._
import scodec.bits._
import codecs._
import scalaz.{Order => Ord, _}
import Scalaz._

import OrderTypes._
import OrderArbitrary._

object StockArbitrary {
  val genStock: Gen[Stock] = for { tic <- arbTicker } yield Stock(tic)
  implicit val arbStock: Arbitrary[Stock] = Arbitrary(genStock)
}

import StockArbitrary._

object StockTests extends EngineProperties("StockTests") {
  test("simple")(forAll ((s: Stock) => s.bids.isEmpty && s.asks.isEmpty))
  test("add bid")(Prop.forAll(genBid){ (o: Order) => 
  	val (t, sOut) = Stock(o.ticker).transaction(o)
  	t == Unfilled(o) && sOut.bids.minimum == o
  })
  test("add ask")(Prop.forAll(genAsk){ (o: Order) => 
  	val (t, sOut) = Stock(o.ticker).transaction(o)
  	t == Unfilled(o) && sOut.asks.minimum == o
  })

  // test("add bid and ask")(Prop.forAll(genBid, genAsk){ 
  // 	(bid: Order, ask:Order) ==> bid.price > ask.price
  // 	val (t, sOut) = sIn.transaction(o)
  // 	t == Unfilled(o) && sOut.asks.minimum == o
  // })

}
