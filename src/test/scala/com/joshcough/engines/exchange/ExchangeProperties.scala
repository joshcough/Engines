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
import TransactionRunner._

object StockArbitrary {
  val genStock: Gen[Stock] = for { tic <- arbTicker } yield Stock(tic)
  implicit val arbStock: Arbitrary[Stock] = Arbitrary(genStock)
}

import StockArbitrary._

object StockTests extends EngineProperties("StockTests") {

  test("simple")(forAll ((s: Stock) => s.bids.isEmpty && s.asks.isEmpty))

  test("add bid")(Prop.forAll(genBid){ (o: Order) => 
  	val (t, sOut) = TransactionRunner.runTransaction(o, Stock(o.ticker))
  	t == Unfilled(o) && sOut.bids.minimum == o
  })

  test("add ask")(Prop.forAll(genAsk){ (o: Order) => 
  	val (t, sOut) = TransactionRunner.runTransaction(o, Stock(o.ticker))
  	t == Unfilled(o) && sOut.asks.minimum == o
  })

  test("add ask and bid")(Prop.forAll(genAsk, genBid){ 
  	(ask:Order, bid: Order) => (bid.price > ask.price && ask.shares > bid.shares) ==> {
  	val (stock, trans) = runOrders(List(ask, bid))
  	trans.length == 2 && stock.asks.minimum.shares == ask.shares - bid.shares
  }})

  var i = 0

  test("piles of orders")(forAll ((orders: List[Order]) => {
  	orders.size > 1 ==> {
  	val (stock, trans) = runOrders(orders)
  	if (stock.bids.isEmpty && stock.asks.isEmpty) {
  		i = i + 1 
  		println (s"emptyness: $i, orders: ${orders.size}")
  		if (orders.size < 5) println(testifyOrders(orders))
  		true 
  	}
  	else stock.bids.minimum.price < stock.asks.minimum.price
  }}))

  def testifyOrders(orders: List[Order]) = orders.map(_.copy(ticker="TEST", clientId="c"))

  def runOrders(orders: List[Order]): (Stock, List[Transaction]) = {
  	TransactionRunner.stateTransactions(
  	  testifyOrders(orders))(Stock.stockTransactionRunner).apply(Stock("TEST"))
  }
}


