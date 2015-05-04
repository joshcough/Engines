package com.joshcough.engines
package exchange

import org.scalacheck._
import org.scalacheck.Prop._
import Gen.{listOf, oneOf}
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

/*
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

  test("piles of orders")(forAll ((orders: List[Order]) => {
  	orders.size > 1 ==> {
  	val (stock, trans) = runOrders(orders)
  	if (stock.bids.isEmpty || stock.asks.isEmpty) true 
  	else stock.bids.minimum.price < stock.asks.minimum.price
  }}))
*/

  test("piles of orders")(Prop.forAll(listOf(oneOf(genSimpleAsk, genSimpleBid)))(orderFacts(_)))

  def orderFacts(orders:List[Order]): Prop = orders.size > 1 ==> {
        println("\n\nPiles of orders test:\n")
        println("\nORDERS:\n\t" + orders.mkString("\n\t") + "\n")
      val (stock, transactions) = runOrders(orders)
        println("\nTRANSACTIONS:\n\t" + transactions.mkString("\n\t"))
      val (bids, asks) = orders.partition(_.orderType==Bid)
      val countBidShares = bids.map(_.shares).sum
      val countAskShares = asks.map(_.shares).sum
        println("BIDS: " + stock.bids.toList + ", nonEmpty: " + stock.bids.nonEmpty)
        println("ASKS: " + stock.asks.toList + ", nonEmpty: " + stock.asks.nonEmpty)
        println(s"bid shares: $countBidShares, ask shares: $countAskShares")
      all(
        "more bids than asks" |: (countBidShares > countAskShares) ==> stock.bids.nonEmpty,
        "more asks than bids" |: (countBidShares < countAskShares) ==> stock.asks.nonEmpty
      )
  }

  def runOrders(orders: List[Order], s:Stock=Stock("Test")): (Stock, List[Transaction]) = 
    TransactionRunner.stateTransactions(orders)(Stock.stockTransactionRunner).apply(s)
}


