package com.joshcough.engines
package exchange

import org.scalacheck._
import org.scalacheck.Prop._
import Gen.{listOf, oneOf}
import scalaz.{Order => Ord, _}
import Ordering._
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

  test("add bid")(forAll(genBid){ (o: Order) => 
  	val (t, sOut) = TransactionRunner.runTransaction(o, Stock(o.ticker))
    val sd = sOut.bids.minimum
  	t == Unfilled(o) && sd.order == o && sd.remaining == o.shares
  })

  test("add ask")(forAll(genAsk){ (o: Order) => 
  	val (t, sOut) = TransactionRunner.runTransaction(o, Stock(o.ticker))
    val sd = sOut.asks.minimum
  	t == Unfilled(o) && sd.order == o && sd.remaining == o.shares
  })

  test("add ask and bid")(forAll(genAsk, genBid){ (ask:Order, bid: Order) => 
    (bid.price > ask.price && ask.shares > bid.shares) ==> {
  	val (stock, trans) = runOrders(List(ask, bid))
  	trans.length == 2 && stock.asks.minimum.remaining == ask.shares - bid.shares
  }})

  test("max bid always less than min sell")(forAll(nonEmptyListOf(genSimpleOrder)){ orders: List[Order] =>
  	val (stock, trans) = runOrders(orders)
  	if (stock.bids.isEmpty || stock.asks.isEmpty) true 
  	else stock.bids.minimum.order.price < stock.asks.minimum.order.price
  })

  test("main properties")(Prop.forAll(nonEmptyListOf(oneOf(genSimpleAsk, genSimpleBid)))(orderFacts))

  property("x") = secure {
    orderFacts(List(
      Order(0,"c0",Bid,"TEST",104,449.9784951829429), 
      Order(1,"c1",Ask,"TEST",168,375.8993038677382), 
      Order(2,"c2",Bid,"TEST",127,596.7779419756251)
    ))
  }

  def orderFacts(orders:List[Order]): Prop = {
    val (stock, transactions) = runOrders(orders)
    val (bids, asks) = orders.partition(_.orderType==Bid)
    val countBidShares = bids.map(_.shares).sum
    val countAskShares = asks.map(_.shares).sum
    // val debugString = {
    //   def clean[A](l: List[A]) = l.map(_.toString).mkString("\n","\n\t", "")
    //   val ordersString = s"\n\n\nORDERS:\n\t${clean(orders)}"
    //   val transactionsString = s"TRANSACTIONS:\n\t${clean(transactions)}"
    //   val bidsString = s"BIDS: ${clean(stock.bids.toList)}, nonEmpty: ${stock.bids.nonEmpty} shares: ${bids.map(_.shares).sum}"
    //   val asksString = s"ASKS: ${clean(stock.asks.toList)}, nonEmpty: ${stock.asks.nonEmpty} shares: ${asks.map(_.shares).sum}"
    //   clean(List(ordersString, transactionsString, bidsString, asksString))
    // }
    all(
      "bid vs ask counts" |: (countBidShares ?|? countAskShares match {
        case EQ => true
        case LT => stock.asks.nonEmpty
        case GT => stock.bids.nonEmpty
      })
    )
  }

  def runOrders(orders: List[Order], s:Stock=Stock("Test")): (Stock, List[Transaction]) = 
    TransactionRunner.stateTransactions(orders)(Stock.stockTransactionRunner).apply(s)
}


