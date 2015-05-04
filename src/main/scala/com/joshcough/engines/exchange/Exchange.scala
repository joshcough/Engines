package com.joshcough.engines
package exchange

import scalaz.{Order => Ord, _}
import syntax.applicative._
import syntax.monad._
import Scalaz._
import scala.annotation.tailrec

import OrderTypes._

object Stock {
  implicit val stockTransactionRunner: TransactionRunner[Stock] = new TransactionRunner[Stock] {
    def runTransaction(o: Order, s: Stock) = s.transaction(o) 
  }
}

/*
 A stock contains:
   * the ticker (MSFT, GOOG, APPL, etc)
   * all the current bids for this stock
   * all the current asks for this stock

 The price of the highest bid should always be lower 
 than the asking price of the lowest ask.
 (Which means that no one is willing to pay the asking price)
 */

case class Stock (
  ticker: Ticker, 
  bids: Heap[ShareData]=Heap.Empty[ShareData], // ordered by price, high to low
  asks: Heap[ShareData]=Heap.Empty[ShareData]  // ordered by price, low  to high
){

  def transaction(order: Order): (Transaction, Stock) = 
    order.orderType match {
      case Bid => bid(order)
      case Ask => ask(order)
    }

  private def bid(o: Order): (Transaction, Stock) = {
    val (t2, newAsks, newBids) = 
      fillOrder(o, ShareData(o), Unfilled(o), asks, bids, ShareData.bidOrdering) { 
        (bidPrice, askPrice) => bidPrice >= askPrice
      }
    (t2, copy(bids=newBids, asks=newAsks))
  }

  private def ask(o: Order): (Transaction, Stock) = {
    val (t2, newBids, newAsks) = 
      fillOrder(o, ShareData(o), Unfilled(o), bids, asks, ShareData.askOrdering) { 
        (askPrice, bidPrice) => askPrice <= bidPrice  
      }
    (t2, copy(bids=newBids, asks=newAsks))
  }

  @tailrec
  private def fillOrder (
    order          : Order,
    shareData      : ShareData,
    trans          : Transaction, 
    drawHeap       : Heap[ShareData], 
    entryHeap      : Heap[ShareData], 
    entryHeapOrder : Ord[ShareData])
   (priceMatch     : (Price, Price) => Boolean): 
   (Transaction, Heap[ShareData], Heap[ShareData]) = {
    /*
       If either:
         * we have no asks
         * the bid is lower than the current lowest asking price
       Then the bid can't possibly be filled.

       In that case, if we have partially filled the order
         then we do not add it to the heap, we send the PartialFill back to the client
         otherwise we add it to the bids awaiting fulfillment
     */
    if (drawHeap.isEmpty || ! priceMatch(order.price, drawHeap.minimum.order.price)) {
      println("if   branch: " + order + " " + shareData)
      (trans, drawHeap, entryHeap.insert(ShareData(order))(entryHeapOrder))
      // trans match {
      //   case Unfilled(_) => (trans, drawHeap, entryHeap.insert(order)(entryHeapOrder))
      //   case _           => (trans, drawHeap, entryHeap)
      // }
    }
    /* 
     We can now either partially fill, or completely fill the order.
     Basic idea:
       Keep taking asks (with price <= bid.price) off the top of the heap 
       Until we've reached the number of shares in the bid

       If we run out of asks, then the bid only gets partially filled.
       If we don't, then we can full fill the bid.

       Special case:
         Unless the last ask (with price <= bid.price) is completely drained
         We need to make sure that last ask is put back on top of the heap (or never removed)/
     */
    // to get the minimum frOrder a Heap, call: h.minimum
    else {
      println("else branch: " + order + " " + shareData)
      val min = drawHeap.minimum
      // TODO: one problem with min here is that we might have only filled
      // some of the shares in it. we might need to send back
      // a partial fill to the client
      val newTransaction = trans.fill(min)

      // PERFECT FILL
      if (min.remaining == shareData.remaining) {
        println("perfect")
        // we can completely fill the bid.
        (newTransaction, drawHeap.deleteMin, entryHeap)
      }
      // FILL, keeping the current node on top of the heap, adjusting it's shares. 
      else if (min.remaining > shareData.remaining) {
        println("kinda perfect, but adjust min")
        // we can completely fill the bid, but the bids shares 
        // must be removed from the ask on top of the heap
        (newTransaction, drawHeap.adjustMin(_.fill(shareData.remaining)), entryHeap)
      }
      // PARTIAL FILL
      else {
        println("partial fill, recur")
        // we can't completely fill the bid, so we need to recur.
        fillOrder(
          // we recur needing fewer shares, since we filled min.shares shares.
          order,
          shareData.fill(min.remaining),
          // min is preserved in the transaction
          newTransaction,
          // min.shares is now empty, so it is deleted
          drawHeap.deleteMin,
          // nothing happens to the entry heap
          entryHeap,
          entryHeapOrder
        )(priceMatch)
      }
    }
  }
}

/*
  A Market is just a collection of stocks
 */
case class Market(stocks: Map[Ticker, Stock]) 

object Market {
  implicit val marketTransactionRunner: TransactionRunner[Market] = new TransactionRunner[Market] {
    def runTransaction(o: Order, m: Market) = 
      m.stocks.get(o.ticker).fold((Unfilled(o): Transaction, m)){ s => 
        val (t, newStock) = TransactionRunner.runTransaction(o, s)
        (t, Market(m.stocks + (o.ticker -> newStock)))
      }
  }
}

case class Exchange(market: Market)

object Exchange {
  implicit val exchangeTransactionRunner: TransactionRunner[Exchange] = new TransactionRunner[Exchange] {
    def runTransaction(o: Order, e: Exchange) = {
      val (t, newMarket) = TransactionRunner.runTransaction(o, e.market)
      (t, Exchange(newMarket))
    }
  }
}

// TODO: who sends the fills back?
case class MutableExchange(private var e: Exchange) {
  def transaction(o: Order): Transaction = {
    val (t, newE) = TransactionRunner.runTransaction(o, e)
    e = newE
    t
  }
}

