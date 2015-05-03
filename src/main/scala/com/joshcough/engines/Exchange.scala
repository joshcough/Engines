package com.joshcough.engines

import scalaz.{Order => Ord, _}
import Scalaz._
import scala.annotation.tailrec

import OrderTypes._

object ExchangeTypes {
  type MaxHeap[A] = Heap[A] // ordered by price, high to low
  type MinHeap[A] = Heap[A] // ordered by price, low  to high
  type TransactionResult[A] = (Transaction, A)
  type TransactionRun[A] = (A, Order) => TransactionResult[A]
  type TransactionAttempt[A,B] = (Order, TransactionRun[A]) => TransactionResult[B]
}

import ExchangeTypes._

// TODO: who sends the fills back?
case class MutableExchange(private var e: Exchange) {
  def transaction(m: Order): Transaction = {
    val (t, newE) = e.transaction(m)
    e = newE
    t
  }
}

case class Exchange(m: Market) {
  def transaction(msg: Order): TransactionResult[Exchange] = {
    val (t, newMarket) = m.transaction(msg)
    (t, Exchange(newMarket))
  }
}

/*
  A Transaction can represent a buy or an ask, and contains:
    * The number of shares filled, which might be less than the order wanted.
    * The original order (which contains the number of shares desired)
 */
trait Transaction {
  def shares  : Shares
  def origin  : Order
  def isEmpty : Boolean = shares == 0
  def fill(additionalShares: Shares, filler: Order): Transaction
}
// An Unfilled Transaction has no shares filled.
case class Unfilled(origin: Order) extends Transaction {
  val shares = 0
  def fill(s: Shares, filler: Order) = PartialFill(s, List(filler), origin)
}
// A PartialFill has a number of shares filled, but less than wanted.
case class PartialFill(
  shares:  Shares, 
  fillers: List[Order], 
  origin:  Order) extends Transaction {
  def fill(additionalShares: Shares, filler: Order) = 
    if (shares + additionalShares == origin.shares) Fill(origin, filler :: fillers)
    else copy(shares=shares+additionalShares, fillers=filler :: fillers)
}
// A Fill is a completely filled Order
case class Fill(origin: Order, fillers: List[Order]) extends Transaction {
  def shares = origin.shares
  def fill(additionalShares: Shares, filler: Order) = 
    sys.error("can't fill an already full order: " ++ 
      List(origin, fillers, additionalShares, filler).toString)
}

/*
  A Market is just a collection of stocks
 */
case class Market(stocks: Map[Ticker, Market]) {
  def transaction(order: Order): TransactionResult[Market] = {
  	stocks.get(order.ticker).fold((Unfilled(order): Transaction, this)){ stock => 
  	  val (t, newStock) = stock.transaction(order)
  	  (t, Market(stocks + (order.ticker -> newStock)))
  	}
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
case class Stock(ticker: Ticker, bids: MaxHeap[Order], asks: MinHeap[Order]) {
  import Stock._
  def transaction(order: Order) = order.orderType match {
    case Bid => Stock.bid(order, this, Unfilled(order))
    case Ask => Stock.ask(order, this, Unfilled(order))
  }
}

object Stock {
  
  def bid(o: Order, s: Stock, t: Transaction): (Transaction, Stock) = {
    val (t2, newAsks) = fillOrder(o, s.asks, t, Order.bidOrdering) { 
      (bidPrice, askPrice) => bidPrice >= askPrice
    }
    (t2, s.copy(asks=newAsks))
  }

  def ask(o: Order, s: Stock, t: Transaction): (Transaction, Stock) = {
    val (t2, newBids) = fillOrder(o, s.bids, t, Order.askOrdering) { 
      (askPrice, bidPrice) => askPrice <= bidPrice  
    }
    (t2, s.copy(bids=newBids))
  }

  @tailrec
  def fillOrder(
    order      : Order, 
    heap       : Heap[Order], 
    trans      : Transaction, 
    heapOrder  : Ord[Order])
   (priceMatch : (Price, Price) => Boolean): (Transaction, Heap[Order]) = {
    /*
       If either:
         * we have no asks
         * the bid is lower than the current lowest asking price
       Then the bid can't possibly be filled.

       In that case, if we have partially filled the order
         then we do not add it to the heap, we send the PartialFill back to the client
         otherwise we add it to the bids awaiting fulfillment
     */
    if (heap.isEmpty || ! priceMatch(order.price, heap.minimum.price)) 
      trans match {
        case Unfilled(_) => (trans, heap.insert(order)(heapOrder))
        case _           => (trans, heap)
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
      val min = heap.minimum
      val newTransaction = trans.fill(order.shares, min)

      // PERFECT FILL
      if (min.shares == order.shares)
        // we can completely fill the bid.
        (newTransaction, heap.deleteMin)
      // FILL, keeping the current node on top of the heap, adjusting it's shares. 
      else if (min.shares > order.shares)
        // we can completely fill the bid, but the bids shares 
        // must be removed from the ask on top of the heap
        (newTransaction, heap.adjustMin(o=>o.copy(shares=o.shares-order.shares)))
      // PARTIAL FILL
      else
        // we can't completely fill the bid, so we need to recur.
        fillOrder(
          // we recur needing fewer shares, since we filled min.shares shares.
          order.copy(shares=order.shares - min.shares),
          // min.shares is now empty, so it is deleted
          heap.deleteMin,
          // min is preserved in the transaction, though
          newTransaction,
          heapOrder
        )(priceMatch)
    }
  }
}

