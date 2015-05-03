package com.joshcough.engines
package exchange

import scalaz.{Order => Ord, _}
import syntax.applicative._
import syntax.monad._
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

object StateFunctions {
  def update[S, A](f: S => (A, S)): State[S, A] = for {
    s       <- get[S]
    (a, s2) = f(s)
    _       <- put(s2)
  } yield a

  def stateTransaction[S](o: Order, s: S)
    (implicit t: TransactionRunner[S]): State[S, Transaction] = 
      update(s => t.runTransaction(o, s))
}

object TransactionRunner {
  def runTransaction[S]
    (o: Order, s: S)
    (implicit t: TransactionRunner[S]): (Transaction, S) = t.runTransaction(o, s)
}

trait TransactionRunner[S] {
  def runTransaction(o: Order, s: S): (Transaction, S)

  def stateTransaction(o: Order)(implicit t: TransactionRunner[S]): State[S, Transaction] = 
    StateFunctions.update(s => t.runTransaction(o, s))

  def trasactions
    (orders: List[Order])
    (implicit t: TransactionRunner[S]): State[S, List[Transaction]] = {
    type X[A] = State[S, A]
    orders.traverse[X,Transaction](t.stateTransaction(_))
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
  bids: MaxHeap[Order]=Heap.Empty[Order], 
  asks: MinHeap[Order]=Heap.Empty[Order]){

  def transaction(order: Order): (Transaction, Stock) = 
    order.orderType match {
      case Bid => bid(order)
      case Ask => ask(order)
    }

  private def bid(o: Order): (Transaction, Stock) = {
    val (t2, newAsks, newBids) = 
      fillOrder(o, Unfilled(o), asks, bids, Order.bidOrdering) { 
        (bidPrice, askPrice) => bidPrice >= askPrice
      }
    (t2, copy(bids=newBids, asks=newAsks))
  }

  private def ask(o: Order): (Transaction, Stock) = {
    val (t2, newBids, newAsks) = 
      fillOrder(o, Unfilled(o), bids, asks, Order.askOrdering) { 
        (askPrice, bidPrice) => askPrice <= bidPrice  
      }
    (t2, copy(bids=newBids, asks=newAsks))
  }

  @tailrec
  private def fillOrder (
    order          : Order, 
    trans          : Transaction, 
    drawHeap       : Heap[Order], 
    entryHeap      : Heap[Order], 
    entryHeapOrder : Ord[Order])
   (priceMatch     : (Price, Price) => Boolean): (Transaction, Heap[Order], Heap[Order]) = {
    /*
       If either:
         * we have no asks
         * the bid is lower than the current lowest asking price
       Then the bid can't possibly be filled.

       In that case, if we have partially filled the order
         then we do not add it to the heap, we send the PartialFill back to the client
         otherwise we add it to the bids awaiting fulfillment
     */
    if (drawHeap.isEmpty || ! priceMatch(order.price, drawHeap.minimum.price)) 
      trans match {
        case Unfilled(_) => (trans, drawHeap, entryHeap.insert(order)(entryHeapOrder))
        case _           => (trans, drawHeap, entryHeap)
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
      val min = drawHeap.minimum
      val newTransaction = trans.fill(order.shares, min)

      // PERFECT FILL
      if (min.shares == order.shares)
        // we can completely fill the bid.
        (newTransaction, drawHeap.deleteMin, entryHeap)
      // FILL, keeping the current node on top of the heap, adjusting it's shares. 
      else if (min.shares > order.shares)
        // we can completely fill the bid, but the bids shares 
        // must be removed from the ask on top of the heap
        (newTransaction, drawHeap.adjustMin(o=>o.copy(shares=o.shares-order.shares)), entryHeap)
      // PARTIAL FILL
      else
        // we can't completely fill the bid, so we need to recur.
        fillOrder(
          // we recur needing fewer shares, since we filled min.shares shares.
          order.copy(shares=order.shares - min.shares),
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
