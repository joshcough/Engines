package com.joshcough.engines

import scalaz._
import Scalaz._

import MessagesTypes._

object ExchangeTypes {
  type Bid  = Message
  type Ask  = Message
  type Fill = Message
  type MaxHeap = Heap[Bid] // ordered by price, high to low
  type MinHeap = Heap[Bid] // ordered by price, low  to high
  type TransactionResult[A] = (Option[Transaction], A)
  type TransactionRun[A] = (A, Message) => TransactionResult[A]
  type TransactionAttempt[A,B] = (Message, TransactionRun[A]) => TransactionResult[B]
}

import ExchangeTypes._

object Transaction {
   def none: Option[Transaction] = None
}

case class Transaction(bidFill: Option[Fill], askFills: List[Fill])

case class Market(stocks: Map[Ticker, Stock]) {
  def bid(b: Bid): TransactionResult[Market] = transaction(b, _ bid _)
  def ask(a: Ask): TransactionResult[Market] = transaction(a, _ ask _)
  def transaction: TransactionAttempt[Stock, Market] = (msg, r) => 
  	stocks.get(msg.ticker).fold((Transaction.none, this)){ stock => 
  	  val (t, newStock): TransactionResult[Stock] = r(stock, msg)
  	  (t, Market(stocks + (msg.ticker -> newStock)))
  	}
}

object Stock {
  // Greatest bids go on top heap (Max heap)
  val bidOrdering: Order[Bid] = new Order[Bid] {
    def order(x: Bid, y: Bid): Ordering = y.price ?|? x.price
  }
  // Lowest asks go on top of the heap (Min heap)
  val askOrdering: Order[Ask] = new Order[Ask] {
    def order(x: Ask, y: Ask): Ordering = x.price ?|? y.price
  }
}

case class Stock(t:Ticker, bids: MaxHeap, asks: MinHeap) {
  import Stock._
  // to get the minimum from a Heap, call: h.minimum
  //def insert(a: A)(implicit o: Order[A]) = insertWith(o.lessThanOrEqual, a)
  def bid(b: Bid): TransactionResult[Stock] =
    /*
       If either:
         * we have no asks
         * the bid is lower than the current lowest asking price
       Then the bid can't possibly be filled.
       So, just add it to the bids awaiting fulfillment.
     */
    if (asks.isEmpty || b.price < asks.minimum.price) {
      val newBids = bids.insert(b)(bidOrdering)
      (Transaction.none, Stock(t, newBids, asks))
    }
	/* 
	 We can now either partially fill, or completely fill the bid.
	 Basic idea:
	   Keep taking asks (with price <= bid.price) off the top of the heap 
	   Until we've reached the number of shares in the bid

	   If we run out of asks, then the bid only gets partially filled.
	   If we don't, then we can full fill the bid.

	   Special case:
	     Unless the last ask (with price <= bid.price) is completely drained
	     We need to make sure that last ask is put back on top of the heap (or never removed)/
     */
    else ???

  def ask(a: Ask): TransactionResult[Stock] =
     /*
       If either:
         * we have no bids
         * the ask is higher than the current highest bidding price
       Then the ask can't possibly be filled.
       So, just add it to the asks awaiting fulfillment.
     */
  	if (bids.isEmpty) {
      val newAsks = asks.insert(a)(askOrdering)
      (Transaction.none, Stock(t, bids, newAsks))
    }
    else ???	
}