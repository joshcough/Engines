package com.joshcough.engines
package exchange

import scalaz.{Order => Ord, _}
import syntax.applicative._
import syntax.monad._
import Scalaz._
import scala.annotation.tailrec

import OrderTypes._

object StateFunctions {
  def update[S, A](f: S => (A, S)): State[S, A] = 
    for { s <- get[S]; (a, s2) = f(s); _ <- put(s2) } yield a

  def stateTransaction[S](o: Order, s: S)
    (implicit t: TransactionRunner[S]): State[S, Transaction] = 
      update(s => t.runTransaction(o, s))
}

object TransactionRunner {
  def runTransaction[S](o: Order, s: S)
    (implicit t: TransactionRunner[S]): (Transaction, S) = t.runTransaction(o, s)

  def stateTransaction[S](o: Order)(implicit t: TransactionRunner[S]): State[S, Transaction] = 
    StateFunctions.update(s => t.runTransaction(o, s))

  def stateTransactions[S](orders: List[Order])
    (implicit t: TransactionRunner[S]): State[S, List[Transaction]] = {
    type X[A] = State[S, A]
    orders.traverse[X,Transaction](stateTransaction(_))
  }  
}

trait TransactionRunner[S] {
  def runTransaction(o: Order, s: S): (Transaction, S)
}

object ShareData {
  def apply(o: Order) = new ShareData(remaining=o.shares, collected=0, o)
  val bidOrdering: Ord[ShareData] = new Ord[ShareData] {
    def order(x: ShareData, y: ShareData): Ordering = y.order.price ?|? x.order.price
  }
  val askOrdering: Ord[ShareData] = new Ord[ShareData] {
    def order(x: ShareData, y: ShareData): Ordering = x.order.price ?|? y.order.price
  }
}

case class ShareData(remaining: Shares, collected: Shares, order: Order){
  def fill(n: Shares) = {
    val newRemainder = if (n < remaining) remaining - n else 0
    copy(remaining = newRemainder, collected=order.shares - newRemainder)
  }
  override def toString = s"ShareData(remaining: $remaining, collected: $collected, order: $order)"
}

/*
  A Transaction can represent a buy or an ask, and contains:
    * The number of shares filled, which might be less than the order wanted.
    * The original order (which contains the number of shares desired)
 */
trait Transaction {
  def shares  : ShareData
  def origin  : Order
  def fill(additionalShares: ShareData): Transaction
}

  // An Unfilled Transaction has no shares filled.
  case class Unfilled(origin: Order) extends Transaction {
    val shares = ShareData(origin)
    def fill(s: ShareData) = PartialFill(shares.fill(s.remaining), origin, List(s.order))
  }

  // A PartialFill has a number of shares filled, but less than wanted.
  case class PartialFill(
    shares:  ShareData, 
    origin:  Order,
    fillers: List[Order] 
  ) extends Transaction {
    def fill(s: ShareData) = {
      val newShareData = shares.fill(s.remaining)
      if(newShareData.remaining == 0) Fill(newShareData, origin, s.order :: fillers)
      else PartialFill(newShareData, origin, s.order :: fillers)
    }
  }

  // A Fill is a completely filled Order
  case class Fill(shares: ShareData, origin: Order, fillers: List[Order]) extends Transaction {
    def fill(s: ShareData) = sys.error("can't fill an already full order: " + List(this, s).toString)
  }

