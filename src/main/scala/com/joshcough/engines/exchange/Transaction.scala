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

