package com.joshcough.engines

import org.scalacheck._
import Shrink._
import Gen._
import Arbitrary.arbitrary
import Prop._
import scodec.bits._
import scodec._
import scodec.bits._
import codecs._
import scalaz._
import Scalaz._

import OrderTypes._

object OrderArbitrary {
  
  val arbTicker: Gen[Ticker]  = oneOf("MSFT", "GOOG", "APPL", "GOLD", "COOL")

  implicit val arbOrderType: Arbitrary[OrderType] = Arbitrary(oneOf(Bid,Ask))

  var id: Int = 0
  def nextId: Int = { id = id + 1; id - 1 }
  val genOrder: Gen[Order] = {
    for { 
      oid <- arbitrary[Int].map(_ => nextId)
      cid <- identifier.map(_.take(10))
      ty  <- arbitrary[OrderType]
      tic <- arbTicker
      sh  <- Gen.choose(1,255)
      pri <- Gen.choose(0.0,1000.0)
    } yield Order(oid, cid, ty, tic, sh, pri)
  }

  implicit val arbOrder: Arbitrary[Order] = Arbitrary(genOrder)
  val genBid: Gen[Order] = genOrder.map(_.copy(orderType=Bid))
  val genAsk: Gen[Order] = genOrder.map(_.copy(orderType=Ask))
  val arbBid: Arbitrary[Order] = Arbitrary(genBid)
  val arbAsk: Arbitrary[Order] = Arbitrary(genAsk)

  val genSimpleOrder: Gen[Order] = genOrder map simplify
  val genSimpleBid:   Gen[Order] = genBid   map simplify
  val genSimpleAsk:   Gen[Order] = genAsk   map simplify

  def simplify(order: Order) = order.copy(ticker="TEST", clientId="c")

  def nonEmptyListOf[A](g: Gen[A]) = for { a <- g; as <- listOf(g) } yield a :: as
}

import OrderArbitrary._

object OrderTests extends EngineProperties("OrderTests") {

}

