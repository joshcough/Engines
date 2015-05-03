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
  
  val arbInt8  : Gen[Int]     = Gen.choose(0,255)
  val arbPrice : Gen[Double]  = Gen.choose(0.0,1000.0)
  val arbId    : Gen[OrderId] = arbitrary[Int]
  val arbTicker: Gen[Ticker]  = oneOf("MSFT", "GOOG", "APPL", "GOLD", "COOL")

  implicit val arbOrderType: Arbitrary[OrderType] = Arbitrary(oneOf(Bid,Ask))

  val genOrder: Gen[Order] = for { 
    mid <- arbId.map(math.abs)
    cid <- identifier.map(_.take(10))
    ty  <- arbitrary[OrderType]
    tic <- arbTicker
    sh  <- arbInt8
    pri <- arbPrice
  } yield Order(mid, cid, ty, tic, sh, pri)

  implicit val arbOrder: Arbitrary[Order] = Arbitrary(genOrder)

  val genBid: Gen[Order] = genOrder.map(_.copy(orderType=Bid))
  val genAsk: Gen[Order] = genOrder.map(_.copy(orderType=Ask))
  val arbBid: Arbitrary[Order] = Arbitrary(genBid)
  val arbAsk: Arbitrary[Order] = Arbitrary(genAsk)
}

import OrderArbitrary._

object OrderTests extends EngineProperties("OrderTests") {

}

