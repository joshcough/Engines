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
  
  val arbInt8 = Gen.choose(0,255)
  val arbId: Gen[OrderId] = arbitrary[Int]
  val arbTicker: Gen[Ticker] = oneOf("MSFT", "GOOG")

  implicit val arbOrderType: Arbitrary[OrderType] = Arbitrary(oneOf(Bid,Ask))

  implicit val arbOrder: Arbitrary[Order] = Arbitrary(for { 
    mid <- arbInt8
    cid <- identifier
    ty  <- arbitrary[OrderType]
    tic <- arbTicker
    sh  <- arbInt8
    pri <- arbitrary[Double].map(math.abs)
  } yield Order(mid, cid, ty, tic, sh, pri))
}

import OrderArbitrary._

object OrderTests extends EngineProperties("OrderTests") {

}

