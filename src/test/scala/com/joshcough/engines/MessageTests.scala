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

import MessagesTypes._

object MessageArbitrary {
  
  val arbInt8 = Gen.choose(0,255)
  val arbId: Gen[MessageId] = arbitrary[Int]
  val arbTicker: Gen[Ticker] = oneOf("MSFT", "GOOG")

  implicit val arbMessageType: Arbitrary[MessageType] = Arbitrary(oneOf(Buy,Sell))

  implicit val arbMessage: Arbitrary[Message] = Arbitrary(for { 
    mid <- arbInt8
    cid <- identifier
    ty  <- arbitrary[MessageType]
    tic <- arbTicker
    sh  <- arbInt8
    pri <- arbitrary[Double].map(math.abs)
  } yield Message(mid, cid, ty, tic, sh, pri))
}

import MessageArbitrary._

object MessageTests extends EngineProperties("MessageTests") {

}

