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
import MessageArbitrary._

abstract class ProtocolProperties(name: String) extends EngineProperties(name){
  def successOrDie[T](a:Attempt[T]) : T = a.fold(e => sys.error(e.message), identity)
  def roundTripTest[T: Equal : Codec](t:T): Boolean = roundTrip(t) === t
  def roundTrip[T: Equal : Codec](t:T): T = successOrDie(roundTripAttempt(t))
  def roundTripAttempt[T:Codec](t:T): Attempt[T] = {
    val c = implicitly[Codec[T]]
    for { bv <- c.encode(t); dr <- c.decode(bv) } yield dr.value
  }
}

object ProtocolAProperties extends ProtocolProperties("ProtocolA tests"){
  import ProtocolA._
  test("round-trip message type")(forAll ((m: MessageType) => roundTripTest(m)))
  test("round-trip")(forAll((m: Message) => roundTripTest(m)))
}

