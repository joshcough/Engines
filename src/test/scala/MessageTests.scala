import org.scalacheck._
import Shrink._
import Gen._
import Arbitrary.arbitrary
import Prop._
import Messages._
import scodec.bits._
import scodec._
import scodec.bits._
import codecs._
import scalaz._
import Scalaz._

object MessageArbitrary {
  
  val arbInt8 = Gen.choose(0,255)
  val arbId: Gen[MessageId] = arbitrary[Int]
  val arbTicker: Gen[Stock] = oneOf("MSFT", "GOOG")

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

abstract class MessageProperties(name: String) extends EngineProperties(name){
  def successOrDie[T](a:Attempt[T]) : T = a.fold(e => sys.error(e.message), identity)
  def roundTripTest[T: Equal](c:Codec[T])(t:T): Boolean = roundTrip(c)(t) === t
  def roundTrip[T: Equal](c:Codec[T])(t:T): T = successOrDie(roundTripAttempt(c)(t))
  def roundTripAttempt[T](c:Codec[T])(t:T): Attempt[T] = 
    for { bv <- c.encode(t); dr <- c.decode(bv) } yield dr.value
}

object MessageTests extends EngineProperties("MessageTests") {}

object ProtocolAProperties extends MessageProperties("ProtocolA tests"){
  import ProtocolA._
  test("round-trip message type")(forAll ((m: MessageType) => roundTripTest(messageTypeCodec)(m)))
  test("round-trip")(forAll((m: Message) => roundTripTest(messageCodec)(m)))
}

