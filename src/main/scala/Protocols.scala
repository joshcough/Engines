import scodec._
import scodec.bits._
import codecs._

object ProtocolA {
  import Messages._
  val messageTypeCodec : Codec[MessageType] = mappedEnum(uint8, Buy -> 1, Sell -> 2)
  val messageCodec = (int32 :: ascii32 :: messageTypeCodec :: ascii32 :: int32 :: double).as[Message]
}

