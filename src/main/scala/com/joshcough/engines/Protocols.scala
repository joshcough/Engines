package com.joshcough.engines

import scodec._
import scodec.bits._
import codecs._

object ProtocolA {
  import Messages._
  implicit val messageTypeCodec : Codec[MessageType] =
    mappedEnum(uint8, Buy -> 1, Sell -> 2)
  implicit val messageCodec : Codec[Message] =
    (int32 :: ascii32 :: messageTypeCodec :: ascii32 :: int32 :: double).as[Message]
}

