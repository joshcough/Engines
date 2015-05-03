package com.joshcough.engines

import scodec._
import scodec.bits._
import codecs._

object ProtocolA {
  implicit val orderTypeCodec : Codec[OrderType] =
    mappedEnum(uint8, Bid -> 1, Ask -> 2)
  implicit val messageCodec : Codec[Order] =
    (int32 :: ascii32 :: orderTypeCodec :: ascii32 :: int32 :: double).as[Order]
}

