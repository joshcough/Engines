package com.joshcough.engines

import scodec._
import scodec.bits._
import codecs._
import scalaz._
import Scalaz._

object MessagesTypes {
  type MessageId = Int
  type ClientId  = String
  type Ticker    = String
  type Shares    = Int
  type Price     = Double
}

import MessagesTypes._  

object MessageType {
  implicit val MessageTypeEqual: Equal[MessageType] = Equal.equalA[MessageType]
  implicit val MessageTypeShow : Show[MessageType]  = Show.showFromToString
}

sealed trait MessageType
case object Buy  extends MessageType
case object Sell extends MessageType

object Message {
  implicit val MessageEqual: Equal[Message] = Equal.equalA[Message]
  /** 
    TODO: fix this instance.
    Q: is there anything like deriving Show for scalaz, 
    that sees that all the members of a datatype are Show instances,
    and builds a Show instance for the new datatype appropriately?
  **/
  implicit val MessageShow: Show[MessageType] = Show.showFromToString
}

case class Message(
  id:MessageId, 
  clientId: ClientId, 
  messageType: MessageType, 
  ticker: Ticker, 
  shares: Shares, 
  price: Price
)


