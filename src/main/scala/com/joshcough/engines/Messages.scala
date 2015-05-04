package com.joshcough.engines

import scodec._
import scodec.bits._
import codecs._
import scalaz.{Order => Ord, _}
import Scalaz._

object OrderTypes {
  type OrderId  = Int
  type ClientId = String
  type Ticker   = String
  type Shares   = Int
  type Price    = Double
}

import OrderTypes._  

object OrderType {
  implicit val OrderTypeEqual: Equal[OrderType] = Equal.equalA[OrderType]
  implicit val OrderTypeShow : Show[OrderType]  = Show.showFromToString
}

sealed trait OrderType
case object Bid extends OrderType
case object Ask extends OrderType

object Order {

  implicit val OrderEqual: Equal[Order] = Equal.equalA[Order]
  /** 
    TODO: fix this instance.
    Q: is there anything like deriving Show for scalaz, 
    that sees that all the members of a datatype are Show instances,
    and builds a Show instance for the new datatype appropriately?
  **/
  implicit val OrderShow: Show[Order] = Show.showFromToString
}

case class Order(
  id       : OrderId, 
  clientId : ClientId, 
  orderType: OrderType, 
  ticker   : Ticker, 
  shares   : Shares, 
  price    : Price)

