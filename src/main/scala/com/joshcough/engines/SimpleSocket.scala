package com.joshcough.engines

import scalaz.stream._

import Cause._
import Process._
import java.net.InetSocketAddress
import java.util.concurrent.CountDownLatch
import scala.concurrent.SyncVar
import scala.util.Random
import scalaz.-\/
import scalaz.\/
import scalaz.\/-
import scalaz.concurrent.{Strategy,Task}
import scalaz.stream.Process.Halt
import scalaz.stream.ReceiveY._
import scalaz.syntax.monad._
import scodec._
import scodec.bits._
import codecs._

object Addr {
   val addr = new InetSocketAddress("localhost", 8007)
}

object SimpleSocket {
  implicit val S = scalaz.concurrent.Strategy.DefaultStrategy
  implicit val AG = tcp.DefaultAsynchronousChannelGroup
  import tcp.syntax._

  def server(addr:InetSocketAddress) = 
    tcp.server(addr, concurrentRequests = 1)(tcp.reads(1024).to(io.stdOutBytes).repeat).join  

  def main(args: Array[String]): Unit = server(Addr.addr).run.run
}

object SimpleClient {
  implicit val S = scalaz.concurrent.Strategy.DefaultStrategy
  implicit val AG = tcp.DefaultAsynchronousChannelGroup
  import tcp.syntax._
  def main(args: Array[String]): Unit = tcp.connect(Addr.addr)(
    Process("hi\n", "world\n")
  ).run.run 
}

