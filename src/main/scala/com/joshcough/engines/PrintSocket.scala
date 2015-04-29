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

object PrintSocket {

  implicit val S = scalaz.concurrent.Strategy.DefaultStrategy
  implicit val AG = tcp.DefaultAsynchronousChannelGroup
  val E = java.util.concurrent.Executors.newCachedThreadPool
  val S2 = Strategy.Executor(E)
  import tcp.syntax._

  def addr(offset:Int) = new InetSocketAddress("localhost", 8000 + offset)
  def server(addr:InetSocketAddress) = 
    tcp.server(addr, concurrentRequests = 1)(tcp.reads(1024).to(io.stdOutBytes).repeat).join  

  def main(args: Array[String]): Unit = {
    server(addr(args(0).toInt)).run.run
  }
}
