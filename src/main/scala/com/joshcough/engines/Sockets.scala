package com.joshcough.engines

import scalaz.stream._
import tcp._
import Process._
import java.net.InetSocketAddress
import scalaz.\/
import scalaz.concurrent.{Strategy,Task}
import scalaz.syntax.monad._
import scodec._
import bits._
import codecs._
import stream.{encode,StreamEncoder, decode => D, StreamDecoder}

object SCodecStreamSockets {
  def encodeToSocket[T](p: Process[Nothing,T])
    (implicit c: Codec[T]): Process[Connection, ByteVector] = {
    val encoder : Process1[T,BitVector] = encode.many(c).encoder
    tcp.lift(p.toSource |> encoder.map(_.toByteVector))
  }
  def decodeFromSocket[T:Codec]: Process[Connection,T] =
    (tcp.reads(10).map(_.toBitVector) |> scodec.stream.decode.process[T]).repeat
}

import SCodecStreamSockets._

object Sockets {
  val addr = new InetSocketAddress("localhost", 8007)
  implicit val S = scalaz.concurrent.Strategy.DefaultStrategy
  implicit val AG = tcp.DefaultAsynchronousChannelGroup
}

import Sockets._
import ProtocolA._

// This guy open a socket, reads stuff, and prints it.
object PrintSocket {
  import tcp.syntax._
  val s: Process[Task,Throwable\/Unit] = 
    tcp.server(addr, concurrentRequests = 1)(
      decodeFromSocket[Order].map(_.toString).to(io.stdOut).repeat
    ).join
  def main(args: Array[String]): Unit = s.run.run
}

// This guy sends scodec serialized Orders
object SenderSocket {
  import tcp.syntax._
  val Orders = List(
    Order(0, "Josh", Bid, "APPL", 50, 600)
   ,Order(0, "Paul", Ask, "GOOG", 10, 500)
   ,Order(0, "Mike", Bid, "MSFT", 50,  95)
  )
  val encoder = encodeToSocket(Process(Orders:_*))
  def main(args: Array[String]): Unit = tcp.connect(addr)(encoder).run.run
}
