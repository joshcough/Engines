package com.joshcough.engines

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._

abstract class EngineProperties(name: String) extends Properties(name){
  def test(name:String)(f: => Prop) = property(name) = secure { trying(f) }
  def trying(f: => Prop) = try f catch { 
  	case e: java.lang.Throwable  => e.printStackTrace(System.err); throw e 
  }
}
