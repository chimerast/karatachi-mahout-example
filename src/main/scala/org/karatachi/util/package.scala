package org.karatachi

import java.security.MessageDigest

package object util {
  def md5(str: String): String = {
    val md5 = MessageDigest.getInstance("MD5")
    md5.reset()
    md5.update(str.getBytes("UTF-8"))

    md5.digest().map(0xFF & _).map("%02x".format(_)).mkString
  }

  def using[S <% { def close(): Unit }, T](what: S)(block: S => T): T = {
    try {
      block(what)
    } finally {
      what.close
    }
  }

  val from = (0, 0)

  def section[T](title: String)(content: => T): T = {
    val Array(chapter, section) = title.split("(\\.| )").take(2).map(_.toInt)
    if (chapter < from._1 || (chapter == from._1 && section < from._2)) {
      return null.asInstanceOf
    }

    println("## " + title)
    val ret = content
    println()
    ret
  }

  def subsection[T](title: String)(content: => T): T = {
    println("### " + title)
    content
  }

  def output(line: Any): Unit = {
    println("    " + line)
  }
}
