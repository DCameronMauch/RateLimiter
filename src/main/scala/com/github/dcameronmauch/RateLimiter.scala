package com.github.dcameronmauch

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

class RateLimiter[T](val rate: Int, val interval: Int) {
  class MyList() {
    class Node(val time: Long, var prev: Option[Node], var next: Option[Node])

    private var head: Option[Node] = None
    private var tail: Option[Node] = None
    private var size: Int = 0

    @tailrec
    private def clean(time: Long): Unit =
      head match {
        case Some(node) if (node.time < time) =>
          head = node.next
          if (head.isEmpty) tail = None
          size -= 1
          clean(time)
        case _ => ()
      }

    def add: Boolean = {
      val time: Long = System.currentTimeMillis()
      clean(time - interval)
      if (size >= rate) false
      else {
        val node: Node = new Node(time, tail, None)
        tail.foreach(_.next = Some(node))
        tail = Some(node)
        if (head.isEmpty) head = Some(node)
        size += 1
        true
      }
    }

    def isEmpty: Boolean = size == 0
    def nonEmpty: Boolean = size > 0
  }

  private val map: MMap[T, MyList] = MMap.empty

  def accept(key: T): Boolean =
    if (map.contains(key))
      map(key).add
    else {
      map += key -> new MyList()
      map(key).add
    }

  def clean(): Unit = map
    .keys
    .toList
    .filter(key => map(key).isEmpty)
    .foreach(map.remove)
}
