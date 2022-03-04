package com.github.dcameronmauch

import org.scalatest.funsuite.AnyFunSuite

class RateLimiterTest extends AnyFunSuite {
  test("burst") {
    val rateLimiter: RateLimiter[String] = new RateLimiter(3, 1000)

    assert(rateLimiter.accept("foobar"))
    assert(rateLimiter.accept("foobar"))
    assert(rateLimiter.accept("foobar"))

    assert(!rateLimiter.accept("foobar"))
    assert(!rateLimiter.accept("foobar"))
    assert(!rateLimiter.accept("foobar"))

    Thread.sleep(1000)

    assert(rateLimiter.accept("foobar"))
    assert(rateLimiter.accept("foobar"))
    assert(rateLimiter.accept("foobar"))
  }

  test("spaced out") {
    val rateLimiter: RateLimiter[String] = new RateLimiter(3, 1000)

    assert(rateLimiter.accept("foobar"))
    Thread.sleep(300)
    assert(rateLimiter.accept("foobar"))
    Thread.sleep(300)
    assert(rateLimiter.accept("foobar"))
    Thread.sleep(300)
    assert(!rateLimiter.accept("foobar"))
    Thread.sleep(300)
    assert(rateLimiter.accept("foobar"))
    Thread.sleep(300)
    assert(rateLimiter.accept("foobar"))
    Thread.sleep(300)
  }
}
