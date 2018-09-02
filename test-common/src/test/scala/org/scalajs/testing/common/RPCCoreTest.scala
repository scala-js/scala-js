package org.scalajs.testing.common

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import java.util.concurrent.atomic.AtomicInteger

import org.junit.Test
import org.junit.Assert._

class RPCCoreTest {
  import RPCCoreTest._

  lazy val x: TestRPC = new TestRPC(y)
  lazy val y: TestRPC = new TestRPC(x)

  object eps {
    val simple: RPCEndpoint.EP[Unit, Unit] = RPCEndpoint[Unit, Unit](2)
    val number: RPCEndpoint.EP[Unit, Int] = RPCEndpoint[Unit, Int](3)
    val msg0: MsgEndpoint.EP[Int] = MsgEndpoint[Int](4)
    val msg1: MsgEndpoint.EP[Int] = MsgEndpoint[Int](5)
  }

  private def fail(msg: String): Nothing = {
    org.junit.Assert.fail(msg)
    throw new AssertionError("Shouldn't reach here")
  }

  @Test
  def simpleEndpoint: Unit = {
    var called = false
    x.attach(eps.simple)((_: Unit) => called = true)
    Await.result(y.call(eps.simple)(()), atMost = 1.second)
    assertTrue(called)
  }

  @Test
  def multiplePendingCalls: Unit = {
    val p = Promise[Int]

    x.attachAsync(eps.number)(_ => p.future)

    val futures = List.fill(20)(y.call(eps.number)(()))

    p.success(1)

    val results = Await.result(Future.sequence(futures), atMost = 10.second)
    assertEquals(List.fill(20)(1), results)
  }

  @Test
  def singleMsgEndpoint: Unit = {
    var intMsg = 0
    x.attach(eps.msg0)(intMsg = _)
    assertEquals(0, intMsg)

    y.send(eps.msg0)(1)
    assertEquals(1, intMsg)
  }

  @Test
  def msgEndpointOrdering: Unit = {
    var calls: List[(Int, Int)] = Nil

    x.attach(eps.msg0)(calls ::= (0, _))
    x.attach(eps.msg1)(calls ::= (1, _))

    val numbers = 0 to 10

    for (i <- numbers) {
      y.send(eps.msg0)(i)
      y.send(eps.msg1)(i)
    }

    val expected = for {
      i <- numbers
      c <- 0 to 1
    } yield (c, i)

    assertArrayEquals(expected.toArray.asInstanceOf[Array[Object]],
        calls.reverse.toArray.asInstanceOf[Array[Object]])
  }

  @Test
  def msgRPCOrdering: Unit = {
    val msgsX = new AtomicInteger(0)
    val msgsY = new AtomicInteger(0)

    x.attach(eps.msg0)(_ => msgsX.incrementAndGet())
    y.attach(eps.msg0)(_ => msgsY.incrementAndGet())

    val p = Promise[Unit]()

    x.attachAsync(eps.simple) { _ =>
      // Message from y must be here by now.
      assertEquals(1, msgsX.get())
      assertEquals(0, msgsY.get())

      x.send(eps.msg0)(0)

      p.future
    }

    y.send(eps.msg0)(0)
    p.success(())

    Await.result(y.call(eps.simple)(()), atMost = 1.second)

    // Message from x must be here by now.
    assertEquals(1, msgsX.get())
    assertEquals(1, msgsY.get())
  }

  @Test
  def unboundEndpoint: Unit = {
    try {
      y.call(eps.simple)(())
      fail("Expected exception")
    } catch {
      case e: IllegalStateException =>
        assertEquals(s"Unknown opcode: ${eps.simple.opCode}", e.getMessage())
    }
  }

  @Test
  def remoteException: Unit = {
    val msg0 = "My message for the outer exception"
    val msg1 = "My message for the inner exception"
    x.attach(eps.simple)(
        (_: Unit) => throw new Exception(msg0, new Exception(msg1)))

    try {
      Await.result(y.call(eps.simple)(()), atMost = 1.second)
      fail("Expected exception")
    } catch {
      case e: RPCCore.RPCException =>
        assertNotNull(e.getCause())
        assertEquals(msg0, e.getCause().getMessage())
        assertNotNull(e.getCause().getCause())
        assertEquals(msg1, e.getCause().getCause().getMessage())
    }
  }

  @Test
  def closeChannel: Unit = {
    // Attach something that never completes.
    x.attachAsync(eps.number)((_: Unit) => Promise[Int].future)

    val future = y.call(eps.number)(())

    val cause = new Throwable("blah")
    y.close(cause)

    try {
      Await.result(future, atMost = 1.second)
      fail("Expected exception")
    } catch {
      case e: RPCCore.ClosedException =>
        assertSame(cause, e.getCause())
    }
  }
}

object RPCCoreTest {
  class TestRPC(otherThunk: => TestRPC) extends RPCCore {
    private lazy val other = otherThunk
    protected def send(msg: String): Unit = other.handleMessage(msg)
  }
}
