package fpinscala.parallelism

import org.specs2.mutable.Specification
import java.util.concurrent._

class NonblockingSpec extends Specification {

  import Nonblocking._
  import Nonblocking.Par._

  "parFilter" should {
    "work like a filter" in {
      val filter: Par[List[Int]] = parFilter(List(1, 2, 3))(_ > 2)
      val actualResultHolder = new java.util.concurrent.atomic.AtomicReference[List[Int]]
      filter.apply(new BlockingThreadExecutor).apply(actualResultHolder.set(_))

      actualResultHolder.get() must_== (List(3))
    }
  }

  "parMap" should {
    "map a list" in {
      val parMapList: Par[List[Int]] = parMap(List(1,2))(_ + 1)

      val actualResultHolder = new java.util.concurrent.atomic.AtomicReference[List[Int]]
      parMapList.apply(new BlockingThreadExecutor).apply(actualResultHolder.set(_))

      actualResultHolder.get() must_== (List(2, 3))
    }

  }

  "sequence" should {

  }
}

class BlockingThreadExecutor extends AbstractExecutorService{
  def execute(command: Runnable) = command.run()

  def shutdown() = true

  def shutdownNow() = new java.util.ArrayList[Runnable]()

  def isShutdown = true

  def isTerminated = true

  def awaitTermination(timeout: Long, unit: TimeUnit) = true
}