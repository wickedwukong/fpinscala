package fpinscala.parallelism

import org.specs2.mutable.Specification
import java.util.concurrent._

class NonblockingSpec extends Specification {

  import Nonblocking._
  import Nonblocking.Par._

  "parFilter" should {
    "work like a filter" in {
      val filter: (ExecutorService) => Future[List[Int]] = parFilter(List(1, 2, 3))(_ > 2)
      val actualResultHolder = new java.util.concurrent.atomic.AtomicReference[List[Int]]
      filter.apply(new BlockingThreadExecutor).apply(actualResultHolder.set(_))

      actualResultHolder.get() must_== (List(3))
    }
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