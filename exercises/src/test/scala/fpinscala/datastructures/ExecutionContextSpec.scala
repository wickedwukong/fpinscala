package fpinscala.datastructures

import org.specs2.mutable.Specification
import scala.Predef._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scala.concurrent.Await.result


trait ExecutionContext[T] {
  def execute(f: () => T): Either[Future[T], T]
}


class SyncExecutionContext[T] extends ExecutionContext[T] {
  def execute(f: () => T): Either[Future[T], T] = {
    Right(f())
  }
}

class AsyncExecutionContext[T] extends ExecutionContext[T] {
  def execute(f: () => T): Either[Future[T], T] = {
    Left(Future {
      f()
    })
  }
}

trait ExecutionContext2[T, U] {
  def execute(f: () => T): U
}

class SyncExecutionContext2[T] extends ExecutionContext2[T, T] {
  override def execute(f: () => T): T = {
    f()
  }
}

class AsyncExecutionContext2[T] extends ExecutionContext2[T, Future[T]] {
  def execute(f: () => T): Future[T] = {
    Future {
      f()
    }
  }
}


class ExecutionContextSpec extends Specification {
  def executeInContext(f: () => String, context: ExecutionContext[String]) = {
    context.execute(f)
  }

  "Execution context" should {
    "execute function synchronously" in {
      val f = () => "Hello"
      executeInContext(f, new SyncExecutionContext) must_== Right("Hello")
    }

    "execute function asynchronously" in {
      val f = () => "Hello"
      val leftFutureResult = executeInContext(f, new AsyncExecutionContext()).left

      leftFutureResult.map{result(_, scala.concurrent.duration.Duration.Inf)} must_== Left("Hello")
    }
  }
}

class ExecutionContextSpec2 extends Specification {
  def executeInContext2[U](f: () => String, context: ExecutionContext2[String, U]) = {
    context.execute(f)
  }

  "Execution context" should {
    "execute function synchronously" in {
      val f = () => "Hello"
      executeInContext2[String](f, new SyncExecutionContext2[String]) must_== "Hello"
    }

    "execute function asynchronously" in {
      val f = () => "Hello"
      val futureResult: Future[String] = executeInContext2[Future[String]](f, new AsyncExecutionContext2[String]())

      Await.result(futureResult, scala.concurrent.duration.Duration.Inf) must_== "Hello"
    }
  }
}
