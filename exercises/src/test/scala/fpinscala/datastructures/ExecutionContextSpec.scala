package fpinscala.datastructures

import org.specs2.mutable.Specification
import scala.Predef._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success


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
      Left(Future{f()})
    }
 }


class ExecutionContextSpec extends Specification {
  def executeInContext(f: () => String, context: ExecutionContext[String]) = {
    context.execute(f)
  }

  "Execution context" should {
    "execute function synconousely" in {
      val f = () => "Hello"
      executeInContext(f, new SyncExecutionContext) must_== Right("Hello")
    }

  "execute function asynconousely" in {
      val f = () => "Hello"
      val leftFutureResult = executeInContext(f, new AsyncExecutionContext()).left

      leftFutureResult.map(_.value) must_== Left(Some(Success("Hello")))
    }

  }

}
