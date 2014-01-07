package fpinscala.state

import org.specs2.mutable.Specification

class StateSimulateMachineSpec extends Specification {

  "locked machine with no candy" should {
    val lockedNoCandyMachine: Machine = Machine(true, 0, 1)

    "ignore coin input" in {
      val stateMachine: State[Machine, Int] = State.simulateMachine(List(Coin))

      stateMachine.run(lockedNoCandyMachine) must_==(lockedNoCandyMachine.coins, lockedNoCandyMachine)
    }

    "ignore turn input" in {
      val stateMachine: State[Machine, Int] = State.simulateMachine(List(Turn))

      stateMachine.run(lockedNoCandyMachine) must_==(lockedNoCandyMachine.coins, lockedNoCandyMachine)
    }
  }
  
  "unlocked machine with no candy" should {
    val unlockedNoCandyMachine: Machine = Machine(false, 0, 1)

    "ignore coin input" in {
      val stateMachine: State[Machine, Int] = State.simulateMachine(List(Coin))

      stateMachine.run(unlockedNoCandyMachine) must_==((unlockedNoCandyMachine.coins, unlockedNoCandyMachine))
    }

    "ignore turn input" in {
      val stateMachine: State[Machine, Int] = State.simulateMachine(List(Turn))

      stateMachine.run(unlockedNoCandyMachine) must_==((unlockedNoCandyMachine.coins, unlockedNoCandyMachine))
    }
  }

  "locked machine" should {
    val lockedMachine: Machine = Machine(true, 1, 1)

    "unlock when taking one coin input" in {
      val stateMachine: State[Machine, Int] = State.simulateMachine(List(Coin))

      stateMachine.run(lockedMachine) must_==((2, Machine(false, 1, 2)))
    }

    "remain locked when taking one turn input" in {
      val stateMachine: State[Machine, Int] = State.simulateMachine(List(Turn))

      stateMachine.run(lockedMachine) must_==((1, Machine(true, 1, 1)))
    }
  }

  "unlocked machine" should {
    val unlockedMachine: Machine = Machine(false, 1, 1)
    "do nothing when taking one coin input" in {
      val stateMachine: State[Machine, Int] = State.simulateMachine(List(Coin))

      stateMachine.run(unlockedMachine) must_==((1, Machine(false, 1, 1)))
    }

    "lock and dispense 1 candy when taking one turn input" in {
      val stateMachine: State[Machine, Int] = State.simulateMachine(List(Turn))

      stateMachine.run(unlockedMachine) must_==((1, Machine(true, 0, 1)))
    }
  }

  "a more comprehensive test" should {
    "test a few inputs with a locked machine" in {
      val lockedMachine: Machine = Machine(true, 5, 10)

      val stateMachine: State[Machine, Int] = State.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))

      stateMachine.run(lockedMachine) must_==((14, Machine(true, 1, 14)))

    }
    
    "test a few inputs with a unlocked machine" in {
      val unlockedMachine: Machine = Machine(false, 5, 10)

      val stateMachine: State[Machine, Int] = State.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))

      stateMachine.run(unlockedMachine) must_==((13, Machine(true, 1, 13)))

    }
  }
}
