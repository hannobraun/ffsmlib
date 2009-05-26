/*
	Copyright (c) 2009 Hanno Braun <hanno@habraun.net>

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

		http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.
*/



package net.habraun.ffsmlib



import scala.collection.immutable._

import org.junit._
import org.junit.Assert._



class FiniteStateMachineTest {

	val sigma = HashSet('0', '1')
	val S0 = State("s0")
	val S1 = State("s1")
	val states = HashSet(S0, S1)
	val transitionFunction: PartialFunction[(State, Char), State] = {
		case (S0, '0') => S0
		case (S0, '1') => S1
		case (S1, '0') => S0
		case (S1, '1') => S1
	}
	val finalStates = HashSet(S1)

	var fsm: FiniteStateMachine = null



    @Before
    def createFSM {
		fsm = FiniteStateMachine(sigma, states, S0, transitionFunction, finalStates)
	}



	@Test
	def giveValidWord {
		assertTrue(fsm.accepts("01"))
	}



	@Test
	def giveInvalidWord {
		assertFalse(fsm.accepts("00"))
	}



	@Test { val expected = classOf[IllegalArgumentException] }
	def giveWordWithInvalidChars {
		fsm.accepts("ab")
	}



	@Test { val expected = classOf[IllegalArgumentException] }
	def createFSMWithInitialStateThatIsNotInStates {
		FiniteStateMachine(sigma, states, State("s8"), transitionFunction, finalStates)
	}



	@Test { val expected = classOf[IllegalArgumentException] }
	def createFSMWithFinalStatesThatAreNotAllValidStates {
		FiniteStateMachine(sigma, states, S0, transitionFunction, HashSet(S1, State("s8")))
	}



	@Test { val expected = classOf[IllegalArgumentException] }
	def createFSMWithNullAlphabet {
		FiniteStateMachine(null, states, S0, transitionFunction, finalStates)
	}



	@Test { val expected = classOf[IllegalArgumentException] }
	def createFSMWithNullStates {
		FiniteStateMachine(sigma, null, S0, transitionFunction, finalStates)
	}



	@Test { val expected = classOf[IllegalArgumentException] }
	def createFSMWithNullS0 {
		FiniteStateMachine(sigma, states, null, transitionFunction, finalStates)
	}



	@Test { val expected = classOf[IllegalArgumentException] }
	def createFSMWithNullTransitionFunction {
		FiniteStateMachine(sigma, states, S0, null, finalStates)
	}



	@Test { val expected = classOf[IllegalArgumentException] }
	def createFSMWithNullFinalStates {
		FiniteStateMachine(sigma, states, S0, transitionFunction, null)
	}



	@Test { val expected = classOf[IllegalArgumentException] }
	def createFSMWithIncompleteTransitionFunction {
		val transitionFunction: PartialFunction[(State, Char), State] ={
			case (S0, '0') => S0
			case (S0, '1') => S1
			case (S1, '0') => S0
		}

		FiniteStateMachine(sigma, states, S0, transitionFunction, finalStates)
	}
}
