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



/**
 * Models a finite-state machine (FSM).
 * A finite-state machine consists of the following elements:
 * * An alphabet.
 * * A set of states.
 * * The initial state of the FSM, before any words have been processed.
 * * A transition function, which takes a state and a symbol from the alphabet as parameters, and returns a
 *   new state. The transition function needs to be complete, i.e. every combination of state and symbol must
     be accepted.
 * * A set of final states, which is a subset of the FSM's states.
 *
 * FiniteStateMachine will verify that its parameters are valid and will throw an IllegalArgumentException on
 * construction if they are not.
 *
 * FiniteStateMachine is immutable. This means that after it has been constructed, no changes can be made to
 * it.
 */

case class FiniteStateMachine(sigma: Set[Char], states: Set[String], initialState: String,
		transitionFunction: PartialFunction[(String, Char), String], finalStates: Set[String]) {
	
	// Check if any of the arguments is null.
	if (sigma == null || states == null || initialState == null || transitionFunction == null
			|| finalStates == null)
		throw new IllegalArgumentException("Parameters must not be null.")

	// Verify that the initial state is a valid state.
	if (!states.contains(initialState))
		throw new IllegalArgumentException("Initial state is not a valid state.")

	// Verify that the initial states are a subset of the FSM's states.
	if (!finalStates.subsetOf(states))
		throw new IllegalArgumentException("Final states are not a subset of the FSM's states.")

	// Verify that the transition function is complete.
	for ( state <- states; symbol <- sigma) {
		if (!transitionFunction.isDefinedAt((state, symbol)))
			throw new IllegalArgumentException("Transition function is incomplete. Doesn't accept"
					+ " parameter (" + state + ", " + symbol + ").")
	}



	/**
	 * Returns a new FiniteStateMachine instance, which is the same as this instance, except the initial
	 * state is changed to the state the FSM has after the given word has been processed.
	 */

	def apply(word: String): FiniteStateMachine = {
		FiniteStateMachine(sigma, states, stateAfter(word), transitionFunction, finalStates)
	}



	/**
	 * Returns true, if the FSM accepts the given word (i.e. the state after processing is a final state),
	 * false otherwise.
	 */
	
	def accepts(word: String): Boolean = {
		finalStates.contains(stateAfter(word))
	}



	/**
	 * Returns the state of the FSM after the given word has been processed.
	 */

	def stateAfter(word: String): String = {
		for (symbol <- word) {
			if (!sigma.contains(symbol)) {
				throw new IllegalArgumentException("Invalid symbol: " + symbol)
			}
		}
		
		def stateAfter(word: String, state: String): String = {
			if (word.length == 0) {
				state
			}
			else {
				stateAfter(word.substring(1), transitionFunction(state, word.charAt(0)))
			}
		}

		stateAfter(word, initialState)
	}
}
