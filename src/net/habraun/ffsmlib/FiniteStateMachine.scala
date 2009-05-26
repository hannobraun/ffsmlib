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



case class FiniteStateMachine(sigma: Set[Char], states: Set[State], initialState: State,
		transitionFunction: PartialFunction[(State, Char), State], finalStates: Set[State]) {
	
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


	
	def accepts(word: String): Boolean = {
		finalStates.contains(stateAfter(word))
	}



	def stateAfter(word: String): State = {
		for (symbol <- word) {
			if (!sigma.contains(symbol)) {
				throw new IllegalArgumentException("Invalid symbol: " + symbol)
			}
		}
		
		def stateAfter(word: String, state: State): State = {
			if (word.length == 0) {
				state
			}
			else {
				stateAfter(word.substring(1), transitionFunction(state, word.charAt(0)))
			}
		}

		stateAfter(word, initialState)
	}



	def apply(word: String): FiniteStateMachine = {
		FiniteStateMachine(sigma, states, stateAfter(word), transitionFunction, finalStates)
	}
}
