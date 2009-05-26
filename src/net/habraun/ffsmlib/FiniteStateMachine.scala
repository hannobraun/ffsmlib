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



case class FiniteStateMachine(sigma: Alphabet, states: Set[State], initialState: State,
		transitionFunction: (State, Char) => State, finalStates: Set[State]) {
	
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


	
	def accepts(word: String): Boolean = {
		for (symbol <- word) {
			if (!sigma.contains(symbol)) {
				throw new IllegalArgumentException("Invalid symbol: " + symbol)
			}
		}

		def accepts(word: String, state: State): Boolean = {
			if (word.length == 0) {
				finalStates.contains(state)
			}
			else {
				accepts(word.substring(1), transitionFunction(state, word.charAt(0)))
			}
		}

		accepts(word, initialState)
	}
}
