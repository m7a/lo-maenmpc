// Ma_Sys.ma Erlang NCurses Music Player Client
// Promela Modle for [NO]IDLE verification - 2024 Ma_Sys.ma <info@masysma.net>

// spin -run [-i] -f idle_crash.pml
// spin -s -r -replay idle_crash.pml
// alt. spin -t -p idle_crash.pml

#define QUEUESIZE 1

mtype = {
	// syncidle states
	S_IDLE, S_TX_PRE, S_TX_PROCESSING, S_INTERRUPTED,
	// syncidle messages in
	M_MPD_IDLE, M_INTERRUPT_NO_TX, M_TX_BEGIN, M_TX_END,
	// syncidle messages out
	M_IDLE_ENTER, M_ERLMPD_NOIDLE
	// non-syncidle messages
	M_REQUEST_UPDATE
}

bool is_idle = true;

chan syncidle_down     = [QUEUESIZE] of {mtype}; // down -> maenmpc_mpd
chan syncidle_down_ack = [1]         of {mtype}; //      <- maenmpc_mpd
chan syncidle_in       = [QUEUESIZE] of {mtype}; //      <- maenmpc_singleplayer
chan syncidle_in_ack   = [1]         of {mtype};
chan syncidle_up       = [QUEUESIZE] of {mtype}; // up   -> maenmpc_singleplayer
chan syncidle_up_ack   = [1]         of {mtype};

active proctype syncidle() { 
	mtype state = S_IDLE;

	do
	:: syncidle_in ? M_MPD_IDLE -> // cast
		if
		:: (state == S_IDLE) || (state == S_INTERRUPTED) ->
			syncidle_up       ! M_MPD_IDLE;
			syncidle_down     ! M_IDLE_ENTER; // call down!
			syncidle_down_ack ? M_IDLE_ENTER; // (await reply)
			state = S_IDLE;
		:: state == S_TX_PRE ->
			syncidle_in_ack ! M_TX_BEGIN; // delayed reply!
			state = S_TX_PROCESSING;
		:: else ->
			assert(0); // not expected to happen
		fi

	:: syncidle_in ? M_INTERRUPT_NO_TX -> // call
		if
		:: (state == S_IDLE) ->
			syncidle_down   ! M_ERLMPD_NOIDLE;
			syncidle_in_ack ! M_INTERRUPT_NO_TX;
			state = S_INTERRUPTED;
		:: (state == S_INTERRUPTED) ->
			syncidle_in_ack ! M_INTERRUPT_NO_TX;
		:: else ->
			assert(0); // not expected to happen
		fi

	:: syncidle_in ? M_TX_BEGIN -> // call
		if
		:: (state == S_IDLE) ->
			syncidle_down ! M_ERLMPD_NOIDLE;
			// delay reply! (syncidle_in_ack M_TX_BEGIN)
			state = S_TX_PRE;
		:: (state == S_INTERRUPTED) ->
			state = S_TX_PRE;
		:: else ->
			assert(0); // not expected to happen
		fi

	:: syncidle_in ? M_TX_END ->
		if
		:: (state == S_TX_PROCESSING) ->
			syncidle_down ! M_IDLE_ENTER
			state = S_IDLE;
		:: else ->
			assert(0); // not expected to happen
		fi
accept:
	od
}

active proctype mpd() {
	do
	// syncidle_down ? M_MPD_IDLE does not exist in the simulation in
	// reality M_IDLE_ENTER would self-message to trigger this
	:: syncidle_down ? M_IDLE_ENTER ->
		is_idle = true;
		syncidle_down_ack ! M_IDLE_ENTER
	:: syncidle_down ? M_ERLMPD_NOIDLE ->
		if
		:: is_idle ->
			syncidle_in ! M_MPD_IDLE;
			is_idle = false;
		:: else ->
			// if already idle, the real MPD would return some sort
			// of “error” but in erlang implementation we drop such
			// errors hence it is as if we were to drop the noidle
			// call altogether...
			true
		fi
	// can nondeterministically also decide to leave idle state.
	// this is intended to provide the emulation mpd_server_emul completely?
	:: is_idle ->
		printf("mpd: nondet leave idle\n");
		is_idle = false;
		syncidle_in ! M_MPD_IDLE
	:: true ->
		printf("mpd: nondet stay put\n");
		true
accept:
	od
}

active proctype singleplayer() {
	do
	:: syncidle_up ? M_MPD_IDLE -> // cast
		syncidle_in     ! M_TX_BEGIN;
		syncidle_in_ack ? M_TX_BEGIN;
		// callback goes here ...
progress:
		syncidle_in     ! M_TX_END;
		syncidle_in_ack ? M_TX_END
	:: syncidle_up ? M_REQUEST_UPDATE -> // call
		syncidle_in     ! M_INTERRUPT_NO_TX;
		syncidle_in_ack ? M_INTERRUPT_NO_TX;
		syncidle_up_ack ! M_REQUEST_UPDATE
accept:
	od
}

active proctype multiplayer() {
	do
	:: true ->
		printf("multiplayer: nondet request update\n");
		syncidle_up     ! M_REQUEST_UPDATE;
		syncidle_up_ack ? M_REQUEST_UPDATE;
	:: true ->
		printf("multiplayer: nondet no update\n");
		true
accept:
	od
}
