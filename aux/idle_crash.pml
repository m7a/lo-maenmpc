// Ma_Sys.ma Erlang NCurses Music Player Client
// Promela Modle for [NO]IDLE verification - 2024 Ma_Sys.ma <info@masysma.net>

// https://spinroot.com/spin/Man/Quick.html
// spin -run [-i] -f idle_crash.pml
// spin -s -r -replay idle_crash.pml
// spin -run -a -i idle_crash.pml
// spin -s -r -replay idle_crash.pml
// spin -run -f -q -a -i idle_crash.pml
// spin -q -s -r -replay idle_crash.pml
// alt. spin -t -p idle_crash.pml

#define QUEUESIZE 1

mtype = {
	// syncidle states
	S_IDLE, S_TX_PRE, S_TX_PROCESSING, S_INTERRUPTED,
	// syncidle messages in
	M_MIDLE, M_INT_NTX, M_TX_BEGIN, M_TX_END,
	// syncidle messages out
	M_IDLE_ENT, M_ER_NOIDL
	// non-syncidle messages
	M_REQU
}

bool is_idle = true;

chan syncidle_down     = [QUEUESIZE] of {mtype}; // down -> maenmpc_mpd
chan syncidle_down_ack = [1]         of {mtype}; //      <- maenmpc_mpd
chan syncidle_in       = [QUEUESIZE] of {mtype}; //      <- maenmpc_singleplayer
chan syncidle_in_ack   = [1]         of {mtype};
chan syncidle_up       = [QUEUESIZE] of {mtype}; // up   -> maenmpc_singleplayer
chan syncidle_up_ack   = [1]         of {mtype};

mtype state = S_IDLE;

active proctype syncidle() {
end:
	do
	:: syncidle_in ? M_MIDLE -> // cast
		if
		:: (state == S_IDLE) || (state == S_INTERRUPTED) ->
			syncidle_up       ! M_MIDLE;
			syncidle_down     ! M_IDLE_ENT; // call down!
			syncidle_down_ack ? M_IDLE_ENT; // (await reply)
			state = S_IDLE;
		:: state == S_TX_PRE ->
			syncidle_in_ack ! M_TX_BEGIN; // delayed reply!
			state = S_TX_PROCESSING;
		:: else ->
			assert(0); // not expected to happen
		fi

	:: syncidle_in ? M_INT_NTX -> // call
		if
		:: (state == S_IDLE) ->
			syncidle_down   ! M_ER_NOIDL;
			syncidle_in_ack ! M_INT_NTX;
			state = S_INTERRUPTED;
		:: (state == S_INTERRUPTED) ->
			syncidle_in_ack ! M_INT_NTX;
		:: else ->
			assert(0); // not expected to happen
		fi

	:: syncidle_in ? M_TX_BEGIN -> // call
		if
		:: (state == S_IDLE) ->
			syncidle_down ! M_ER_NOIDL;
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
			syncidle_down ! M_IDLE_ENT
			state = S_IDLE;
		:: else ->
			assert(0); // not expected to happen
		fi
	od
}

active proctype mpd() {
end:
	do
	// syncidle_down ? M_MIDLE does not exist in the simulation in
	// reality M_IDLE_ENT would self-message to trigger this
	:: syncidle_down ? M_IDLE_ENT ->
		is_idle = true;
		syncidle_down_ack ! M_IDLE_ENT
	:: syncidle_down ? M_ER_NOIDL ->
		if
		:: is_idle ->
			syncidle_in ! M_MIDLE;
			is_idle = false;
		:: else ->
			// if already idle, the real MPD would return some sort
			// of “error” but in erlang implementation we drop such
			// errors hence it is as if we were to drop the noidle
			// call altogether...
			true
		fi
	// For now (`else`) we assert that this only happens while no incoming
	// messages are ready to process... Use `true` rather than `else` to
	// always lalow this...
	:: else ->
		if
		// can nondeterministically also decide to leave idle state.
		// this is intended to provide the emulation mpd_server_emul
		// completely?
		:: is_idle ->
			printf("mpd: nondet leave idle\n");
			is_idle = false;
			syncidle_in ! M_MIDLE
		:: true ->
			printf("mpd: nondet stay put\n");
		fi
	od
}

active proctype singleplayer() {
end:
	do
	:: syncidle_up ? M_MIDLE -> // cast
		syncidle_in     ! M_TX_BEGIN;
		syncidle_in_ack ? M_TX_BEGIN;
		printf("singleplayer: working on tx\n");
		syncidle_in     ! M_TX_END;
		syncidle_in_ack ? M_TX_END
	:: syncidle_up ? M_REQU -> // call
		syncidle_in     ! M_INT_NTX;
		syncidle_in_ack ? M_INT_NTX;
		syncidle_up_ack ! M_REQU
	od
}

active proctype multiplayer() {
end:
	do
	:: true ->
		printf("multiplayer: nondet request update\n");
		syncidle_up     ! M_REQU;
		syncidle_up_ack ? M_REQU;
	:: true ->
		printf("multiplayer: nondet no update\n");
	od
}

// [] eq always, <> eq eventually
// counterexample goes like “request update all of the time”
// (not very interesting)
//ltl back_to_idle {
//	always ((state != S_IDLE) -> (eventually (state == S_IDLE)))
//}
