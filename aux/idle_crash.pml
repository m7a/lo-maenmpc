// Ma_Sys.ma Erlang NCurses Music Player Client
// Promela Modle for [NO]IDLE verification - 2024 Ma_Sys.ma <info@masysma.net>

// spin -run -DNFAIR=3 -f -i -a idle_crash.pml
// spin -s -r -replay idle_crash.pml

#define QUEUESIZE    3
#define WIGGLE_SPACE 3

mtype = {
	// syncidle states
	S_IDLE, S_TX_PRE, S_TX_PROCESSING, S_INTERRUPTED,
	// syncidle messages in
	M_MIDLE, M_INT_NTX, M_TX_BEGIN, M_TX_END,
	// syncidle messages out
	M_IDLE_ENT, M_ER_NOIDL,
	// non-syncidle messages
	M_REQU
}

chan syncidle_down     = [QUEUESIZE] of {mtype}; // down -> maenmpc_mpd
chan syncidle_down_ack = [1]         of {mtype}; //      <- maenmpc_mpd
chan syncidle_in       = [QUEUESIZE] of {mtype}; //      <- maenmpc_singleplayer
chan syncidle_in_ack   = [1]         of {mtype};
chan syncidle_up       = [QUEUESIZE] of {mtype}; // up   -> maenmpc_singleplayer
chan syncidle_up_ack   = [1]         of {mtype};
chan event_in          = [1]         of {mtype};

mtype state = S_IDLE;
bool is_idle = true;

byte wiggle_tx     = 0;
byte wiggle_update = 0;
byte const_tx;
byte const_update;

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

	:: syncidle_in ? M_TX_END -> // call
		if
		:: (state == S_TX_PROCESSING) ->
			syncidle_down     ! M_IDLE_ENT;
			syncidle_down_ack ? M_IDLE_ENT;
			state = S_IDLE;
			syncidle_in_ack ! M_TX_END
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
		:: atomic { is_idle ->
			syncidle_in ! M_MIDLE;
			is_idle = false;
		}
		:: else ->
			// if already idle, the real MPD would return some sort
			// of “error” but in erlang implementation we drop such
			// errors hence it is as if we were to drop the noidle
			// call altogether...
			true
		fi
	:: atomic { event_in ? M_MIDLE ->
		// see above...
		if
		:: is_idle ->
			is_idle = false;
			syncidle_in ! M_MIDLE
		:: else ->
			true
		fi
	}
	od
}

active proctype singleplayer() {
end:
	do
	:: syncidle_up ? M_MIDLE -> // cast
		syncidle_in     ! M_TX_BEGIN;
		syncidle_in_ack ? M_TX_BEGIN;
		printf("singleplayer: tx\n");
		wiggle_tx = (wiggle_tx + 1) % WIGGLE_SPACE;
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
	:: event_in ? M_REQU ->
		syncidle_up     ! M_REQU;
		syncidle_up_ack ? M_REQU;
		wiggle_update = (wiggle_update + 1) % WIGGLE_SPACE;
		true
	od
}

// Special event generation with a limited number of initial tries to spit
// out messages very quickly as to check whether this could cause any issues
// After a few initial messages, slow down and only when everyone else has
// suspended consider generating another event! Simulates low priority in a
// channel-compatible way.
active proctype eventgen() {
	byte i = 0;
	do
	:: i > 2 ->
		break
	:: else ->
		if
		:: true ->
			printf("ev M_REQU\n");
			event_in ! M_REQU;
			i++

		:: atomic { is_idle && empty(event_in) } ->
			printf("ev M_MIDLE\n");
			event_in ! M_MIDLE;
			i++
		fi
	od
end:
	do
	:: timeout ->
progress:
		if
		:: true ->
			printf("ev M_REQU\n");
			event_in ! M_REQU
		:: atomic { is_idle && empty(event_in) } ->
			printf("ev M_MIDLE\n");
			event_in ! M_MIDLE
		fi
	od
}

init {
	select(const_tx:     0 .. WIGGLE_SPACE-1);
	select(const_update: 0 .. WIGGLE_SPACE-1);
	true
}

// G/[]/always, F/<>/eventually

//ltl back_to_idle {
//	always ((state != S_IDLE) -> (eventually (state == S_IDLE)))
//}

ltl cannot_end_up_const {
	! (eventually (always  (wiggle_tx     == const_tx &&
				wiggle_update == const_update)))
}
