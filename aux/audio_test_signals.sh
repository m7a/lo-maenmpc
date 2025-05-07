#!/bin/sh -eu
# Ma_Sys.ma Audio Test Signals, v.1.0.0 (c) 2025 Ma_Sys.ma <info@masysma.net>
# inspired by <https://gtkc.net/audio-testing-scripts-for-linux>

samplerate=88200
channels=2

pid=

info() {
	cat <<EOF
0 - stop
1 - 80Hz test tone
2 - low frequency sweep +5Hz/3sec 10..110Hz
3 - 100Hz test tone
q - Exit

EOF
	printf "[0/1/q]? "
}

info
while read -r input; do
	case "$input" in
	(0)
		kill -s TERM $pid
		wait $pid
		pid=
		;;
	(1)
		play -q -r "$samplerate" -n -c "$channels" synth 600 sin 80 &
		pid="$pid
$!"
		;;
	(2)
		(
			set -e
			for i in 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 \
					85 90 95 100 105 110; do
				echo "** $i **"
				play -q -r "$samplerate" -n -c "$channels" \
								synth 3 sin "$i"
			done
		) &
		pid="$pid
$!"
		;;
	(3)
		play -t pulseaudio -q -r "$samplerate" -n \
					-c "$channels" synth 600 sin 80 &
		pid="$pid
$!"
		;;
	(q)
		exit 0
		;;
	(*)
		printf "Unknown input: %s\n" "$input"
		;;
	esac
	info
done
