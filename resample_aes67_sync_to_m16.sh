#!/bin/sh -eu
# Script to Re-Sample Audio 1.1.0, (c) 2024 Ma_Sys.ma <info@masysma.net>

# NOTE The script cannot currently detect modifications for album/single
#      entries, only for files. For the other data types, the respective
#      directory needs to be deleted from target prior to running

root_src=/data/programs/music2
root_target=/var/lib/mpd/music
ssh=mpd@192.168.1.22

parallelism=8

if [ $# = 0 ]; then
	rsl="$(printf "%s" "$root_src"    | wc -c)"
	rtl="$(printf "%s" "$root_target" | wc -c)"

	# -- Directory-based part --
	for dir in album single; do
		locald="$(find "$root_src/$dir" \
				-mindepth 1 -type d | cut -c $((rsl +2))-)"
		remoted="$(ssh "$ssh" find "$root_target/$dir" \
				-mindepth 1 -type d | cut -c $((rtl + 2))-)"
		patloc="^($(printf "%s" "$locald" | sed 's#/#\\/#g' | \
								tr '\n' '|'))$"
		patrem="^($(printf "%s" "$remoted" | sed 's#/#\\/#g' | \
								tr '\n' '|'))$"
		printf "%s\n" "$locald" | grep -vE "$patrem" | \
				parallel -P "$parallelism" "$0" "$dir" {}
		printf "%s" "$remoted" | grep -vE "$patloc" | \
						while read -r line; do
			echo "[RMRSQ] $line" >&2
			# shellcheck disable=SC2029
			ssh "$ssh" "rm -r \"$root_target/$dir/$line\""
		done
	done

	# -- File-based part --
	for dir in track epic; do
		localf="$(find "$root_src/$dir" -type f \( -name '*.flac' -or \
			-name '*.mp3' \) -exec stat --printf "%n:%Y\n" {} + | \
			cut -c $((rsl + 2))- | sed 's/\.mp3:/.flac:/g')"
		remotef="$(ssh "$ssh" find "$root_target/$dir" -type f \
			-name '*.flac' -exec stat --printf "\"%n:%Y\\n\"" {} + \
			| cut -c $((rtl + 2))-)"
		{
			dnl=
			for i in $localf; do
				fn="$(printf "%s" "$i" | cut -d: -f 1)"
				other="$(printf "%s\n" "$remotef" | \
							grep -F "$fn:" || true)"
				proc=0
				if [ -n "$other" ]; then
					remotef="$(printf "%s\n" "$remotef" | \
							grep -vF "$fn:")"
					mydate="$(printf "%s" "$i" | \
								cut -d: -f 2)"
					otherdate="$(printf "%s" "$other" | \
								cut -d: -f 2)"
					if [ "$mydate" -gt "$otherdate" ]; then
						proc=1
					fi
				else
					proc=1
				fi
				if [ "$proc" = 1 ]; then
					dn="$(dirname "$fn")"
					if ! { printf "%s\n" "$dnl" | \
							grep -qF "$dn"; }; then
						echo \
						"[MKDIR] $root_target/$dn" >&2
						ssh $ssh \
						"mkdir -p \"$root_target/$dn\""
						dnl="$dnl
$dn"
					fi
					echo "$fn"
				fi
			done
			for j in $(printf "%s\n" "$remotef" | cut -d: -f 1); do
				dst="$root_target/$j"
				echo "[DELET] $dst" >&2
				# shellcheck disable=SC2029
				ssh "$ssh" "rm \"$dst\""
			done
		} | parallel -P "$parallelism" "$0" filegain {}
	done
	exit 0
fi

case "$1" in
(--help)
	echo "USAGE $0 - Consult source code prior to use!"
	;;
(album|single)
	src="$root_src/$2"
	dst="$root_target/$2"
	tmpd="/tmp/resample3_$$"
	tmpf="$tmpd/tmp.flac"
	mkdir "$tmpd"
	echo "[ IN  ] $src"
	find "$src" -maxdepth 1 -type f -name '*.flac' | while read -r line; do
		if file "$line" | grep -qF "24 bit, stereo, 96 kHz"; then
			cp "$line" "$tmpd"
		else
			ReSampler -i "$line" -o "$tmpf" \
						-r 96000 -b 24 > /dev/null
			# https://stackoverflow.com/questions/40699771/batch-
			# -> restore album art!
			ffmpeg -nostdin -loglevel error -y -i "$line" \
				-i "$tmpf" -map 1 -codec copy -map 0:1? \
				-map_metadata 0 "$tmpd/$(basename "$line")"
			rm "$tmpf"
		fi
	done
	echo "[  L  ] $dst"
	loudgain -q -a -k -s e "$tmpd"/*.flac > /dev/null
	echo "[  CP ] $dst"
	ssh "$ssh" mkdir "$dst"
	scp "$tmpd"/*.flac "$ssh:$dst"
	rm -r "$tmpd"
	;;
(filegain)
	path="$2"
	src="$root_src/$path"
	dst="$root_target/$path"
	tmpf1="/tmp/resample1_$$.flac"
	tmpf2="/tmp/resample2_$$.flac"
	tmpf3="/tmp/resample3_$$.flac"
	ismp3=0
	if [ -f "$src" ]; then
		if file "$src" | grep -qF "24 bit, stereo, 96 kHz"; then
			# no need to convert
			echo "[ CPF ] $dst"
			exec scp "$src" "$ssh:$dst"
		fi
	else
		src="${src%.flac}.mp3"
		ismp3=1
	fi
	if [ -f "$src" ]; then
		rm "$tmpf1" "$tmpf2" 2> /dev/null || true
		if [ "$ismp3" = 1 ]; then
			echo "[ FF  ] $src"
			ffmpeg -loglevel error -i "$src" "$tmpf1"
			src="$tmpf1"
		fi
		echo "[ RES ] $src"
		# remove redirect to display more logging
		ReSampler -i "$src" -o "$tmpf3" -r 96000 -b 24 > /dev/null
		# copy metadata
		ffmpeg -nostdin -loglevel error -y -i "$src" -i "$tmpf3" \
			-map 1 -codec copy -map 0:1? -map_metadata 0 "$tmpf2"
		# file-based replay gain
		loudgain -q -r -k -s e "$tmpf2" > /dev/null
		echo "[  CP ] $dst"
		scp "$tmpf2" "$ssh:$dst"
	else
		echo "[ERROR] DELETE FROM TARGET ??? $dst"
	fi
	rm "$tmpf1" "$tmpf2" "$tmpf3" 2> /dev/null || true
	;;
esac
