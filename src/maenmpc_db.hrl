-define(RATING_UNRATED, -1).
-define(RATING_ERROR,   -2).

% key          := {Artist, Album, Title}
% uris         := tuple of URIs       (binary()) in MPD list order
% audios       := tuple of Audio info (binary()) in MPD list order
% assoc_status is implicit from the presence of URIs!
-record(dbsong, {key, uris, playcount, rating, duration, year, trackno, audios,
		playlist_id}).
-type dbsong() :: #dbsong{key::{binary(), binary(), binary()}, uris::tuple(),
		playcount::integer(), rating::integer(), duration::integer(),
		year::binary(), trackno::integer(), audios::binary(),
		playlist_id::integer()}.

% New generic scroll record
% -- general --
% type := queue | list
% -- ui view --
% user_data when sent to UI = {current_song_playlist_id, absolute_offset}
% coffset guaranteed to be 0 for UI usage
% -- db view --
% qoffset=0 means at upper boundary, len(cnt)==total means entire DB covered
% user_data(type=queue) = {artists,beforerev,after}
-record(dbscroll, {type, cnt, coffset, csel, total, last_query_len, qoffset,
		user_data}).
-type dbscoll() :: #dbscroll{type::atom(), cnt::[dbsong()], coffset::integer(),
		csel::integer(), total::integer(), last_query_len::integer(),
		qoffset::integer(), user_data::term()}.
