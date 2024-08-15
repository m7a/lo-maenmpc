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
% user_data(type=list) = {artists,beforerev,after}
-record(dbscroll, {type, cnt, coffset, csel, total, last_query_len, qoffset,
		user_data}).
-type dbscoll() :: #dbscroll{type::atom(), cnt::[dbsong()], coffset::integer(),
		csel::integer(), total::integer(), last_query_len::integer(),
		qoffset::integer(), user_data::term()}.

-record(dboutput, {player_idx, partition_name, output_id, output_name}).
% active_set {player idx, partition name, output id} when active!
% partitions list of binary or list of list of binary depending on
%            singleplayer/multiplayer view
% assigned   {player idx, partition name} of the current player state
%            It can only be known for real by the multiplayer, but singleplayer
%            can propose a value which is becoming effective if that singleplayer
%            is the active one.
-record(dboutputs, {outputs, partitions, active_set, assigned, cursor}).
