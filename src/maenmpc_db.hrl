-define(RATING_UNRATED, -1).
-define(RATING_ERROR,   -2).

% TODO DERIVED FROM CLI - ONCE IT IS OK, RE-ALIGN CLI TO THIS HERE!
% key          := {Artist, Album, Title}
% uris         := tuple of URIs       (binary()) in MPD list order! (TODO x DEVIATES)
% audios       := tuple of Audio info (binary()) in MPD list order
% assoc_status is implicit from the presence of URIs!
-record(dbsong, {key, uris, playcount, rating, duration, year, trackno, audios,
		playlist_id}).
-type dbsong() :: #dbsong{key::{binary(), binary(), binary()}, uris::tuple(),
		playcount::integer(), rating::integer(), duration::integer(),
		year::binary(), trackno::integer(), audios::binary(),
		playlist_id::integer()}.

-record(queue, {cnt, total, qoffset, doffset, dsel, last_query_len}).
-type queue() :: #queue{cnt::[dbsong()], total::integer(), qoffset::integer(),
		doffset::integer(), last_query_len::integer()}.

% results := {count1, count2}
% qartist := lists:first(cnt).name
% dsong, ssong := {<<>>,<<>>,<<>>}
% artists := list of sartist | [{artist, [list]}, {artist, [list]}]...
-record(sartist, {name, results, minsz, knownsz}).
-record(slist, {cnt, artists, dsong, ssong, last_query_len}).
