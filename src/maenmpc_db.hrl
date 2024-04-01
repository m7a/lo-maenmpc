-define(RATING_UNRATED, -1).
-define(RATING_ERROR,   -2).

% TODO DERIVED FROM CLI - ONCE IT IS OK, RE-ALIGN CLI TO THIS HERE!
% key          := {Artist, Album, Title}
% uris         := tuple of URIs (binary()) in MPD list order! (TODO x DEVIATES)
% assoc_status is implicit from the presence of URIs!
-record(dbsong, {key, uris, playcount, rating, duration, year, trackno, audios}).
-type dbsong() :: #dbsong{key::{binary(), binary(), binary()}, uris::tuple(),
		playcount::integer(), rating::integer(), duration::integer(),
		year::binary(), trackno::integer(), audios::binary()}.
