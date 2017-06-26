
# freewill.ai

FWaaS - "Free Will as a Service"!

Feeling limited to deciding to do things as an individual? Now you can decide to do things _in the cloud!_

![](https://github.com/sordina/freewill.it/blob/master/frontend/images/freewill.png?raw=true)

(Will move to freewill.ai at some point...)

## Running

There are options defined with optparse-generic that you can access by using

> stack run -- -- --help


    freewill.ai

    Usage: freewill [--port INT] [--database DATABASE] [--jwtKey STRING]
                    [--safeAuth BOOL] [--jsURL TEXT] [--logLevel LOGLEVEL]

    Available options:
      -h,--help                Show this help text
      --database DATABASE      Memory | Postgres (Default)
      --jwtKey STRING          JWT Key FilePath
      --safeAuth BOOL          False | True (Default) - Mandate HTTPS for Auth
      --jsURL TEXT             URL that Javascript points to
      --logLevel LOGLEVEL      Prod | Dev (Default) | Debug


To get up and running quickly for local development with a persisted JWK key, use

> stack run -- -- --database Memory --safeAuth False --jwtKey key.jwk

This allows for an in-memory database with no existing users, no requirement for HTTPs,
and a persisted key, for resumption of sessions between server restarts.


If you want to run using postgres, you can pass

> --database Postgres

This currently expects that you have a running, migrated database called freewill.

There is an open task to make this configurable.

Migrations live in the db/ folder.


## Testing

There is a small test-suite that will curl the application routes that lives in test/curl.bash.


## Ideas

* <https://hackage.haskell.org/package/hmemdb-0.4.0.0/docs/Data-HMemDb.html>
* <https://github.com/myfreeweb/magicbane>
* <https://github.com/valderman/selda>
* <https://hackage.haskell.org/package/monad-persist-0.0.1.2/docs/Control-Monad-Persist.html>
* <https://www.reddit.com/r/haskell/comments/68qzlr/freerpersistent_freereffects_wrapper_for/>
* <https://hackage.haskell.org/package/uuid-1.3.13/docs/Data-UUID.html>


## TODO

* Move all the orphans into special modules sepearating all the junk out
* Supe up the user, have an email, etc.
* Encrypt user password in the database






