
# freewill.ai

FWaaS - "Free Will as a Service"!

Feeling limited to deciding to do things as an individual? Now you can decide to do things _in the cloud!_

![](https://github.com/sordina/freewill.it/blob/master/frontend/images/freewill.png?raw=true)

(Will move to freewill.ai at some point...)

## Ideas

* <https://hackage.haskell.org/package/hmemdb-0.4.0.0/docs/Data-HMemDb.html>
* <https://github.com/myfreeweb/magicbane>
* <https://github.com/valderman/selda>
* <https://hackage.haskell.org/package/monad-persist-0.0.1.2/docs/Control-Monad-Persist.html>
* <https://www.reddit.com/r/haskell/comments/68qzlr/freerpersistent_freereffects_wrapper_for/>
* <https://hackage.haskell.org/package/servant-docs-0.10/docs/Servant-Docs.html>

## Thoughts

Is there ever a time when I'll need another natural-transformation besides runReaderTNat?

Probably not, as we'll always want to fetch a value, then do something interesting with it,
and the interesting effects can be captured on the constant-side of the Reader.

For example:

    type MemDBHandler    = ReaderT (T.TVar AppState)    Handler
    type PostgresHandler = ReaderT (PostgresConnection) Handler

etc...
