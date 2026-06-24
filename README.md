# servant-ede

## Differences from Upstream

Unlike the upstream `servant-ede` package, this fork makes some big changes.
Namely, template names are given by a new

```haskell
class HasTemplate contentType a where
  templateFor :: Proxy contentType -> Proxy a -> FilePath
```

rather than attached directly to the `HTML` and `Tpl` types. This means the
actual templating is *not* a visible part of the API, which makes sense when you
consider the API from the client side (who presumably doesn't have access to the
template files).

The other change is technical; we use
[`withDict`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/GHC-Base.html#t:WithDict)
to magic up the compiled template witnesses, rather than mucking around with
unsafe IO.
