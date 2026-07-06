# 1.0.0.0

## Additions

- Added `serveWithContextAndTemplates`
- Added `unsafeLoadTemplates`

## Breaking Changes

- Templates are no longer specified in the API combinators, but rather via
  `HasTemplate` instances
- Rendered templates are now passed around as an opaque `LoadedTemplates`
  instance


# 0.6

## Additions

- Support for filters

