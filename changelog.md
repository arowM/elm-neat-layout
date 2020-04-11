# 2.0.0

### Improvements:

* Add `setBoundaryWith` function and `Neat.Boundary` module
* Add special map functions
    * `setBoundaryWithMap`
    * `Neat.Layout.Column.columnWithMap`
    * `Neat.Layout.Row.rowWithMap`
* Add functions for accessibility
    * `setRole`
    * `setAria`
    * `setBoolAria`
* Add `textNode` function
* Add special Html node functions
    * `input`
    * `textarea`
    * `select`
* Add `when` and `unless`
* Add `Neat.Media` module

### Breaking Changes

* Change type of `optimized`
* Change type of `Row.optimized`
* Change type of `Column.optimized`
* Rename `text` to `textBlock`
* Remove `div` for (`lift Html.div`)
* Remove `unsafeFromHtml`
* Change `Row.defaultRow`
    * Change `.vertical` from `Row.Top` to `Row.Stretch`
* Change `Column.defaultColumn`
    * Change `.horizontal` from `Column.Left` to `Column.Stretch`
* Change type of `Row.Row.wrap` and `Column.Column.wrap` from `Bool` to `Wrap`
    * `Wrap.WrapInto n` enables to break line into `n`.
* Remove `toPage` and add `sandbox`, `element`, `document`, and `application`, instead
* Rename `IsPadding`, `NoPadding`, `fromNoPadding` to `IsGap`, `NoGap`, `fromNoGap`
