# 2.0.0

### Improvements:

* Add `setBoundaryWith` function and `Neat.Boundary` module
* Add functions for accessibility
    * `setRole`
    * `setAria`
    * `setBoolAria`
* Add `textNode` function
* Add special Html node functions
    * `input`
    * `textarea`
    * `select`

### Breaking Changes

* Change type of `Row.optimized`
* Change type of `Column.optimized`
* Rename `text` to `textBlock`
* Remove `div` for (`lift Html.div`)
* Remove `unsafeFromHtml`
* Change `Row.defaultRow`
    * Change `.vertical` from `Row.Top` to `Row.Stretch`
* Change `Column.defaultColumn`
    * Change `.horizontal` from `Column.Left` to `Column.Stretch`
* Remove `toPage` and add `sandbox`, `element`, `document`, and `application`, instead
* Rename `IsPadding`, `NoPadding`, `fromNoPadding` to `IsGap`, `NoGap`, `fromNoGap`
